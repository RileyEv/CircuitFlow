%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Benchmarks}\label{chap:benchmarks}
To perform benchmarks in this Chapter, the data pre-processing pipeline that aggregated audio history will be used from Section~\ref{example-pre-proc-pipeline}.
Each benchmark made will be tested on 5 different numbers of inputs: 1, 10, 100, 1000, 2000.
Three months of my own audio history will be used, to ensure that the data closely aligns with the real world.
This allows for the evaluation of how each implementation scales with more inputs.
All benchmarks will take place on a dual-core Intel i5-6276U CPU with hyper-threading, giving 4 virtual cores.

\section{Benchmarking Technicalities}
When benchmarking Haskell, there are several important features that should be considered to ensure correct and meaningful timings are obtained.

\paragraph{Lazy Evaluation}
Haskell makes use of lazy evaluation, in many cases this is a very useful feature.
However, when timing code, care needs to be taken to ensure that values are actually evaluated, within the timed block.
Consider this example of timed code:

\begin{code}
newtype Value = Value Int deriving Show
add :: Int -> Int -> IO (Value)
add x y = return (Value (x + y))

main :: IO ()
main = do
  startTime  <- getTime clock
  r          <- add 1 2
  endTime    <- getTime clock
  print (diffTimeSpec endTime startTime)
  print r
\end{code}

To an untrained eye, this would appear to time how long it takes to add the two numbers together, but this isn't the case.
The laziness of Haskell means that, the evaluation of |r| will not take place until |print| needs it.

Luckily, Haskell provides infrastructure to avoid these problems.
|deepseq :: a -> b -> b| forces the full evaluation of |a|, before it returns |b|: making use of this function the evaluation of |r| is forced in the timed code.
The |BangPatterns| language extension is used to force evaluation to weak head normal form.

%format r'

\begin{code}
main :: IO ()
main = do
  startTime  <- getTime clock
  r          <- add 1 2
  let !r' = r `deepseq` r
  endTime    <- getTime clock
  print (diffTimeSpec endTime startTime)
  print r'
\end{code}

This examples firstly uses |deepseq| to force the full evaluation of the result, however, Haskell will lazily evaluate this application of |deepseq|.
A bang pattern is used to force the evaluation the application of |deepseq|.


\paragraph{Multi-Core Haskell}
By default the Haskell runtime does not enable multi-core processing.
Considering the aim of this project partly involves making |CircuitFlow| run in parallel, multi-core processing is crucial.
To enable this the \texttt{-threaded} flag is set when building the binary.
Then, using the runtime options, the number of threads can be set by adding \texttt{+RTS -N} flags when running the binary.
The \texttt{-N} allows the runtime to select the optimal number of threads for the program.


\section{Parallel vs Serial}
The first test will ensure that CircuitFlow's parallelisation has a positive effect on run-times.
To ensure that the test is fair, the serial implementation will make use of the same tasks in the pre-processing pipeline.
The inputs and outputs will just be manually fed into each task, in a sequential way.
The results from this test are found in figure~\ref{fig:parallel-speedup}


\begin{figure}[ht]
  \centering
  \input{graphs/parallel_speedup}
  \caption{Speedup provided by using CircuitFlow, relative to a serial implementation}
  \label{fig:parallel-speedup}
\end{figure}

This shows that CircuitFlow does indeed provide a performance gain, with on average speedup of 1.3x.
However, with 4 threads available a higher speedup may have been expected, as there is now 4x as much processing power.

\subsection{Areas for improvement}
Profiling the circuit shows that a significant proportion of time is spend reading CSV files.
Improving the speed of parsing CSV files is outside the scope of this project, however there could be ways to optimise how often a CSV is read.
For example, replicating a data store leads to the same value being read from the data store twice, the use of caching could allow for the value to only be read once.
In the pipeline example, this would mean that each input CSV is only read once.

Another area for improvement is that there is an expectation on the user to know where is best to split up the workflow into tasks.
With a user who is familiar with the domain this should not be too hard,
but it would also be beneficial if a circuit could automatically fuse tasks together,
when it would have a positive effect on the runtime.


\subsection{An Aside: 1 Core Circuit vs Serial}
Another interesting scenario to test is checking if the network structure adds additional overhead, in a situation where there is only 1 core.
To test this, the multi-core support of the Haskell runtime will not be enabled: this will then simulate multiple cores with context switching.
Figure~\ref{fig:linear-linear-c}, shows the results of this benchmark.

\begin{figure}[ht]
  \centering
  \input{graphs/linear_vs_linear_c}
  \caption{A comparison of runtimes of a serial implementation vs single core CircuitFlow}
  \label{fig:linear-linear-c}
\end{figure}

This shows that both the linear and single core implementation scale together in a linear fashion.
Most importantly, neither implementation is faster than the other, therefore, it is concluded that |CircuitFlow| does not add additional overheads.
This will be particularly helpful for a user, that needs to run code on multiple types of devices.
There is no need for them to create a different implementation for devices where parallelisation may not be possible.


\section{CircuitFlow vs Luigi}
The final benchmark on CircuitFlow is comparing it to widely used library: Luigi by Spotify~\cite{luigi}.
Since Luigi uses a \ac{DPN}, it can use any number of threads: in this test it is set to 4 --- the same as CircuitFlow.
Figure~\ref{fig:luigi-parallel-c}, shows the results of the benchmark.

\begin{figure}[ht]
  \centering
  \input{graphs/luigi_vs_parallel_c}
  \caption{A comparison of runtimes of CircuitFlow vs Luigi}
  \label{fig:luigi-parallel-c}
\end{figure}

This shows clearly that CircuitFlow out performs Luigi on larger numbers of inputs.
CircuitFlow scales linearly with the number of inputs, whereas Luigi's runtime appears to grow at a quicker rate than linear.
CircuitFlow always out performs Luigi on any number of inputs, this can lead to the conclusion that CircuitFlow is able to out perform another dataflow \ac{DSL} --- exceeding the requirements.

\subsection{Why is CircuitFlow so good?}
There are differences between CircuitFlow and Luigi which could explain these significant differences in runtime --- especially for larger numbers of inputs.

\paragraph{Computation Models}
CircuitFlow uses a \acf{KPN} to compute the results of the system: this makes use of threads and channels, with the computation on each thread set statically at compile-time.
Luigi uses a variant of a \ac{KPN}, known as \acf{DPN}: making use of a central scheduler and threads that can perform any task.
One possible reason for slowdown in Luigi is this central scheduler: this scheduler checks tasks dependencies at runtime, therefore it will need to resolve the dependencies in tasks to work out which ones to execute.
This is the opposite to CircuitFlow, where dependencies between tasks are resolved at compile time.

\paragraph{Use of Threads}
CircuitFlow aims to be as lightweight on threads as possible, it only starts 1 thread for each task in a circuit: this will reduce the overhead of starting new threads.
Luigi, does not make use of threads, instead it creates new processes, however it does so in an inefficient way: it creates a new process for each invocation of a task, which is destroyed after.
This means that in the example benchmarked with 2000 inputs, it creates 8000 new processes: this adds significant overhead that CircuitFlow does not have.

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
