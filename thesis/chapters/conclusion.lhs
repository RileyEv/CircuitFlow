%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Conclusion}\label{chap:conclusion}

\section{Related Work}

\subsection{Existing Workflow Tools}
\paragraph{Composing Effects into Tasks and Workflows~\cite{10.1145/3406088.3409023}}
Another approach taken that solves the un-typed task composition problem is the Funflow library based on the techniques in the paper \textit{Composing Effects into Tasks and Workflows}.
This work notices that tasks in a workflow are similar to effects in the functional community.
It draws from existing work on combining and analysing effects, with categories and arrows, and applies this to constructing workflows.

\paragraph{SciPipe: A workflow library for agile development of complex and dynamic bioinformatics pipelines~\cite{10.1093/gigascience/giz044}}
Whilst CircuitFlow and many other libraries place emphasis on defining tasks as functions within the embedding language, SciPipe takes a different approach.
It instead focuses on the orchestration of external jobs that are called with Bash commands, which allows for simple integration with pre-existing binaries.
It also allows for the library to be able to orchestrate tasks written in the best language for its requirements.

\paragraph{Luigi}
Luigi is used to orchestrate tasks in a data workflow, it is a library that falls into the trap of un-typed task dependencies.
It makes use of a central scheduler and workers, allowing work to be distributed across multiple machines.
It also comes with built in support for many different output formats, such as files in a Hadoop file system.


\paragraph{Pipes~\cite{pipes}}
The CircuitFlow examples described in this thesis focus on processing a group of jobs and waiting until the are complete before moving on, another approach is to support streaming of data.
This is the route that the Haskell library pipes takes, it allows the user to build streaming pipelines, that can be composed together.
This approach could also be adopted by CircuitFlow without any modifications, allowing for inputs and outputs to be streamed in, with the channels acting as buffers on the stream.


\subsection{Indexed Functors}
\paragraph{Staged Selective Parser Combinators~\cite{parsley}}
Indexed functors~\cite{mcbride2011functional}, are a new technique for building typed \acp{DSL}.
This paper makes use of this new tool to have a type index representing the type of a parser.
This allows it to make optimisations and translations while ensuring that the value parsed never changes.

% Not sure where it fits into the story?

% \paragraph{Scoping Monadic Relational Database Queries~\cite{10.1145/3331545.3342598}}
% (read to see writing style of a Haskell Symp paper for a DSL)
% - story goes from domain to need for DSL
% - they lean on monadic interface
% - emphasis on importance on domain, theirs is data bases that are used all the time
% - lists advantages of eDSL
% * output correct by construction (they dont generate wrong SQL statements)
% * type checked by host - imagine making your DSL from scratch and the type system you would have needed
% * higher level of abstraction
% - also uses advanced haskell stuff like Hlists and type heft if you wanna look to the writing of those intros
% - clear simple and small example of the problem they are solving

\section{Future Work}

\paragraph{Performance Improvements}
Whilst CircuitFlow beats the competition, there are some possible improvements that could help to improve the runtime even further.
One optimisations is to fuse tasks together, when the fusing would have a greater increase than the parallelisation from being left un-fused.
This would require analysis of the computation inside a task, to rate the relative computational intensity of each task.
Unfortunately, it is not possible to inspect the AST of another Haskell function, so another approach must be taken.
This could involve using defunctionalisation and building a new DSL that can construct tasks.
This would generate an \ac{AST}, that can be traversed to measure its computational intensity.
However, this could restrict the computation a task is capable of doing.
Another approach could be, introducing a new |Network| instance that is capable of profiling a network with a few example inputs.
The information can then be used to perform optimisations to the original network.

\paragraph{Network Visualiser}
A network could be constructed of long running tasks, making it desirable to see progress of the network in a quick and simple way.
A visualiser could be build that shows all the tasks currently being executed and the number of pending values on all the channels in the system.
This could help a user find bottlenecks in the system and where backlogs of pending values are occurring.

\paragraph{More Networks}
The network system has been designed in a way that allows for multiple instances.
One future task could be introducing more variants.
One example has already been discussed --- a profiling network.
Another example could be a distributed network, which would become extremely helpful when data workflows become too big for a single machine.
To do this it is likely that data stores would need to be serialisable, so that it can be transferred between different nodes in the network.

\paragraph{Further Benchmarks}
Currently the only benchmark is a pre-processing pipeline for song data aggregation, however, there are many other uses for CircuitFlow.
More benchmarks could be defined across a wider array of uses, this will allow for further comparison to other similar systems.
However, for the scope of this project it was enough to beat a leading tool by Spotify at its own game --- aggregating song data.

\paragraph{More Circuit Smart Constructors}
CircuitFlow focuses on the transformations made to data as it passes through the system, however, there are many transformations a developer may find themselves repeating.
For example, what if a user wanted to replicate an input to produce three outputs instead of two: they'd find themselves repeating |replicate <-> id <> replicate| multiple times.
What if they wanted 4 or more outputs? Smart constructors can be used to combat this problem, with new ones being defined for common patterns that occur.
For this specific example a new smart constructor |replicateN :: SNat n -> Circuit (Q([f])) (Q([a])) (Q([f a])) (Repeat n f) (Repeat n a) (Repeat n (f a)) N1|, where |Repeat| is a type family that produces a list of the argument repeated |n| times.

\paragraph{Better Support for External Tasks}
The lhs2TeX build system made use of external tasks, however, it did so at a very raw level.
More smart constructors for tasks can be introduced that allows for easier definition of external tasks.
I could also add a specific methods for how inputs and outputs are handled with those external tasks, possibly through stdin and stdout, or as file.

\paragraph{Visual Construction}
All the constructors for CircuitFlow have graphical meanings, so why not have a graphical way to compose them?
A new GUI could be introduced that allows users to drag and drop building blocks to create circuits.
This would enable the library to be used by developers who may have less experience or prefer to view visual representations instead.



\section{Conclusion}




\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
