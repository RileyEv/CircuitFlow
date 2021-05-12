%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Conclusion}\label{chap:conclusion}

\section{Related Work}
\todo[inline]{Do this...}

\paragraph{Composing Effects into Tasks and Workflows~\cite{10.1145/3406088.3409023}}

 - general about:
 * domain = data science workflow, so more specific than ours
 * contributions
 - new complication process
 - new method for composing arrow effects / handlers (main contribution)
 - practical examples (examples can be contributions!)
 - on writing
 * story is essential and what people buy into. Their story is:
 - overview data science workflows
 - draw similarities to lastest work in Haskell
 => implementation in haskell that is natural, but oddly not done before so novel
 * time spend on domain cos it is not expected that people will know about workflow / ML so we will do similar

 \paragraph{Scoping Monadic Relational Database Queries~\cite{10.1145/3331545.3342598}}

 (read to see writing style of a Haskell Symp paper for a DSL)
 - story goes from domain to need for DSL
 - they lean on monadic interface
 - emphasis on importance on domain, theirs is data bases that are used all the time
 - lists advantages of eDSL
 * output correct by construction (they dont generate wrong SQL statements)
 * type checked by host - imagine making your DSL from scratch and the type system you would have needed
 * higher level of abstraction
 - also uses advanced haskell stuff like Hlists and type heft if you wanna look to the writing of those intros
 - clear simple and small example of the problem they are solving

\section{Future Work}

\paragraph{Performance Improvements}
Whilst CircuitFlow performs well against the competition, there have been some possible improvements highlighted that could help to improve the runtime further.
One optimisations is to fuse tasks together, when the fusing would have a greater increase than the parallelisation from being left un-fused.
This would require analysis of the computation inside a task, to rate the relative computational intensity of each task.
Unfortunately, it is not possible to inspect the AST of another Haskell, so another approach must be taken.
This could involve using defunctionalisation and building a new DSL that can construct tasks.
This would generate an \ac{AST}, that can be traversed to measure its computational intensity.
However, this could restrict the computation a task is capable of doing.
Another approach could be, introducing a new |Network| instance that is capable of profiling a network with a few example inputs.
The information can then be used to perform optimisations to the original network.

\paragraph{Network Visualiser}
A network could be constructed of long running tasks, for a user they might like to see the state of the network is a quick and simple way.
A visualiser could be build that shows all the tasks currently being executed and the number of pending values on all the channels in the system.
This could help a user find bottlenecks in the system and where backlogs of pending values are occurring.

\paragraph{More Networks}
The network system has been designed in a way that allows for multiple instances, one future task could be introducing more variants.
One example has already been discussed --- a profiling network.
Another example could be a distributed network, this will become extremely helpful when data workflows become too big for a single machine.
To do this it is likely that data stores would need to be serialisable, so that it can be transferred between different nodes in the network.

\paragraph{Further Benchmarks}
Currently the only benchmark is a pre processing pipeline for song data aggregation, however, there are many other uses for CircuitFlow.
More benchmarks could be defined across a wider array of uses, this will allow for further comparison to other similar systems.

\paragraph{More Circuit Smart Constructors}
CircuitFlow focuses on the transformations made to data as it passes through the system, however, there are many transformations a developer may find themselves repeating.
For example, what if a user wanted to replicate an input to produce three outputs instead of two: they'd find themselves repeating |replicate <-> id <> replicate| multiple times.
What if they wanted 4 or more outputs? Smart constructors can be used to combat this problem, with new ones being defined for common patterns that occur.
For this specific example a new smart constructor |replicateN :: SNat n -> Circuit (Q([f])) (Q([a])) (Q([f a])) (Repeat n f) (Repeat n a) (Repeat n (f a)) N1|, where |Repeat| is a type family that produces a list of the argument repeated |n| times.


\section{What do i call this ? (actual conclusion)}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
