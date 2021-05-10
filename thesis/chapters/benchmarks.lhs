%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Benchmarks}\label{chap:benchmarks}
To perform benchmarks in this Chapter, the data pre-processing pipeline will be used from Section~\ref{example-pre-proc-pipeline}.
The |CircuitFlow| implementation will be benchmarked against a serial implementation and one in a similar library Luigi.

\section{Benchmarking Technicalities}
\paragraph{Lazy Evaluation}
Ensure computation does not escape the timed section.

\paragraph{Multi-Code Haskell}


\section{Parallel vs Serial}
The first test will ensure that using multiple threads has a positive affect on run-times.

\paragraph{An Aside: 1 Core Circuit vs Serial}


\section{CircuitFlow vs Luigi}


\subsection{Why is CircuitFlow so good?}
well it revolves around luigi=bad.

luigi = \ac{DPN}, so needs a scheduler. but this does not scale well to high numbers of tasks.
New process is created for each firing of a task. CircuitFlow only creates a \textit{thread} for each task --- much more lightweight.


\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
