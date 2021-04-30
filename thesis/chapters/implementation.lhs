\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\chapter{Implementation}\label{chap:intro}

\section{Requirements}

\begin{itemize}
  \item Typesafe
  \item Parallel
  \item Competitive Speed
  \item Failure Tolerance
\end{itemize}

\section{Circuit AST}
How have i modified a la carte to work with ifunctors.
then give a few examples and maybe one of the smart constructors that injects the L's and R's



\section{Network}
\subsection{Network Typeclass}
\paragraph{Interaction with Network}


\section{Translation}
\subsection{Steps of translation}
\paragraph{icataM}
\subsection{UUIDS}
\paragraph{Why are they needed?}
\paragraph{How are they added?}

\section{Failure in the Process Network}
\paragraph{Why?}
\subsection{Maybe Monad}
\paragraph{No error messages}
\subsection{Except Monad}
based on |Either|
\paragraph{Propagation}





% ------
% not really as important
% \section{Scheduler \& Typed Graph}
% \subsection{Problems representing a typed graph as a AST}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
