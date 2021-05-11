%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Conclusion}\label{chap:conclusion}

\section{Related Work}

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
Whilst CircuitFlow performs well against the competition, there have been some possible improvements highlighted that could help to improve the runtime further.



more benchmarks

Visualiser, show state of network
Different versions of network: has had groundwork setup already! distributed.
Ability to profile a network/perform optimisations on the composition of tasks.


\section{What do i call this ? }

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
