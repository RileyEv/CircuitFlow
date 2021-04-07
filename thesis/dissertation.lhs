\documentclass[
author={Riley Evans},
supervisor={Dr. Meng Wang},
degree={MEng},
title={\vbox{TBD}},
subtitle={},
type={research},
year={2021}
]{dissertation}

% lhs2tex setup

%include polycode.fmt
%include forall.fmt
%include spacing.fmt




\begin{document}

\maketitle

% =============================================================================

\frontmatter
\makedecl{}
\tableofcontents

% -----------------------------------------------------------------------------

\chapter*{Supporting Technologies}

% -----------------------------------------------------------------------------

\chapter*{Notation and Acronyms}

maybe?

% -----------------------------------------------------------------------------

\chapter*{Acknowledgements}

\noindent
It is common practice (although totally optional) to acknowledge any
third-party advice, contribution or influence you have found useful
during your work.  Examples include support from friends or family, 
the input of your Supervisor and/or Advisor, external organisations 
or persons who  have supplied resources of some kind (e.g., funding, 
advice or time), and so on.

% =============================================================================

\mainmatter{}


\chapter{Introduction}\label{chap:intro}

% -----------------------------------------------------------------------------

\chapter{Background}\label{chap:background}

\section{Dataflow Programming}
\begin{itemize}
  \item What is is?
  \item Why is it useful?
  \item What benefits does it have?
  \item Examples
\end{itemize}

\section{Domain Specific Languages}
General introduction of what they are and a few examples.

Discuss embedded DSLs.

\subsection{Shallow Embeddings}


\subsection{Deep Embeddings}



\begin{itemize}
  \item What are they? Why do they differ to GPLs
  \item Deep vs Shallow
\end{itemize}

\section{Higher Order Functors}
\begin{itemize}
  \item IFunctors, imap, natural transformation
  \item Maybe drop some cat theory diagrams
  \item IFix
  \item Their use for DSL development, icata, small example.
  \item Data types a la carte
\end{itemize}

\section{Type Families}
\begin{itemize}
  \item What are they?
  \item DataKinds
  \item Examples
\end{itemize}

\section{Dependently Typed Programming}
\begin{itemize}
  \item What is is?
  \item Singletons, why they needed, examples, using with typefamilies.
\end{itemize}


\section{Kahn Process Networks}
\begin{itemize}
  \item What is is?
  \item Diagrams
\end{itemize}


% -----------------------------------------------------------------------------

\chapter{Project Execution}\label{chap:execution}



% -----------------------------------------------------------------------------

\chapter{Critical Evaluation}\label{chap:evaluation}

% -----------------------------------------------------------------------------

\chapter{Conclusion}\label{chap:conclusion}

% =============================================================================

\backmatter{}

\bibliography{dissertation}

% =============================================================================

\end{document}
