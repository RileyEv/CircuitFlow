\documentclass[
author={Riley Evans},
supervisor={Dr. Meng Wang},
degree={MEng},
title={\vbox{CircuitFlow: A Domain Specific Language for Dataflow Programming}},
subtitle={},
type={research},
year={2021}
]{dissertation}


\usepackage{todonotes}
\setlength{\marginparwidth}{2cm}
\usepackage{caption}
\usepackage{subcaption}
\usepackage{amsmath}
\usepackage{tikz-cd}
\usepackage{latexsym}
\usepackage{acronym}
\usepackage{listings}
\usepackage{amssymb}
\usepackage[T1]{fontenc}
\usepackage{textcomp}
\usepackage{lmodern}
\usetikzlibrary{arrows.meta}
\usetikzlibrary{decorations.markings}
\usepackage{pgfplots}
\pgfplotsset{compat=newest}
\usepgfplotslibrary{groupplots}
\usepgfplotslibrary{dateplot}

\usepackage{subfiles} % load last

\setcounter{tocdepth}{2}

% lhs2tex setup
%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\maketitle

% =============================================================================

\frontmatter

% -----------------------------------------------------------------

\makedecl{}

% -----------------------------------------------------------------

\tableofcontents

% -----------------------------------------------------------------

\listoffigures

% -----------------------------------------------------------------------------

\subfileinclude{chapters/abstract}

% -----------------------------------------------------------------

%% \subfileinclude{chapters/technologies}

% -----------------------------------------------------------------------------

\chapter*{Notation and Acronyms}

\begin{acronym}
  \acro{DSL}{Domain Specific Language}
  \acro{EDSL}{Embedded DSL}
  \acro{FIFO}{First-In First-Out}
  \acro{KPN}{Kahn Process Network}
  \acro{GPL}{General Purpose Language}
  \acro{DPN}{Data Process Network}
  \acro{DAG}{Directed Acyclic Graph}
  \acro{AST}{Abstract Syntax Tree}
  \acro{PID}{Process Identifier}
\end{acronym}


% -----------------------------------------------------------------------------

\chapter*{Acknowledgements}

\todo[inline]{Change this to something meaningful}
\noindent
It is common practice (although totally optional) to acknowledge any
third-party advice, contribution or influence you have found useful
during your work.  Examples include support from friends or family,
the input of your Supervisor and/or Advisor, external organisations
or persons who  have supplied resources of some kind (e.g., funding,
advice or time), and so on.

% =============================================================================

\mainmatter{}

\subfileinclude{chapters/introduction}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/background}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/the-language}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/implementation}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/examples}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/benchmarks}

% -----------------------------------------------------------------------------

\subfileinclude{chapters/conclusion}

% =============================================================================

\backmatter{}

% -----------------------------------------------------------------------------

\bibliography{dissertation}

% =============================================================================

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
