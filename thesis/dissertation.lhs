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


\long\def\ignore#1{}
\ignore{
\begin{code}
{-# ANN module "HLint: ignore" #-}
\end{code}
}

\maketitle

% =============================================================================

\frontmatter

% -----------------------------------------------------------------

\makedecl{}

% -----------------------------------------------------------------------------

\chapter*{Acknowledgements}

I would like to thank the following people for their help and support:


\begin{itemize}
  \item Jamie Willis for giving me pointers on Haskell techniques to look at, and convincing me to keep going even when I thought it wasn't possible.
  \item My flatmates Chris \& Jack for their support.
  \item My supervisor Meng, who noticed the value of indexed functors and encouraged me complete a project based on them.
  \item Finally, Sam who has provided tips and ideas that have been invaluable throughout this project, as well as taking the time to proof-read this work.
\end{itemize}

% -----------------------------------------------------------------

\tableofcontents

% -----------------------------------------------------------------

\listoffigures

% -----------------------------------------------------------------------------

\subfileinclude{chapters/abstract}

% -----------------------------------------------------------------

%% \subfileinclude{chapters/technologies}

% -----------------------------------------------------------------------------

\chapter*{Acronyms}

Throughout this thesis, several acronyms will be used:

\begin{acronym}
  \acro{DSL}{Domain Specific Language}
  \acro{e-DSL}{Embedded DSL}
  \acro{FIFO}{First-In First-Out}
  \acro{KPN}{Kahn Process Network}
  \acro{GPL}{General Purpose Language}
  \acro{DPN}{Data Process Network}
  \acro{DAG}{Directed Acyclic Graph}
  \acro{AST}{Abstract Syntax Tree}
  \acro{PID}{Process Identifier}
  \acro{WHNF}{Weak Head Normal Form}
\end{acronym}




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
