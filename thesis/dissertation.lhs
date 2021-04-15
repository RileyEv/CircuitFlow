
\documentclass[
author={Riley Evans},
supervisor={Dr. Meng Wang},
degree={MEng},
title={\vbox{Circuit: A Domain Specific Language for Dataflow Programming}},
subtitle={},
type={Research},
year={2021}
]{dissertation}
  
%% \usepackage{libertine}
\usepackage{todonotes}
\usepackage{caption}
\usepackage{subcaption}
\usepackage{amsmath}

% lhs2tex setup

%include format.fmt
%options ghci

\begin{document}
  
%if False

\begin{code}
{-# LANGUAGE KindSignatures, GADTs, LambdaCase #-}
module Dissertation where
import Prelude hiding (or)
import Data.Kind (Type)
\end{code}
%endif

\maketitle

% =============================================================================

\frontmatter
\makedecl{}
\tableofcontents
\listoftodos

% -----------------------------------------------------------------------------

% \chapter*{Supporting Technologies}

% % -----------------------------------------------------------------------------

% \chapter*{Notation and Acronyms}

% maybe?

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
programming paradigm that represents applications as a DAG (like a dataflow diagram).
nodes with inputs and outputs. nodes are sources, sinks or processing nodes.
nodes connected by directed edges which define the flow of information

\subsection{The Benefits}
\paragraph{Visual}
visual programming language, easier for the end user to visualise what is happening.

\paragraph{Implicit Parallelism}
implicit parallelism~\cite{10.1145/1013208.1013209}, each node is pure and has no side effects.
no data dependencies.
parallelism is now more important because of multicore cpus and the need to process large amounts
of data, that can benefit from parallel processing.

\subsection{Dataflow Diagrams}

In the traditional imperative approach, the code written will be sequential, with each line executed one after another. An example is visible in Figure~\ref{subfig:dataflow-example-equations}.
However, in

\begin{figure}[ht]
  \centering
  \begin{subfigure}{0.3\textwidth}
    \centering
    \begin{equation*}
      \begin{aligned}
      A &:= 100 \times X \\
      B &:= X + Y \\
      C &:= A - B \\
      \end{aligned}
    \end{equation*}
    \caption{}
    \label{subfig:dataflow-example-equations}
  \end{subfigure}
  \begin{subfigure}{0.3\textwidth}
    \centering
    \input{diagrams/dataflow-example}
    \caption{}
    \label{subfig:dataflow-example-diagram}
  \end{subfigure}
  \caption{an example \ref{subfig:dataflow-example-diagram}}
\end{figure}

\todo[inline]{Give some example diagrams for a data flow}
Give a simple expression style program and a dataflow equivalent.

\paragraph{Batch Processing}
What is it? and where is it used?

An example?

\paragraph{Stream Processing}
What is it and where is it used?

An example?

\paragraph{Batch vs Stream}
Comparison of features, discuss how dataflow programming can be used for both.


\subsection{Kahn Process Networks}
What are they?

How can they be used to model a Kahn process network? Maybe give an example diagram

Discuss the specific case i am using, where the firing of a node will always pop 1 from the input tape.




\section{Domain Specific Languages (DSLs)}
A Domain Specific Language (DSL) is a programming language unit that has a specialised domain or use-case.
This differs from a General Purpose Language (GPL), which can be applied across a larger set of domains.
HTML is an example of a DSL, it is good for describing the appearance of websites, however,
it cannot be used for more generic purposes, such as adding two numbers together.

\paragraph{Approaches to Implementation}
DSLs are typically split into two categories: standalone and embedded.
Standalone DSLs require their own compiler and typically their own syntax; HTML would be an example of a standalone DSL.
Embedded DSLs use an existing language as a host, therefore they use the syntax and compiler from the host.
This means that they are easier to maintain and often quicker to develop than standalone DSLs.
An embedded DSL, can be implemented using two differing techniques: shallow and deep embeddings.

\todo[inline]{Add something about why embedded DSLs are used in Haskell}

\subsection{Shallow Embeddings}
A shallow approach, is when the terms of the DSL are defined as first class components of the language.
For example, a function in Haskell.
Components can then be composed together and evaluated to provide the semantics of the language.
Consider the example of a minimal non-deterministic parser combinator library~\cite{wuYoda}.

\begin{code}
newtype Parser a = Parser {parse :: String -> [(a, String)]}

or :: Parser a -> Parser a -> Parser a
or (Parser px) (Parser py) = Parser (\ts -> px ts ++ py ts)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser (\case
  []       -> []
  (t:ts')  -> [(t, ts') | p t])
\end{code}

\noindent
This can be used to build a parser that can parse the characters |'a'| or |'b'|.

\begin{code}
aorb :: Parser Char
aorb = satisfy (== 'a') `or` satisfy (== 'b')
\end{code}

\noindent
The program can then be evaluated by the |parse| function.
For example, |parse aorb "a"| evaluates to \eval{parse aorb "a"}, and |parse aorb "c"| evaluates to \eval{parse aorb "c"}.

\todo[inline]{Add the advantages of shallow embeddings.}

\subsection{Deep Embeddings}

Alternatively, a deep embedding can be used to represent a DSL.
This is when the terms of the DSL will construct an Abstract Syntax Tree (AST) as a host language datatype.
Semantics can then be provided later on with an |eval| function.
Again a simple parser example can be considered.

%format Parser2
%format aorb2
%format parse2

\begin{code}
data Parser2 (a :: Type) where
  Satisfy  :: (Char -> Bool)  -> Parser2 Char
  Or       :: Parser2 a       -> Parser2 a -> Parser2 a
\end{code}

\noindent
The same |aorb| parser can be created\todo{reads dodgy} by creating an AST.

\begin{code}
aorb2 :: Parser2 Char
aorb2 = Satisfy (== 'a') `Or` Satisfy (== 'b')
\end{code}

\noindent
However, this parser does not have any semantics, therefore this needs to be provided by the evaluation function |parse2|.

\begin{code}
parse2 :: Parser2 a -> String -> [(a, String)]
parse2 (Satisfy p) = \case
  []       -> []
  (t:ts')  -> [(t, ts') | p t]
parse2 (Or px py) = \ts -> parse2 px ts ++ parse2 py ts
\end{code}


\todo[inline]{Add the advantages of deep embeddings.}


\section{Higher Order Functors}
Introduce the need for them\ldots folding typed ASTs to provide syntax.
\begin{itemize}
  \item IFunctors, imap, natural transformation
  \item Maybe drop some cat theory diagrams
  \item IFix
  \item Their use for DSL development, icata, small example.
  %% \item Data types a la carte
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

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
