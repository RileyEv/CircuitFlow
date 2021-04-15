
\documentclass[
author={Riley Evans},
supervisor={Dr. Meng Wang},
degree={MEng},
title={\vbox{Circuit: A Domain Specific Language for Dataflow Programming}},
subtitle={},
type={research},
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
Dataflow programming is a paradigm that models applications as a directed graph.
The nodes of the graph have inputs and outputs and are pure functions, therefore have no side effects.
It is possible for a node to be a: source; sink; or processing node.
Edges connect these nodes together, and define the flow of information.
\todo[inline]{this feels a little light on detail }


\paragraph{Example - Data Pipelines}
A common use of dataflow programming is in pipelines that process data.
This paradigm is particularly helpful as it helps the developer to focus on each specific transformation on the data as a single component.
Avoiding the need for long and laborious scripts that could be hard to maintain.

\paragraph{Example - Quartz Composer}
Apple developed a tool included in XCode, named Quartz Composer, which is a node-based visual programming language~\cite{quartz}.
It allows for quick development of programs that process and render graphical data.
By using visual programming it allows the user to build programs, without having to write a single line of code.
This means that even non-programmers are able to use the tool.

\paragraph{Example - Spreadsheets}
A widely used example of dataflow programming is in spreadsheets.
A cell in a spreadsheet can be thought of as a single node.
It is possible to specify dependencies to other cells through the use of formulas.
Whenever a cell is updated it sends its new value to those who depend on it, and so on.
Work has also done to visualise spreadsheets using dataflow diagrams, to help debug ones that are complex\cite{hermans2011breviz}.


\subsection{The Benefits}
\paragraph{Visual}
The dataflow paradigm uses graphs, which make programming visual.
It allows the end-user programmer to see how data passes through the program, much easier than in an imperative approach.
In many cases, dataflow programming languages use drag and drop blocks with a graphical user interface to build programs,
for example Tableau Prep~\cite{tableauPrep}.
This makes programming more accessible to users who do not have programming skills.

\paragraph{Implicit Parallelism}
Moore's law states that the number of transistors on a computer chip doubles every two years~\cite{4785860}.
This meant that the chips processing speeds also increased in alignment with Moore's law.
However, in recent years this is becoming harder for chip manufacturers to achieve~\cite{bentley_2020}.
Therefore, chip manufactures have had to turn to other approaches to increase the speed of new chips, such as multiple cores.
It is this approach the dataflow programming can effectively make use of.
Since each node in a dataflow is a pure function, it is possible to parallelise implicitly.
No node can interact with another node, therefore there are no data dependencies outside of those encoded in the dataflow.
Thus eliminating the ability for a deadlock to occur.

\subsection{Dataflow Diagrams}
Dataflow programs are typically viewed as a graph.
An example dataflow graph along with its corresponding imperative approach, is visible in Figure~\ref{fig:dataflow-example}.
In this diagram is possible to see how implicit parallelisation is possible.
Both $A$ and $B$ can be calculated simultaneously, with $C$ able to be evaluated after they are complete.


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
  \caption{An example dataflow and its imperative approach.}
    \label{fig:dataflow-example}
\end{figure}


\subsection{Kahn Process Networks}
A method introduced by Gilles Kahn, called Kahn Process Networks (KPN) realised this concept through the use of threads
and unbounded FIFO queues~\cite{DBLP:conf/ifip/Kahn74}.
A node in the dataflow becomes a thread in the process network.
These threads are then able to communicate through FIFO queues.
The node can have multiple input queues and is able to read any number of values from them.
It will then compute a result and add it to an output queue.
A requirement of KPNs is that a thread is suspended if it attempts to fetch a value from an empty queue.
It is not possible for a process to test for the presence of data in a queue.

Parks described a variant of KPNs, called Data Processing networks~\cite{381846}.
They recognise that if functions have no side effects then they have no values to be shared between each firing.
Therefore, a pool of threads can be used with a central scheduler instead.




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


\subsection{Deep Embeddings}
A deep embedding is when the terms of the DSL will construct an Abstract Syntax Tree (AST) as a host language datatype.
Semantics can then be provided later on with an |eval| function.
Consider the example of a minimal non-deterministic parser combinator library~\cite{wuYoda}.

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

A key benefit for deep embeddings is that the structure can be inspected, and then modified to optimise the user code.
However, they also have drawbacks - it can be laborious to add a new constructor to the language.
Since it requires that all functions that use the deep embedding be modified to add a case for the new constructor.


\subsection{Shallow Embeddings}
In contrast, a shallow approach is when the terms of the DSL are defined as first class components of the language.
For example, a function in Haskell.
Components can then be composed together and evaluated to provide the semantics of the language.
Again a simple parser example can be considered.

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

Using a shallow implementation has the benefit of being able add new `constructors' to a DSL, without having to modify any other functions.
Since each `constructor', produces the desired result directly.
However, this causes one of the main disadvantages of a shallow embedding - you cannot inspect the structure.
This means that optimisations cannot be made to the structure before evaluating it.


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
