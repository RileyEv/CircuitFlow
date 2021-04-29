\documentclass[dissertation.tex]{subfiles}


%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, KindSignatures, GADTs, RankNTypes #-}
module Language where
import Data.Kind (Type)
\end{code}
}

\chapter{The Language}\label{chap:the-language}

\section{Language Requirements}
For the design of the language to be considered a success, several criteria need to be met:

\begin{itemize}
  \item \textbf{Easy to build} --- This is critical to the success of the language, if it is not simple to use then no one will want to use it.
        It is important that when defining an application the programmer has a clear understanding of how it will behave.
  \item \textbf{Type-safe} --- A feature missing in many Python dataflow tools is the lack of type checking.
        This causes problems later on in the development process with more debugging and testing needed.
        The language should be type-safe to avoid any run-time errors occurring where types do not match.
\end{itemize}

\section{Tasks}
Tasks are the core construct in the language.
They are responsible for reading from an input data source, completing some operation on the input, then finally writing to an output data sink.
Tasks could take many different forms, for example they could be:

\begin{itemize}
  \item A pure function - a function with type |a -> b|
  \item An external operation - interacting with some external system. For example, calling a terminal command.
\end{itemize}

\todo[inline]{Haskell is good for pure functions}


A task could have a single input or multiple inputs, however, for now just a single input task will be considered.
Multi-input tasks are explained further in Sub-Section~\ref{sec:multi-input-tasks}

\subsection{Data Stores}
Data stores are used to pass values between different tasks, this ensures that the input and output of tasks are closely controlled.
A data store can be defined as a type class, with two methods |fetch| and |save|:

\begin{code}
class DataStore f a where
  fetch  :: f a  ->  IO a
  save   :: f a  ->  a -> IO (f a)
\end{code}

A |DataStore| is typed, where |f| is the type of |DataStore| being used and |a| is the type of the value stored inside it.
The aptly named methods describe their intended function: |fetch| will fetch a value from a |DataStore|, and |save| will save a value.
The |fetch| method takes a DataStore as input and will return the value stores inside.
However, the |save| method may not be as self explanatory, since it has an extra |f a| argument.
This argument can be thought of as a pointer to a |DataStore|: it contains the information needed to save.
For example, in the case of a file store it could be the file name.

By implementing as a type class, there can be many different implementations of a |DataStore|.
The library comes with several pre-defined |DataStore|s, such as a |VariableStore|.
This can be though of as an in memory storage between tasks.

\begin{code}
data VariableStore a = Var a | Empty

instance DataStore VariableStore a where
  fetch (Var x) = return x
  save Empty x = return (Var x)
\end{code}

The |VariableStore| is the most basic example of a |DataStore|, a more complex example could be the aforementioned |FileStore|:

\begin{code}
newtype FileStore a = FileStore String

instance DataStore FileStore String where
  fetch (FileStore fname) = readFile fname
  save (FileStore fname) x = writeFile fname x >> return (FileStore fname)

instance DataStore FileStore [String] where
  fetch (FileStore fname) = readFile fname >>= return . lines
  save (FileStore fname) x = writeFile fname (unlines x) >> return (FileStore fname)
\end{code}

The |FileStore| is only defined to store two different types: |String| and |[String]|.
If a user attempts to store anything other than these two types then a compiler error will be thrown, for example:

@ghci> @|save (FileStore "test.txt") (123 :: Int)|

@> No instance for (@|DataStore FileStore Int|@) arising from a use of `@|save|@'@


Although a small set of |DataStore|s are included in the library, the user is also able to add new instances of the type class with their own |DataStore|s.
Some example expansions, could be supporting writing to a database table, or a Hadoop file system.


\subsection{Task Constructor}
A task's type details the type of the input and outputs.
It requires two arguments to the constructor, the function that will be invoked and an output |DataStore|.
The constructor makes use of GADTs syntax so that constraints can be placed on the types used.
It enforces that a |DataStore| must exist for the input and output types.
This allows the task to make use of the |fetch| and |save| functions.

\begin{code}
data Task (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Task :: (DataStore f a, DataStore g b) => (f a -> g b -> IO (g b)) -> g b -> Task f a g b
\end{code}

When a |Task| is executed the stored function is executed, with the input being passed in as the first argument and the output ``pointer'' as the second argument.
This returns an output |DataStore| that can be passed on to another |Task|



\section{Chains}
In a dataflow programming, one of the key aspects is the definition of dependencies between tasks in the flow.
One possible approach to encoding this concept in the language is to make use of sequences of tasks --- also referred to as chains.
These chains compose tasks, based on their dependencies. A chain can be modelled with an abstract datatype:

\begin{code}
data Chain (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Chain  ::  Task   f  a  g  b  ->  Chain  f  a  g  b
  Then   ::  Chain  f  a  g  b  ->  Chain  g  b  h  c -> Chain f a h c
\end{code}

%if style /= newcode
%format >>> = ">\!\!>\!\!>"
%endif

This allows for a |Task| to be combined with others to form a chain. To make this easier to use a chain operator |>>>| can be defined:

\begin{code}
(>>>) :: Chain f a g b -> Chain g b h c -> Chain f a h c
(>>>) = Then
\end{code}

This can be now be used to join sequences of tasks together, for example:

\begin{spec}
task1  ::  Task  VariableStore  Int       VariableStore  String
task2  ::  Task  VariableStore  String    FileStore      [String]
task3  ::  Task  FileStore      [String]  VariableStore  Int

sequence :: Chain VariableStore Int VariableStore Int
sequence = Chain task1 >>> Chain task2 >>> Chain task3
\end{spec}

|sequence| will perform the three tasks in order, starting with |task1| and finishing with |task3|.


\subsection{Trees as Chains}
Now that tasks can be performed in sequence, the next logical step will be to introduce the concept of branching out.
This results in a tasks output being given to multiple tasks, rather than just 1.

To do this a new abstract datatype is required.
This will be used to form a list of |Chain|s, conventionally the |[]| type would be used,
however this is not possible as each chain will have a different type.
This means that existential types will need to be used.~\todo{cite where this comes from?}

\begin{code}
data Pipe where
  Pipe  ::  forall f a g b. (DataStore f a, DataStore g b) => Chain f a g b -> Pipe
  And   ::  Pipe -> Pipe -> Pipe
\end{code}

The |And| constructor can be used to combine multiple chains together.
Figure~\ref{fig:pipe-dataflow-example} shows the previous |sequence|, with a new |task4| which also uses the input from |task2|.

\begin{figure}[ht]
\centering
\begin{subfigure}{0.7\textwidth}
\centering
\begin{spec}
task4 :: Task FileStore [String] VariableStore String

branchExample :: Pipe
branchExample =  Pipe (Chain task1  >>> Chain  task2 >>> Chain task3)
                 `And`
                 Pipe (Chain task2  >>> Chain  task4)
\end{spec}
\vspace{-7mm}
\caption{}
\vspace{5mm}
\end{subfigure}
\begin{subfigure}{0.7\textwidth}
\centering
\begin{tikzpicture}[node distance={24mm}, main/.style = {draw, circle, thick}]
\node[main] (task1) {|task1|};
\node[main] (task2) [right of=task1] {|task2|};
\node[main] (task3) [right of=task2] {|task3|};
\node[main] (task4) [below of=task3, right of=task2] {|task4|};
\node (input) [left of=task1] {$ $};
\node (out1)  [right of=task3] {$ $};
\node (out2)  [right of=task4] {$ $};

\draw[->, >=stealth] (input) -- (task1);
\draw[->, >=stealth] (task1) -- (task2);
\draw[->, >=stealth] (task2) -- (task3);
\draw[->, >=stealth] (task2) -- (task4);
\draw[->, >=stealth] (task3) -- (out1);
\draw[->, >=stealth] (task4) -- (out2);

\end{tikzpicture}
\caption{}
\end{subfigure}
\caption{A |Pipe| (a) and its corresponding dataflow diagram (b).}
\label{fig:pipe-dataflow-example}
\end{figure}

There is, however, one problem with this approach.
To be able to form a network similar to that shown in Figure~\ref{fig:pipe-dataflow-example},
the language will need to know where to join two |Chain|s together.
However, with the current definition of a Task, it is not possible to easily check the equivalence of two functions.
Similarly, if a user wanted to use the same task multiple times, it would not be possible to differentiate between them.

\paragraph{\acfp{PID}}
This is where the concept of \acfp{PID} are useful.
A Chain can be modified so that instead of storing a |Task| it instead stores a |PID|.
The |PID| data type can make use of phantom type parameters, to retain the same information as a |Task|,
whilst storing just an |Int| that can be used to identify it.

%if style /= newcode
%format Chain'
%format Then'
%endif

\begin{code}
data PID (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  PID :: Int -> PID f a g b

data Chain' (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Chain'  ::  PID     f  a  g  b  ->  Chain'  f  a  g  b
  Then'   ::  Chain'  f  a  g  b  ->  Chain'  g  b  h  c -> Chain' f a h c
\end{code}

This however leaves a key question, how do |Task|s get mapped to |PID|s.
This can be done by employing the State monad.
This state stores a map from PID to task and a counter for PIDs.
As |Task|s each have a different type again a new datatype is required to use existential types, so that just one type is stored in the map.
By creating a type alias for the State monad, the |Workflow| monad can now be used.

\begin{code}
data TaskWrap = forall f a g b. TaskWrap (Task f a g b)

data WorkflowState = WorkflowState {
  pidCounter :: Int,
  tasks :: M.Map Int TaskWrap
}

type Workflow = State WorkflowState
\end{code}

There is only one operation that is defined in the monad --- |registerTask|.
This takes a |Task| and returns a |Chain| that stores a |PID| inside it.
Whenever a user would like to add a new task to the workflow, they register it.
They can then use this returned value to construct multiple chains, which can now be joined easily by comparing the stored \ac{PID}.

\begin{spec}
registerTask :: Task f a g b -> Workflow (Chain' f a g b)
\end{spec}

One benefit to this approach is that if the user would like to use a task again in a different place,
they can simply register it again and use the new PID value.

\subsection{Evaluation}
\paragraph{Easy to Build}
The concept of chains are easy for a user to grapple with.
Chains can be any length and represent paths along a dataflow graph.
The chains can be any length that the user requires.
This gives them the choice of how to structure the program.
In one case they could specify a minimal number of chains that describe the dataflow graph.
However, another approach from the user could be to just focus dependencies between each tasks,
and specifying a chain for each edge in the dataflow graph.


\paragraph{Type-safe}
Although a |Chain| can be well typed, the use of existential types to join chains together pose a problem.
This causes the types to be `hidden', this means that when executing these tasks, the types need to be recovered.
This is possible through the use of |gcast| from the |Data.Typeable| library.
However, this has to perform a reflection at run-time to compare the types.
There is the possibility that the types could not matching and this would only be discovered at run-time.
There is a mechanism to handle the failed match case, however, this does not fulfil the criteria of being fully type-safe.


\section{Circuit}
This approach was inspired by the parallel prefix circuits as described by Hinze~\cite{scans}.
It uses constructors similar to those used by Hinze to create a circuit that represents the \ac{DAG}, used in the dataflow.
The constructors seen in Figure~\ref{fig:circuit-constructors} represent the behaviour of edges in a graph.


%format <-> = "<\!\!-\!\!>"

\newcommand{\centered}[1]{\begin{tabular}{l} #1 \end{tabular}}
\begin{figure}[ht]
\centering
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/id}
\caption{|id|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/replicate}
\caption{|replicate|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/swap}
\caption{|swap|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/then}
\caption{|a <-> b|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/beside}
\caption{|a <> b|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/task}
\caption{|task f|}
\end{subfigure}

\caption{The constructors in the |Circuit| library alongside their graphical representation.}
\label{fig:circuit-constructors}
\end{figure}


\subsection{Idea}
More likened to a way to construct a workflow by its corresponding dataflow graph.
\subsection{Typing}
\subsection{Tasks with multiple inputs}\label{sec:multi-input-tasks}
\paragraph{Apply}
\subsection{Examples}
\paragraph{Song aggregation}
\paragraph{lhs2tex Build System}
\subsection{Benefits}
\subsection{Problems}

\todo[inline]{where does mapC operator go?}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
