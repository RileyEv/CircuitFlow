\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Language where
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
  Task :: (DataSource f a, DataSource g b) => (f a -> g b -> IO (g b)) -> g b -> Task f a g b
\end{code}

When a |Task| is executed the stored function is executed, with the input being passed in as the first argument and the output ``pointer'' as the second argument.
This returns an output |DataStore| that can be passed on to another |Task|


\section{Chains}
In a dataflow programming, one of the key aspects is the definition of dependencies between tasks in the flow.
One possible approach to encoding this concept in the language is to make use of sequences of tasks --- also referred to as chains.
These chains compose tasks, based on their dependencies.
For example a chain operator |>>>| can be defined:

\begin{spec}
(>>>) :: Task f a g b -> Task g b h c -> Task f a h c
\end{spec}

\subsection{Idea behind them}
\subsection{Joining Chains into a tree}
\paragraph{PIDs}
\subsection{Examples}
\subsection{Benefits}
\subsection{Problems}

\section{Circuit}
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
