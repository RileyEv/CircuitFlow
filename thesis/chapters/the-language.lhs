\documentclass[dissertation.tex]{subfiles}


%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, KindSignatures, GADTs, RankNTypes, DataKinds, TypeFamilies, PolyKinds #-}
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

\subsection{Data Stores}\label{sec:data-stores}
Data stores are used to pass values between different tasks, this ensures that the input and output of tasks are closely controlled.
A data store can be defined as a type class, with two methods |fetch| and |save|:

\todo[inline]{Motivate further why a |DataStore| needs to exist --- prevents the user from reading from a source incorrectly.}

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

\vspace{3mm}
@ghci> @|save (FileStore "test.txt") (123 :: Int)|

@> No instance for (@|DataStore FileStore Int|@) arising from a use of `@|save|@'@
\vspace{3mm}

\noindent
Although a small set of |DataStore|s are included in the library, the user is also able to add new instances of the type class with their own |DataStore|s.
Some example expansions, could be supporting writing to a database table, or a Hadoop file system.


\subsection{Task Constructor}
A task's type details the type of the input and outputs.
It requires two arguments to the constructor, the function that will be invoked and an output |DataStore|.
The constructor makes use of GADTs~\cite{10.1145/1160074.1159811} syntax so that constraints can be placed on the types used.
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


\newcommand{\centered}[1]{\begin{tabular}{l} #1 \end{tabular}}
\begin{figure}[hbt]
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

%format DataStore'

\subsection{Constructors}
Each of these constructors use strong types to ensure that they are combined correctly.
A |Circuit| has 7 different type parameters:

\begin{spec}
Circuit  (inputsStorageTypes   :: [Type -> Type])  (inputsTypes   :: [Type])  (inputsApplied   :: [Type])
         (outputsStorageTypes  :: [Type -> Type])  (outputsTypes  :: [Type])  (outputsApplied  :: [Type])
         (nInputs :: Nat)
\end{spec}

A |Circuit| can be thought of as a list of inputs, which are processed and a resulting list of outputs are produced.
To represent this it makes use of the |DataKinds| language extension~\cite{10.1145/2103786.2103795}, to use type-level lists and natural numbers.
Each parameter represents a certain piece of information needed to construct a circuit:

\begin{itemize}
\item |inputStorageTypes| is a type-list of storage types, for example |(Q([VariableStore, CSVStore]))|.
\item |inputTypes| is a type-list of the types stored in the storage, for example |(Q([Int, [(String, Float)]]))|.
\item |inputsApplied| is a type-list of the storage types applied to the types stored, for example\\ |(Q([VariableStore Int, CSVStore [(String, Float)]]))|.
\item |outputsStorageTypes|, |outputTypes| and |outputsApplied| mirror the examples above, but for the outputs instead.
\item |nInputs| is a type-level Nat that is the length of the input lists.
\end{itemize}

In the language there are two different types of constructor, those that recurse and those that can be considered leaf nodes.
The behaviour of both types of constructor is recorded within the types, using phantom type parameters~\cite{phantom_types}.
For example, the |id| constructor has the type:

\begin{spec}
id :: DataStore' (Q([f])) (Q([a])) => Circuit (Q([f])) (Q([a])) (Q([f a])) (Q([f]) (Q([a])) (Q([f a])) N1
\end{spec}

It can be seen how the type information for this constructor states that it has 1 input value of type |f a| and it returns that same value.
Some more interesting examples would be the |swap| and |replicate|:

\begin{spec}
replicate  :: DataStore'  (Q([f]))     (Q([a]))     => Circuit  (Q([f]))     (Q([a]))     (Q([f a]))       (Q([f,  f]))  (Q([a, a]))  (Q([f  a,  f a]))  N1
swap       :: DataStore'  (Q([f, g]))  (Q([a, b]))  => Circuit  (Q([f, g]))  (Q([a, b]))  (Q([f a, g b]))  (Q([g,  f]))  (Q([b, a]))  (Q([g  b,  f a]))  N2
\end{spec}

The |replicate| constructor states that a single input value of type |f a| should be input, and that value should then be duplicated and output.
The |swap| constructor takes two values as input: |f a| and |g b|. It will then swap these values over, such that the output will now be: |g b| and |f a|.

All three of these constructors are leaf nodes in the \ac{AST}. To be able to make use of them they need to be combined in some way.
To do this two new constructors named `beside' and `then' will be used.
However, before defining these constructors there are some tools that are required.
This is due to the types no longer being concrete. \todo{might be a bit hand wavy description...? }
For example, the input type list is no longer known: it can only be referred to as |fs| and |as|.
This means it is much harder to specify the new type of the |Circuit|.


% Both of these two constructors, however, have multiple inputs and/or multiple outputs.
% But currently it is not possible to place two circuits next to each other to receive these two inputs.
% This is where the |<>| operator is needed. It allows two circuits to be placed next to each other.
% \todo{also mention then}


\paragraph{Apply Type Family}
It would not be possible to use a new type variable |xs| for the |inputsApplied| parameter.
This is because it needs to be constrained so that it is equivalent to |fs| applied to |as|.
To solve this a new closed type family~\cite{10.1145/2535838.2535856} is created that is able to apply the two type lists together.
This type family pairwise applies a list of types storing with kind |* -> *| to a list of types with kind |*| to form a new list containing types of kind |*|.
For example, |Apply (Q([f, g, h])) (Q([a, b, c])) ~ (Q([f a, g b, h c]))|.


%format :+ = ":\!\!+"
%format :++ = ":\!\!+\!\!+"

\todo[inline]{': looks awful }

\begin{spec}
type family Apply (fs :: [Type -> Type]) (as :: [Type]) where
  Apply  (Q([]))       (Q([]))       = (Q([]))
  Apply  (f (Q(:)) fs) (a (Q(:)) as) = f a (Q(:)) Apply fs as
\end{spec}


\paragraph{Append Type Family}
There will also be the need to append two type level lists together.
To do this an append type family~\cite{10.1145/1017472.1017488} can be used:

%format l'

\begin{spec}
type family (:++) (l1 :: [k]) (l2 :: [k]) :: [k] where
  (:++)  (Q([]))       l  = l
  (:++)  (e (Q(:)) l)  l' = e (Q(:)) (l :++ l')
\end{spec}

This type family makes use of the language extension |PolyKinds|~\cite{10.1145/2103786.2103795} to allow for the append to be polymorphic on the kind stored in the type list.
This will avoid defining multiple versions to append |fs| with |gs|, and |as| with |bs|.

\paragraph{The `Then' Constructor}
This constructor --- denoted by |<->| --- is used to stack two circuits on top of each other.
Through types it enforces that the output of the top circuit is the same as the input to the bottom circuit.

\begin{spec}
(<->) :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
  => Circuit  fs  as  (Apply  fs  as)  gs  bs  (Apply  gs  bs)  nfs
  -> Circuit  gs  bs  (Apply  gs  bs)  hs  cs  (Apply  hs  cs)  ngs
  -> Circuit  fs  as  (Apply  fs  as)  hs  cs  (Apply  hs  cs)  nfs
\end{spec}

\paragraph{The `Beside' Constructor}
Denoted by |<>|, the beside constructor is used to place two circuits side-by-side.
The resulting |Circuit| has the types of left and right circuits appended together.

\begin{spec}
(<>) :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs, DataStore' is ds)
  =>  Circuit  fs  as  (Apply  fs  as)  gs  bs  (Apply  gs  bs)  nfs
  ->  Circuit  hs  cs  (Apply  hs  cs)  is  ds  (Apply  is  ds)  nhs
  ->  Circuit  (fs  :++  hs)   (as  :++  cs)  (Apply  fs  as  :++ Apply  hs  cs)
               (gs  :++  is)   (bs  :++  ds)  (Apply  gs  bs  :++ Apply  is  ds)
               (nfs :+ nhs)
\end{spec}

\subsection{Combined DataStores}

%format HList'

A keen eyed reader may notice that all of these constructors have not been using the original |DataStore| type class.
Instead they have all used the |DataStore'| type class.
This is a special case of a |DataStore|, it allows for them to also be defined over type lists, not just a single type.
Combined DataStores make it easier for tasks to fetch from multiple inputs.
Users will just have to call a single |fetch'| function, rather than multiple.

To be able to define |DataStore'|, heterogeneous lists~\cite{10.1145/1017472.1017488} are needed --- specifically three different forms.
|HList| is as defined by TODO, |HList'| stores values of type |f a| and is parameterised by two type lists |fs| and |as|.
|IOList| stores items of type |IO a| and is parameterised by a type list |as|. Their definitions are:

%format HCons'
%format HNil'

\begin{spec}
data HList (xs :: [Type]) where
  HCons  :: x -> HList xs -> HList (x (Q(:)) xs)
  HNil   :: HList (Q([]))

data HList' (fs :: [Type -> Type]) (as :: [Type]) where
  HCons'  :: f a -> HList' fs as -> HList' (f (Q(:)) fs) (a (Q(:)) as)
  HNil'   :: HList' (Q([])) (Q([]))

data IOList (xs :: [Type]) where
  IOCons  :: IO x -> IOList xs -> IOList (x (Q(:)) xs)
  IONil   :: IOList (Q([]))
\end{spec}

Now that there is a mechanism to represent a list of different types, it is possible to define |DataStore'|:

%format fetch'
%format save'

\begin{spec}
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  fetch'  :: HList' fs as ->  IOList  as
  save'   :: HList' fs as ->  HList   as -> IOList (Apply fs as)
\end{spec}

However, it would be cumbersome to ask the user to define an instance of |DataStore'| for every possible combination of data stores.
Instead, it is possible to make use of the previous |DataStore| type class.
To do this instances can be defined for |DataStore'| that make use of the existing |DataStore| instances:

\begin{spec}
instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' (Q([f])) (Q([a])) where
  fetch'  (HCons' x    HNil')                 = IOCons  (fetch x)     IONil
  save'   (HCons' ref  HNil') (HCons x HNil)  = IOCons  (save ref x)  IONil

instance (DataStore f a, DataStore' fs as) => DataStore' (f (Q(:)) fs) (a (Q(:)) as)  where
  fetch'  (HCons' x    xs)               = IOCons  (fetch uuid x)  (fetch' xs)
  save'   (HCons' ref  rs) (HCons x xs)  = IOCons  (save ref x)    (save' rs xs)
\end{spec}

This means that a user does not need to create any instances of |DataStore'|.
They can instead focus on each single case, with the knowledge that they will automatically be able to combine them with other |DataStore|s.


\subsection{Multi-Input Tasks}\label{sec:multi-input-tasks}
With a |Circuit| it is possible to represent a \ac{DAG}.
This means that a node in the graph can now have multiple dependencies, as seen in Figure~\ref{fig:multi-depen-task}.

\begin{figure}[ht]
\centering
\input{diagrams/circuit-constructors/multi-input}
\caption{A graphical representation of a task with multiple dependencies}
\label{fig:multi-depen-task}
\end{figure}

To support this a modification can be made to the |task| constructor.
Rather than have an input value type of |f a|.
It can now have an input value type of |HList' fs as|.
The function executed in the task can now use |fetch'| to fetch all inputs with one function call.

\todo[inline]{What is Length???}

\begin{spec}
task :: (DataStore' fs as, DataStore g b)
  => (HList' fs as -> g b -> IO (g b))
  -> g b
  -> Circuit fs as (Apply fs as) (Q([g])) (Q([b])) (Q([g b])) (Length fs)
\end{spec}

\paragraph{Smart Constructors}
There could be many times that the flexibility provided by defining your own tasks from scratch could cause a large amount of boiler plate code.
For example, there may be times that a user already has pre-defined function and would like to convert it to a task.
Therefore there are also two smart constructors that they are able to use:

\begin{spec}
multiInputTask :: (DataStore' fs as, DataStore g b)
  => (HList as -> b)
  -> g b
  -> Circuit fs as (Apply fs as) (Q([g])) (Q([b])) (Q([g b])) (Length fs)

functionTask :: (DataStore f a, DataStore g b)
  => (a -> b)
  -> g b
  -> Circuit (Q([f])) (Q([a])) (Q([f a])) (Q([g])) (Q([b]) (Q([g b])) N1
\end{spec}

The first allows for a simple function with multiple inputs to be defined.
With the fetching and saving handled by the smart constructor.
The second allows for a simple |a -> b| function to be turned into a |Task|.


\subsection{mapC operator}
Currently a circuit has a static design --- once it has been created it cannot change.
There are times when this could be a flaw in the language.
For example, when there is a dynamic number of inputs.
This could be combated with more smart constructors to generate more complex circuits, with the pre-existing constructors.
Another approach would be to add new constructors that allow for more dynamic circuits, such as |mapC|.
This new constructor is used to map a circuit on a single input containing a list of items.
The input is fed into the inner circuit, accumulated back into a list, and then output.

\begin{spec}
mapC :: (DataStore' (Q([f])) (Q([[a]])), DataStore g [b])
  => Circuit (Q([VariableStore])) (Q([a])) (Q([VariableStore a])) (Q([VariableStore])) (Q([b])) (Q([VariableStore b])) N1
  -> g [b]
  -> Circuit (Q([f])) (Q([[a]])) (Q([f [a]])) (Q([g])) (Q([[b]])) (Q([g [b]])) N1
\end{spec}

\todo[inline]{Graphical representation of this?}


\subsection{Completeness}
\todo[inline]{something about the stuff Alex said}
monadic resource theories.


\subsection{Evaluation}

\paragraph{Easy to Build}
A |Circuit| focuses on the transformations that are made to edges on a graph.
This can be beneficial to the user as it is the edges in a dataflow diagram that encode dependencies between tasks.
Although circuits may initially appear complex, there is a relatively simple process that can be used to construct them.
By hand-drawing a dataflow diagram, a circuit can always be constructed that closely mirrors this diagram.
This means that the user can easily visualise what is happening inside a circuit.
In fact it could be possible to pretty print a circuit to recreate this diagram --- although this has not been implemented.

\paragraph{Type-safe}
One key benefit that a |Circuit| brings is that constructing them uses strong types.
Each constructor encodes its behaviour within the types.
This allows the GHC type checker to validate a |Circuit| at compile-time, to ensure that each task is receiving the correct values.
This avoids the possibility of crashes are run-time, where types do not match correctly.
There is, however, a consequence of this type-safety: the user now needs to add some explicit types on a |Circuit| to help the type checker.


\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
