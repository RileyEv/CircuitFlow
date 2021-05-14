%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}


%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, KindSignatures, GADTs, RankNTypes, DataKinds, TypeFamilies, PolyKinds #-}
{-# ANN module "HLint: ignore" #-}
module Language where
import Data.Kind (Type)
\end{code}
}

\chapter{The Language}\label{chap:the-language}

\section{Language Requirements}
For the design of the language to be considered a success, several criteria need to be met:

\begin{itemize}
  \item \textbf{Describe any dataflow} --- The combinators in the language should be able to describe any dataflow that the user needs.
  \item \textbf{Quickness to Learn} --- The language should be quick and easy to learn.
        If it takes users too long to learn, they may never bother, meaning that the language will never be used.
  \item \textbf{Easy to write} --- A user should be able to write programs easily.
        They should not have to spend time creating additional boilerplate code, where it is not necessary.
  \item \textbf{Easy to understand} --- Any program written with this language should be easy to understand,
        so that users can review existing code and know what it will do.
  \item \textbf{Type-safe} --- A feature missing in many dataflow tools is the lack of type checking.
        This causes problems later on in the development process with more debugging and testing needed.
        The language should be type-safe to avoid any run-time errors occurring where types do not match.
\end{itemize}

\section{Tasks}
Dataflow programming focuses on the transforming inputs to outputs. To be able to transform inputs this language will make use of tasks.
They are responsible for reading from an input data source, completing some operation on the input, then finally writing to an output data sink.
Tasks could take many different forms, for example they could be:

\begin{itemize}
  \item A pure function --- a function with type |a -> b|
  \item An external operation --- interacting with some external system. For example, calling a terminal command.
\end{itemize}


A task could have a single input or multiple inputs, however, for now just a single input task will be considered.
Multi-input tasks are explained further in Sub-Section~\ref{sec:multi-input-tasks}

\subsection{Data Stores}\label{sec:data-stores}
Data stores are used to pass values between different tasks, this ensures that the input and output of tasks are closely controlled.
They are used to represent different ways of storing values: one example could be a point to a CSV file.
By also having just one place that defines how to read and write to data stores, it will reduce the possibility of an error occurring and make it easier to test.
A data store can be defined as a type class, with two methods |fetch| and |save|:

\begin{code}
class DataStore f a where
  fetch  :: f a  ->  IO a
  save   :: f a  ->  a -> IO (f a)
\end{code}

A |DataStore| has two type parameters: where |f| is the type of |DataStore| being used and |a| is the type of the value stored inside it.
The aptly named methods describe their intended function: |fetch| will fetch a value from a |DataStore|, and |save| will save a value.
The |fetch| method takes a DataStore as input and will return the value stores inside.
However, the |save| method may not be as self explanatory, since it has an extra |f a| argument.
This argument can be thought of as a pointer to a |DataStore|: it contains the information needed to save.
For example, in the case of a file store it could be the file name.


By implementing this as a type class, there can be many different implementations of a |DataStore|.
The library comes with several pre-defined |DataStore|s, such as a |VariableStore|.
This can be though of as an in memory storage between tasks.

\begin{code}
data VariableStore a = Var a | Empty

instance DataStore VariableStore a where
  fetch (Var x) = return x
  save Empty x = return (Var x)
\end{code}

The |VariableStore| is the most basic example of a |DataStore|, a more complex example is a |FileStore|,
which represents a pointer to a file:

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
The type of a task details the inputs and outputs.
A task is created via a constructor that takes two arguments: the function it represents and somewhere to store the output.
This constructor makes use of GADTs syntax~\cite{10.1145/1160074.1159811} so that constraints can be placed on the types used.
It enforces that a |DataStore| must exist for the input and output types.
This allows the task to make use of the |fetch| and |save| functions.

\begin{code}
data Task (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Task :: (DataStore f a, DataStore g b) => (f a -> g b -> IO (g b)) -> g b -> Task f a g b
\end{code}

When a |Task| is executed the stored function is executed, with the input being passed in as the first argument and the output ``pointer'' as the second argument.
This returns an output |DataStore| that can be passed on to another |Task|.



\section{Chains, A Dalliance}\label{sec:lang-chains}
In a dataflow programming, one of the key aspects is the definition of dependencies between tasks in the flow.
One possible approach to encoding this concept in the language, that was ultimately not up to scratch, is to make use of sequences of tasks --- also referred to as chains.
These chains compose tasks, based on their dependencies. A chain can be modelled with an abstract datatype:

\begin{code}
data Chain (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Chain  ::  Task   f  a  g  b  ->  Chain  f  a  g  b
  Then   ::  Chain  f  a  g  b  ->  Chain  g  b  h  c -> Chain f a h c
\end{code}

%if style /= newcode
%format >>> = ">\!\!>\!\!>"
%endif

The |Chain| constructor wraps a |Task| in the |Chain| data type. This allows for them to be easily composed with other |Tasks|.
Without this there would need to be an ``empty'' element, however, this could lead to the construction of a chain with no tasks.
The |Then| constructor combines multiple chains to form a sequence of sequential tasks.
To make this easier to use an operator |>>>| is defined that represents |Then|:


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
This means that existential types will need to be used.

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

However, there is a problem with this approach: to be able to form a network similar to that shown in Figure~\ref{fig:pipe-dataflow-example},
the language will need to know where to join two |Chain|s together.
However, with the current definition of a Task, it is not possible to easily check the equivalence of two functions.
Being able to check for equivalence will be key to defining a method to merge these chains together:
it will need to match |task2| in the first chain with the |task2| in the second chain.
Similarly, if a user wanted to use the same task multiple times, it would not be possible to differentiate between them.
One approach to this would be to have unique identifiers for each task, such as PIDs.

\paragraph{\acfp{PID}}
A Chain can be modified so that instead of storing a |Task| it instead stores a |PID| --- a unique identifier for a task.
However to do this a new |PID| data type is needed:

%if style /= newcode
%format Chain'
%format Then'
%endif

\begin{code}
data PID (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  PID :: Int -> PID f a g b
\end{code}

The |PID| data type has the same kind as a |Task| and makes use of phantom type parameters, to retain the same type information as a |Task|,
whilst storing just an |Int| that can be used to identify it.

\begin{code}
data Chain' (f :: Type -> Type) (a :: Type) (g :: Type -> Type) (b :: Type) where
  Chain'  ::  PID     f  a  g  b  ->  Chain'  f  a  g  b
  Then'   ::  Chain'  f  a  g  b  ->  Chain'  g  b  h  c -> Chain' f a h c
\end{code}

This new version of |Chain| named |Chain'|, is almost identical in the way it behaves, however instead of storing a |Task|, it stores a |PID|.

This, however, leaves a key question, how do |Task|s get mapped to |PID|s.
This can be done by employing the State monad.
The state stores a map from PID to task and a counter for PIDs.
As |Task|s each have a different type a new wrapper datatype is required, making use of existential types, to close over the types, and produce values of apparently the same type.
This is because a |Map| can only store one type.
The |Workflow| monad is a type alias for the |State| monad, which stores the |WorkflowState|.

\begin{code}
data TaskWrap = forall f a g b. TaskWrap (Task f a g b)

data WorkflowState = WorkflowState {
  pidCounter :: Int,
  tasks :: M.Map Int TaskWrap
}

type Workflow = State WorkflowState
\end{code}

An operation that is defined for the monad is |registerTask|.
This takes a |Task| and returns a |Chain| that stores a |PID| inside it.
Whenever a user would like to add a new task to the workflow, they register it.
They can then use this returned value to construct multiple chains, which can now be joined easily by comparing the stored \ac{PID}.

\begin{spec}
registerTask :: Task f a g b -> Workflow (Chain' f a g b)
\end{spec}

One benefit to this approach is that if the user would like to use a task again in a different place,
they can simply register it again and use the new PID value.

\subsection{Evaluation}
\paragraph{$\text{\rlap{{\scriptsize\textonehalf}}}\square$ Describe any Dataflow}
This method would be capable of describing any dataflow, although in its current state, it can only support trees.
The algorithm that would be used to join different chains together could be developed to allow them to rejoin onto another chain.
This will allow for any \ac{DAG} to be defined.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Quickness to Learn}
With only 4 different combinators (|Chain|, |>>>|, |Pipe|, |And|) and the |Task| constructor, this language should be simple to learn.
There are only two main concepts the user needs to understand: how to join tasks into a chain and how to join chains together.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Easy to Write}
Building chains is a very simple process, and allows the user to focus on the dependencies of one task.
They do not need to be aware of the bigger picture.
For example, adding a new |task5| that depends on |task2| in Figure~\ref{fig:pipe-dataflow-example}.
The user will just need to add one extra chain |task5 >>> task2|, which will have no effect on the existing chains.

\paragraph{$\text{\rlap{$\times$}}\square$ Easy to Read}
Although it is easy to write chains, this could lead to messy definitions with no structure.
This will make it harder for a user to interpret an already defined collection of chains.
Its possible that they will need to draw out some of the dependencies to further understand what is already defined.

\paragraph{$\text{\rlap{$\times$}}\square$ Type-safe}
Although a |Chain| can be well typed, the use of existential types to join chains together pose a problem.
This causes the types to be `hidden', this means that when executing these tasks, the types need to be recovered.
This is possible through the use of |gcast| from the |Data.Typeable| library.
However, this has to perform a reflection at run-time to compare the types.
There is the possibility that the types could not matching and this would only be discovered at run-time.
There is a mechanism to handle the failed match case, however, this does not fulfil the criteria of being fully type-safe.

\paragraph{Chains are not good enough}
Chains do not satisfy some of the key requirements that this library set out to solve, such as type safety.
This could lead to crashes at run-time as the compiler is not able to fully verify that the system type-checks.
Another approach could be to look to category theory, for ways to compose functions together in a type-safe way.


\section{Solution: Circuit}
This approach is inspired by monadic resource theory, which has a collection of mathematical operators for composing functions together.
It uses parallel prefix circuits, described by Hinze~\cite{scans}, as a starting point for the design of the combinators.
A set of constructors can be defined that are used to represent a \ac{DAG}, which are similar to the wiring diagrams in Section~\ref{sec:bg-wiring-diagrams}.
The constructors seen in Figure~\ref{fig:circuit-constructors} represent the behaviour of edges in the graph.

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
\input{diagrams/circuit-constructors/dropL}
\caption{|dropL|}
\end{subfigure}
\begin{subfigure}{0.4\textwidth}
\centering
\input{diagrams/circuit-constructors/dropR}
\caption{|dropR|}
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

\subsection{Constructors}\label{sec:lang-circuit-constructors}
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
\item |inputsStorageTypes| is a type-list of storage types, for example |(Q([VariableStore, CSVStore]))| --- these all have kind |* -> *|.
\item |inputsTypes| is a type-list of the types stored in the storage, for example |(Q([Int, [(String, Float)]]))|.
\item |inputsApplied| is a type-list of the storage types applied to the types stored, for example\\ |(Q([VariableStore Int, CSVStore [(String, Float)]]))|.
\item |outputsStorageTypes|, |outputsTypes| and |outputsApplied| mirror the examples above, but for the outputs instead.
\item |nInputs| is a type-level Nat that is the length of the input lists.
\end{itemize}

Although, to a human some of these types may seem irrelevant, GHC is not able to make all of the deductions itself, when type checking the code.
It requires additional information, such as |inputsApplied| or |nInputs|.

In the language there are two different types of constructor, those that recurse and those that can be considered leaf nodes.
The behaviour of both types of constructor is recorded within the types.
For example, the |id| constructor has the type:

\begin{spec}
id :: DataStore' (Q([f])) (Q([a])) => Circuit (Q([f])) (Q([a])) (Q([f a])) (Q([f]) (Q([a])) (Q([f a])) N1
\end{spec}

It can be seen how the type information for this constructor states that it has 1 input value (|N1 ~ (Q(Succ)) (Q(Zero))|) of type |f a| and it returns that same value.
Each type parameter in |id| is a phantom type~\cite{phantom_types}, since there are no values stored in the data type that use the type parameters.
The rest of the constructors that are leaf nodes are: |replicate|, |swap|, |dropL|, and |dropR|:

\begin{spec}
replicate  :: DataStore'  (Q([f]))     (Q([a]))     => Circuit  (Q([f]))     (Q([a]))     (Q([f a]))       (Q([f,  f]))  (Q([a, a]))  (Q([f  a,  f a]))  N1
swap       :: DataStore'  (Q([f, g]))  (Q([a, b]))  => Circuit  (Q([f, g]))  (Q([a, b]))  (Q([f a, g b]))  (Q([g,  f]))  (Q([b, a]))  (Q([g  b,  f a]))  N2
dropL      :: DataStore'  (Q([f, g]))  (Q([a, b]))  => Circuit  (Q([f, g]))  (Q([a, b]))  (Q([f a, g b]))  (Q([g]))      (Q([b]))     (Q([g  b]))        N2
dropR      :: DataStore'  (Q([f, g]))  (Q([a, b]))  => Circuit  (Q([f, g]))  (Q([a, b]))  (Q([f a, g b]))  (Q([f]))      (Q([a]))     (Q([f  a]))        N2
\end{spec}

The |replicate| constructor states that a single input value of type |f a| should be input, and that value should then be duplicated and output.
The |swap| constructor takes two values as input: |f a| and |g b|. It will then swap these values over, such that the output will now be: |g b| and |f a|.
|dropL| will take two inputs: |f a| and |g b|. It will then drop the left argument and return just a |g b|.
The |dropR| has the same behaviour as |dropL|, it just drops the right argument instead.

To be able to make use of the leaf nodes, they need to be combined in some way.
To do this two new recursive constructors named `beside' and `then' will be used.
However, before defining these constructors there are some tools that are required.
This is due to the types no longer being concrete.
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
This type family pairwise applies a list of types with kind |* -> *| to a list of types with kind |*| to form a new list containing types of kind |*|.
For example, |Apply (Q([f, g, h])) (Q([a, b, c])) ~ (Q([f a, g b, h c]))|.


%format :+ = ":\!\!+"

\begin{spec}
type family Apply (fs :: [Type -> Type]) (as :: [Type]) where
  Apply  (Q([]))         (Q([]))         =  (Q([]))
  Apply  (f (SQ(:)) fs)  (a (SQ(:)) as)  =  f a (SQ(:)) Apply fs as
\end{spec}


\paragraph{Append Type Family}
There will also be the need to append two type level lists together.
Lists would need to be appended in this way, when combining inputs and outputs to form a larger circuit.
For example, |(Q([a, b, c])) :++ (Q([d, e, f])) ~ (Q([a, b, c, d, e, f]))|.
To do this an append type family~\cite{10.1145/1017472.1017488} can be used:

%format l'

\begin{spec}
type family (:++) (l1 :: [k]) (l2 :: [k]) :: [k] where
  (:++)  (Q([]))        l   = l
  (:++)  (e (SQ(:)) l)  l'  = e (SQ(:)) (l :++ l')
\end{spec}

The |:++| type family is defined in the same way as the standard |++| function on value lists, however, it appends type lists together instead.
This type family makes use of the language extension |PolyKinds|~\cite{10.1145/2103786.2103795} to allow for the append to be polymorphic on the kind stored in the type list.
This will avoid defining multiple versions to append |fs| with |gs|, and |as| with |bs|.

\paragraph{The `Then' Constructor}
This constructor --- denoted by |<->| --- is used to stack two circuits on top of each other.
It is used to encapsulate the idea of dependencies, between different circuits.
Through types it enforces that the output of the top circuit is the same as the input to the bottom circuit.

\begin{spec}
(<->) :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
  => Circuit  fs  as  (Apply  fs  as)  gs  bs  (Apply  gs  bs)  nfs
  -> Circuit  gs  bs  (Apply  gs  bs)  hs  cs  (Apply  hs  cs)  ngs
  -> Circuit  fs  as  (Apply  fs  as)  hs  cs  (Apply  hs  cs)  nfs
\end{spec}

It employs a similar logic to function composition |(.) :: (a -> b) -> (b -> c) -> (a -> c)|.
The resulting type from this constructor uses the input types from the first argument |fs as (Apply fs as)|,
and the output types from the second argument |hs cs (Apply hs cs)|.
It then forces the constrain that the output type of the first argument and the input type of the second are the same --- |gs bs (Apply gs bs)|.


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

This constructor works by making use of the |:++| type family to append the input and output type list of the left constructor to those of the right constructor.
It also makes use of the |:+| type family --- defined in Section~\ref{sec:bg-type-families} --- to add the number of inputs from the left and right together.


%% The |<>| constructor combined with the |id| constructor can be considered a Monoid at the behaviour level.
%% However, due to the types on the constructors it is not possible to define a Monoid instance for |<>| in Haskell.
%% \todo[inline]{Give example on the types as to why it isnt.}
%% \todo[inline]{I think its a monoid in cat theory, but it isn't one in haskell. empty exists with id, but this would not conform to the types.}

\subsection{Combined Data Stores}

%format HList'
%format fetch'
%format save'

A keen eyed reader may notice that all of these constructors have not been using the original |DataStore| type class.
Instead they have all used the |DataStore'| type class.
This is a special case of a |DataStore|, allowing for constructors to also be defined over type lists, not just a single type.
Combined data stores make it easier for tasks to fetch from multiple inputs.
Users will just have to call a single |fetch'| function, rather than multiple.
However, since tasks can only have one output, there is no need for a |save'| function, that would be able to save to multiple data stores.

To be able to define |DataStore'|, heterogeneous lists~\cite{10.1145/1017472.1017488} are needed --- specifically two different forms.
|HList'| stores values of type |f a| and is parameterised by two type lists |fs| and |as|.
|IOList| stores items of type |IO a| and is parameterised by a type list |as|.
Using an |IOList| makes it easier to define a function that produces a list of IO computations.
Their definitions are:

%format HCons'
%format HNil'

\begin{spec}
data HList' (fs :: [Type -> Type]) (as :: [Type]) where
  HCons'  :: f a -> HList' fs as -> HList' (f (SQ(:)) fs) (a (SQ(:)) as)
  HNil'   :: HList' (Q([])) (Q([]))

data IOList (xs :: [Type]) where
  IOCons  :: IO x -> IOList xs -> IOList (x (SQ(:)) xs)
  IONil   :: IOList (Q([]))
\end{spec}

Now that there is a mechanism to represent a list of different types, it is possible to define |DataStore'|:


\begin{spec}
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  fetch'  :: HList' fs as ->  IOList  as
\end{spec}

To save the user of the cumbersome task of having to define an instance of |DataStore'| for every possible combination of data stores,
the instance is derived from the previous |DataStore| type class.
This means that a user does not need to create any instances of |DataStore'|.
They can instead focus on each single case, with the knowledge that they will automatically be able to combine them with other data stores.

\begin{spec}
instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' (Q([f])) (Q([a])) where
  fetch'  (HCons' x  HNil')  = IOCons  (fetch x)  IONil

instance (DataStore f a, DataStore' fs as) => DataStore' (f (SQ(:)) fs) (a (SQ(:)) as)  where
  fetch'  (HCons' x  xs)     = IOCons  (fetch x)  (fetch' xs)
\end{spec}

One of these instances makes use of the |{-# OVERLAPPING #-}| pragma.
In most cases the base case instance that would be defined is |DataStore' (Q([])) (Q([]))|.
However, it does not makes sense to have an empty data store.
Therefore, the base case is selected to be a list with one element |DataStore' (Q([f])) (Q([a]))|.
This leads to a problem: GHC is unable to decide which instance to use.
It could use either the |DataStore' (Q([f])) (Q([a]))| or the |DataStore' (f (SQ(:)) (Q([]))) (a (SQ(:)) (Q([])))| instance.
The overlapping pragma tells GHC, that if it encounters this scenario, it should choose the one with the pragma.



\subsection{Multi-Input Tasks}\label{sec:multi-input-tasks}
With a |Circuit|, it is possible to represent a \ac{DAG}.
This means that a node in the graph can now have multiple dependencies, as seen in Figure~\ref{fig:multi-depen-task}.

\begin{figure}[ht]
\centering
\input{diagrams/circuit-constructors/multi-input}
\caption{A graphical representation of a task with multiple dependencies}
\label{fig:multi-depen-task}
\end{figure}

To support this, a modification is made to the |task| constructor: rather than have an input value type of |f a|, it can now have an input value type of |HList' fs as|.
The function executed in the task can now use |fetch'| to fetch all inputs with one function call.

\begin{spec}
task :: (DataStore' fs as, DataStore g b)
  => (HList' fs as -> g b -> IO (g b))
  -> g b
  -> Circuit fs as (Apply fs as) (Q([g])) (Q([b])) (Q([g b])) (Length fs)
\end{spec}

Now that the length of the inputs is unknown, in order to specify the |nInputs| type parameter, the |Length| type family defined in Section~\ref{sec:bg-heterogeneous-lists} must be used. This will return a type-level |Nat|, which is the length of the input array |fs|.

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
  => Circuit  (Q([VariableStore]))  (Q([a]))    (Q([VariableStore a]))  (Q([VariableStore]))  (Q([b]))    (Q([VariableStore b]))  N1
  -> g [b]
  -> Circuit  (Q([f]))              (Q([[a]]))  (Q([f [a]]))            (Q([g]))              (Q([[b]]))  (Q([g [b]]))            N1
\end{spec}

This example can be though of a production line (|[a] -> [b]|).
The circuit given as an argument describes how to produce one item (|a -> b|).
The |mapC| can then be provided with a pallet (or a list) of resources to build multiple items (|[a]|), it will then return a pallet of made items (|[b]|).


\subsection{Completeness}
The constructors in this library make up a symmetric monoidal preorder.
For simplicity only the |inputsApplied| and |outputsApplied| type parameters will be used to formalise a |Circuit| --- all other type parameters are only required to aid GHC in compilation.

A preorder is defined over tasks and |DataStore|s.
The preorder relation $\le$, can be used to describe the dependencies in the |DataStore|s, with a task being able to transform |DataStore|s into new |DataStore|s.
The relation is defined over the set $X$, which describes the set of all possible |DataStore|s.

The monoidal product $\otimes$ can be thought of as the concatenation of multiple |DataStore|s into type-lists.
For example the monoidal product of $(f\:a) \otimes (g\:b) = $ |(Q([f a])) :++ (Q([g b])) ~ (Q([f a, g b]))|.
The monoidal unit, is tricky to define as it has no real meaning within a |Circuit|, however it could be considered the empty |DataStore|: |(Q([]))|.

%format x1
%format y1
%format x2
%format y2

The axioms are then satisfied as follows:
\begin{enumerate}
  \item Reflexivity --- this is the |id :: Circuit (Q([f a])) (Q([f a]))| constructor, it represents a straight line with the same input and output.
  \item Transitivity --- this is the |<-> :: Circuit x y -> Circuit y z -> Circuit x z| constructor, it allows for circuits to be placed in sequence.
  \item Monotonicity --- this is the |<> :: Circuit x1 y1 -> Circuit x2 y2 -> Circuit (x1 :++ x2) (y1 :++ y2)| constructor. This can place circuits next to each other.
  \item Unitality --- given the monoidal unit |(Q([]))| and a |DataStore| |xs|, then the rules hold true: |(Q([])) :++ xs ~ xs| and |xs :++ (Q([])) ~ xs|.
  \item Associativity --- given three |DataStore|s: |xs|, |ys|, |zs|. Since concatenation of lists is associative then this rule holds: |(xs :++ ys) :++ zs ~ xs :++ (ys :++ zs)|.
  \item Symmetry --- this is the |swap :: Circuit (Q([f a, g b])) (Q([g b, f a]))| constructor, it allows for values to swap over.
  \item Delete Axiom --- this is satisfied by the |dropL :: Circuit (Q([f a, g b])) (Q([g b]))| and |dropR :: Circuit (Q([f a, g b])) (Q([f a]))|.
    Although this does not directly fit with the axiom, it also has to ensure the constraint on a circuit that there must always be 1 output value.
  \item Copy Axiom --- this is the |replicate :: Circuit (Q([f a])) (Q([f a, f a]))| constructor. It allows for a |DataStore| to be duplicated.
\end{enumerate}

By satisfying all the axioms a |Circuit| is a symmetric monoidal preorder.


\subsection{Evaluation}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Describe any Dataflow}
It is possible to represent a \ac{DAG} with the constructors in this library. Its completeness has been formalised as a symmetric monoidal preorder.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Quickness to Learn}
The language uses, combinators that also have a visual representation, this makes it easy to quickly understand how they all work.
A user can then also benefit from the familiarity of using the host language Haskell.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Easy to Write}
Although a circuit may appear hard to construct initially, the skills of the domain expert also need to be considered.
To be able to define a circuit the user needs to have an understanding of the shape of the dataflow diagram: a skill the domain expert is expected to have.
Once the user has a sketch for the dataflow they would like to create, translating to a circuit is a relatively simple job.
This creates more upfront work for the user, however, it is offset by the additional benefits that a circuit brings.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Easy to Read}
Due to the graphical nature of the constructors, it is relatively simple to build up a picture of how an existing circuit works.
The user is able to infer the shape of the dataflow diagram easily by working their way down a circuit from top to bottom,
and visualising how different tasks are connected.
It could also be possible to pretty print a circuit to recreate the dataflow diagram --- although this has not been implemented.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Type-safe}
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
