%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\long\def\ignore#1{}

\begin{document}


\chapter{Implementation}\label{chap:implementation}

\section{Requirements}
The implementation of the network itself has several requirements that are separate from the language design:

\begin{itemize}
  \item \textbf{Type-safe} --- This is a continuation of the previous requirement for the language. It is also important that once the user has built a well-typed |Circuit|, that the code also continues to be executed in a well-typed environment, to ensure that all inputs and outputs are correctly typed.
  \item \textbf{Parallel} --- One of the key benefits that comes from dataflow programming is implicit parallelisation.
        With this \ac{DSL} being tailored towards data pipelines, which could be computationally expensive,
        it should be able to benefit from parallel execution.
  \item \textbf{Competitive Speed} --- This library should be able to execute dataflows in a competitive time, with other libraries that already exist.
  \item \textbf{Failure Tolerance} --- It is important that if one invocation of a task crashes, it does not crash the whole program.
        This implementation should be able to gracefully handle errors and propagate them through the circuit.
  \item \textbf{Usable} --- The implementation of the library should not break any of the usability of the language design.
  \item \textbf{Maintainable} --- It should be easy to maintain the library and add new constructors in the future.
\end{itemize}

\section{Circuit AST}
The constructors for the language are actually \textit{smart constructors}.
They provide a more elegant way to build an \ac{AST}, which represents the circuit.
They give the ability to gain the benefits of extensibility and modularity, usually found in a shallow embedding, while still having a fixed core \ac{AST} that can be used for interpretation.

%format IFunctor7

\subsection{IFunctor}
To build the fixed core \ac{AST} for a |Circuit|, indexed functors --- also known as |IFunctor| --- are used.
|IFunctor|s are, however, only defined to take a single type index --- a |Circuit| needs 7.
To do this |IFunctor7| can be defined:

%format f'
%format g'
%format imap7
%format IFix7
%format IIn7

\noindent\begin{minipage}{\linewidth}
\begin{code}
class IFunctor7 iF where
  imap7 :: (forall a b c d e f g. f' a b c d e f g -> g' a b c d e f g) -> iF f' a b c d e f g -> iF g' a b c d e f g
\end{code}
\end{minipage}

|IFunctor7| follows a similar structure to a standard |IFunctor|, it just has 7 type indicies instead.
This indexed functor can be used to mark the recursive points of the data types used to construct the |Circuit| \ac{AST}.

\noindent\begin{minipage}{\linewidth}
However, any data type that is converted to use an |IFunctor7|, will need a way to tie the recursive knot.
A new type called |IFix7| can be used, it will follow a similar pattern to |IFix|, but with 7 type indicies.

\begin{code}
newtype IFix7 iF a b c d e f g = IIn7 (iF (IFix7 iF) a b c d e f g)
\end{code}
\end{minipage}


\subsection{Indexed Data Types \`{a} la Carte}
To build the \ac{AST}, the data types \`{a} la carte~\cite{swierstra_2008} approach is taken.
This allows for a modular approach, making the library more extendable later on.
To be able to use this approach, it needs to be modified to support the 7 type indicies --- |a| through to |g|:

\noindent\begin{minipage}{\linewidth}
\begin{code}
data (iF :+: iG)  (f' :: i -> j -> k -> l -> m -> n -> o -> Type)
                  (a :: i) (b :: j) (c :: k) (d :: l) (e :: m) (f :: n) (g :: o) where
  L :: iF  f' a b c d e f g -> (iF :+: iG) f' a b c d e f g
  R :: iG  f' a b c d e f g -> (iF :+: iG) f' a b c d e f g

infixr :+:
\end{code}
\end{minipage}

As mentioned in Section~\ref{sec:bg-dtalacarte}, using the |:+:| operator comes with problem of many |L|'s and |R|'s,
when trying to create the \ac{AST} structure.
To avoid this, the |:<:| operator can also be extended to work with |IFunctor7|.
The definition follows closely to the original definition, with some minor modifications to the types and constraints.

\noindent\begin{minipage}{\linewidth}
\begin{code}
class (IFunctor7 iF, IFunctor7 iG) => iF :<: iG where
  inj :: iF f' a b c d e f g -> iG f' a b c d e f g

instance IFunctor7 iF => iF :<: iF where
  inj = id

instance (IFunctor7 iF, IFunctor7 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor7 iF, IFunctor7 iG, IFunctor7 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
\end{code}
\end{minipage}

\paragraph{Defining a constructor}
Data types for each constructor can be defined individually.
The |Then| constructor is used as an example, however, the process can be applied to all constructors in the language.

\noindent\begin{minipage}{\linewidth}
\begin{code}
data Then  (iF  ::   [Type -> Type] -> [Type] -> [Type]
                ->   [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
           (inputsS   :: [Type -> Type])  (inputsT   :: [Type])  (inputsA   :: [Type])
           (outputsS  :: [Type -> Type])  (outputsT  :: [Type])  (outputsA  :: [Type])
           (ninputs :: Nat) where
  Then :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
    =>  iF  fs  as  (Apply  fs  as)  gs  bs (Apply  gs  bs)  nfs
    ->  iF  gs  bs  (Apply  gs  bs)  hs  cs (Apply  hs  cs)  ngs
    ->  Then iF fs as (Apply fs as) hs cs (Apply hs cs) nfs
\end{code}
\end{minipage}

Each |iF| denotes the recursive points in the data type, with the subsequent type arguments mirroring those seen in Section~\ref{sec:lang-circuit-constructors}.
A corresponding |IFunctor7| instance formalises the points of recursion, by showing how to transform the structure inside it.

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance IFunctor7 Then where
  imap7 f (Then x y) = Then (f x) (f y)
\end{code}
\end{minipage}

The smart constructor, that injects the |L|'s and |R|'s automatically can be defined for |Then| as:

\noindent\begin{minipage}{\linewidth}
\begin{code}
(<->) :: (Then :<: iF, DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
  =>  IFix7 iF fs  as  (Apply fs  as)  gs  bs  (Apply gs  bs)  nfs
  ->  IFix7 iF gs  bs  (Apply gs  bs)  hs  cs  (Apply hs  cs)  nhs
  ->  IFix7 iF fs  as  (Apply fs  as)  hs  cs  (Apply hs  cs)  nfs
(<->) l r = IIn7 (inj (Then l r))
infixr 4 <->
\end{code}
\end{minipage}

The constructor adds one extra constraint, to the constructor defined in Section~\ref{sec:lang-circuit-constructors} --- |Then :<: iF|.
This allows the smart constructor to produce an node in the \ac{AST} for any sum of data types, that includes the |Then| data type.


\paragraph{Representing a Circuit}
Once each constructor has been defined then they can be combined together to form the |CircuitF| type, which can be used to represent a circuit.
\begin{code}
type CircuitF = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: Task :+: Map
\end{code}

The fixed-point of the |CircuitF| datatype can be defined with |IFix7|:

\begin{code}
type Circuit = IFix7 CircuitF
\end{code}


\section{Process Network}
Now that it is possible to build a |Circuit|, which can be considered a specification for how to execute a set of tasks, there needs to be a mechanism in place to execute the specification.
The standard implementation of a process network will use a \acf{KPN}.
This means that each task in a circuit will run on its own separate thread, with inputs being passed between them on unbounded channels.

\subsection{Network Typeclass}
To allow for different process networks, a typeclass will be used to specify all the functions that every network should have.
The |Network| typeclass is defined as:

\noindent\begin{minipage}{\linewidth}
\begin{code}
class Network n where
  startNetwork  ::  Circuit  inputsS inputsT inputsA outputsS outputsT outputsA nInputs
                ->  IO (n    inputsS inputsT inputsA outputsS outputsT outputsA)

  stopNetwork   ::  n inputsS inputsT inputsA outputsS outputsT outputsA
                ->  IO ()

  write  ::  HList' inputsS inputsT
         ->  n inputsS inputsT inputsA outputsS outputsT outputsA
         ->  IO ()

  read   ::  n inputsS inputsT inputsA outputsS outputsT outputsA
         ->  IO (HList' outputsS outputsT)
\end{code}
\end{minipage}

This type class requires that a network has 4 different functions:
\begin{itemize}
  \item |startNetwork| is responsible for converting the circuit into the underlying representation for a process network:
        it will be discussed in more detail in Section~\ref{sec:circuit-translation}.
  \item |stopNetwork| is for cleaning up the network after it is no longer needed. For example, this could be stopping the threads running.
        This could be particularly important if embedding a circuit into a larger program, where unused threads could be left hanging.
  \item |write| should take some input values and add them into the network, so that they can be processed.
  \item |read| should retrieve some output values from the network.
\end{itemize}

Examples of how to use a network are included in Chapter~\ref{chap:examples}.

\subsection{The Basic Network Representation}
An implementation of the Network typeclass is a |BasicNetwork|.
This implementation makes use of a special case of heterogeneous list.
A |PipeList| is used to represent a heterogeneous list of channels.
This allows the |BasicNetwork| to store multiple channels in the same list with out the need for existential types --- one of the problems previously encountered with chains in Section~\ref{sec:lang-chains}.

\noindent\begin{minipage}{\linewidth}
\begin{code}
data PipeList (fs :: [Type -> Type]) (as :: [Type]) (xs :: [Type]) where
  PipeCons  :: Chan (f a) -> PipeList fs as xs -> PipeList (f (Q(:)) fs) (a (Q(:)) as) (f a (Q(:)) xs)
  PipeNil   :: PipeList (Q([])) (Q([])) (Q([]))
\end{code}
\end{minipage}

Making use of |PipeList|s, the |BasicNetwork| data type can be defined.
This definition makes use of record syntax, this allows for named fields, with accessors automatically generated.

\noindent\begin{minipage}{\linewidth}
\begin{code}
data BasicNetwork  (inputsS   :: [Type -> Type]) (inputsT   :: [Type])  (inputsA   :: [Type])
                   (outputsS  :: [Type -> Type]) (outputsT  :: [Type])  (outputsA  :: [Type]) where
  BasicNetwork :: {
    threads  :: [ThreadId],
    inputs   :: PipeList  inputsS   inputsT   inputsA,
    outputs  :: PipeList  outputsS  outputsT  outputsA }
    -> BasicNetwork inputsS inputsT inputsA outputsS outputsT outputsA
\end{code}
\end{minipage}

The |BasicNetwork| has three fields:

\begin{itemize}
  \item |threads| is a list of |ThreadId|s, this allows for the threads to be managed after their creation.
  \item |inputs| is a |PipeList| containing the channels that the initial input into the network.
  \item |outputs| is a |PipeList| that stores channels, which the output of the network can be read from.
\end{itemize}

The |Network| type instance for a |BasicNetwork| is relatively trivial to implement: if given a function to transform a |Circuit| to it.

%format forM_ = forM"\_"

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance Network BasicNetwork where
  startNetwork      = buildBasicNetwork -- Definition to come...
  stopNetwork    n  = forM_ (threads n) killThread
  write uuid xs  n  = writePipes xs (inputs n)
  read  n           = readPipes (outputs n)
\end{code}
\end{minipage}

The |writePipes| function will input a list of values into each of the respective pipes.
The |readPipes| function will make a blocking call to each channel to read an output from it.
This function will block till an output is read from every output channel.

% \todo[inline]{add a definition of these}

\section{Translation to a Network}\label{sec:circuit-translation}

%format icata7
%format icataM7

There is now a representation for a |Circuit| that the user will build, and a representation used to execute the |Circuit|.
However, there is no mechanism to convert between them.
This can be achieved by folding the circuit data type into a network.
This fold, however, will need to create threads and channels, both of which |IO| actions.
The current definition for the fold |icata7| is not able perform monadic computation inside the algebra.
To solve this |unsafePerformIO| could be used, however, for this to be safe the |IO| computation needs to have no side-effects.
This fold will violate this rule, therefore, the only other way to support this is to modify the catamorphism to support monadic computation.

\subsection{Indexed Monadic Catamorphism}

%format imapM7

An indexed monadic catamorphism, found in Section~\ref{sec:bg-monadic-cat}, can be used to perform this fold: it will allow for monadic computation within the algebra.
However, |icataM| needs to be modified to support the 7 type indicies needed.
The first step is to define a monadic |imap| that supports the needed number of type indicies.
This will be added by extending the |IFunctor7| instance to also include a function named |imapM7|.


\noindent\begin{minipage}{\linewidth}
\begin{code}
class IFunctor7 iF where
  imap7 :: (forall a b c d e f g. f' a b c d e f g -> g' a b c d e f g) -> iF f' a b c d e f g -> iF g' a b c d e f g
  imapM7 :: Monad m  => (forall a b c d e f g. f' a b c d e f g -> m (g' a b c d e f g))
                     -> iF f' a b c d e f g
                     -> m (iF g' a b c d e f g)
\end{code}
\end{minipage}

The definition for this new function |imapM7| for each instance closely follows the non-monadic version, however,
now has a monadic function to map on the input.
Here is the definition of the new |IFunctor7| instance for |Beside|:

%format l'
%format r'

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance IFunctor7 Beside where
  imap7   f (Beside l r)  = Beside (f l) (f r)
  imapM7  f (Beside l r)  = do
    l'  <- f l
    r'  <- f r
    return (Beside l' r')
\end{code}
\end{minipage}

The definition is intuitively the same, just using do-notation instead.

Now that there is a indexed monadic map, it is possible to define the a monadic catamorphism for an |IFunctor7|:

\noindent\begin{minipage}{\linewidth}
\begin{code}
icataM7 :: (IFunctor7 iF, Monad m)
  => (forall a b c d e f g . iF f' a b c d e f g -> m (f' a b c d e f g))
  -> IFix7 iF a b c d e f g
  -> m (f' a b c d e f g)
icataM7 algM (IIn7 x) = algM =<< imapM7 (icataM7 algM) x
\end{code}
\end{minipage}

|icataM7| is almost identical to |icataM|, however, it makes use of |imapM7| instead of |imapM|.

\subsection{BuildNetworkAlg}
To use |icataM7| to fold a |Circuit| into a |BasicNetwork|, an algebra is required.
However, a standard algebra will not be able to complete this transformation.
Consider this example |Circuit| with two tasks executed in sequence.


\noindent\begin{minipage}{\linewidth}
\begin{code}
example =  task1
           <->
           task2
\end{code}
\end{minipage}

In a standard algebra both sides of the |Then| constructor would be evaluated independently.
In this case it would produce two disjoint networks, both with their own input and output channels.
The algebra for |Then|, would then need to join the output channels of task1 with the input channels of task2.
However, it is not possible to join channels together.
Instead, the output channels from task1 need to be accessible when creating task2.
This is referred to as a \textit{context-sensitive} or \textit{accumulating} fold.
An accumulating fold forms series of nested functions, that collapse to give a final value once the base case has been applied.
A simple example of an accumulating fold could be, implementing |foldl| in terms of |foldr|.

\begin{code}
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f b as = foldr (\a g x -> g (f x a)) id as b
\end{code}

A simple example of |foldl| can be considered.

\begin{spec}
    foldl (+) 0 [1, 2]
==
    (\x->(\x-> id (x+2)) (x+1)) 0
\end{spec}

To be able to have an accumulating fold, inside an indexed catamorphism a carrier data type is required to wrap up this function.
This carrier, which shall be named |AccuN|, contains a function that when given a network that has been accumulated up to that point,
then it is able to produce a network including the next layer in a circuit.
This can be likened to the lambda function given to |foldr|, when defining |foldl|.
The type of the layer being folded will be |Circuit a b c d e f g|.

% \todo{this clear enough? maybe a diagram showing the layer and all the types}

\noindent\begin{minipage}{\linewidth}
\begin{code}
newtype AccuN n asS asT asA a b c d e f g = AccuN
  { unAccuN :: n asS asT asA a b c -> IO (n asS asT asA d e f) }
\end{code}
\end{minipage}

This newtype has 3 additional type parameters at the beginning, namely: |asS|, |asT|, |asA|.
They represent the input types to the initial circuit.
Since the accumulating fold will work layer by layer from the top downwards, these types will remain constant and never change throughout the fold.

\noindent\begin{minipage}{\linewidth}
\paragraph{Classy Algebra}
An algebra type class can now be defined.
This will ensure that the approach remains with modular: a new instance can be made when adding a new constructor to the language.

\begin{code}
class (Network n, IFunctor7 iF) => BuildNetworkAlg n iF where
  buildNetworkAlg  ::  iF (   AccuN n asS asT asA) bsS bsT bsA csS csT csA nbs
                   ->  IO ((  AccuN n asS asT asA) bsS bsT bsA csS csT csA nbs)
\end{code}
\end{minipage}

This algebra type class is parameterised by |n| and |iF|. The |n| is constrained to have a |Network| instance, this allows the same algebra to be used for defining folds for multiple network types. The |iF| is the |IFunctor7| that this instance is being defined for, an example is |Then| or |Id|.
This algebra uses the |N| data type to perform an accumulating fold.
The input to the algebra is an |IFunctor7| with the inner elements containing values of type |AccuN|.
The function can be retrieved from inside |AccuN| to perform steps that are dependent on the previous, for example, in the |Then| constructor.

To be able to define the algebra on sums of |IFunctor7|s, without having a nest of |L|s and |R|s to pattern match, an instance for the sum of two data types is defined:

\begin{code}
instance (BuildNetworkAlg n iF, BuildNetworkAlg n iG) => BuildNetworkAlg n (iF :+: iG) where
  buildNetworkAlg  (L x)  = buildNetworkAlg  x
  buildNetworkAlg  (R y)  = buildNetworkAlg  y
\end{code}

This instance enforces that there must also be an instance for the left and right hand side of the sum.
It will then be able to automatically recurse through the |L|s and |R|s, to get to the types that have been summed.\todo{Sketchy :s}


\paragraph{The Initial Network}
Before being able to define the actual translation, there is one more base to cover.
This is an accumulating fold that depends on the previous layer to be able to define the current one.
However, what happens on the first layer? There is no previous |Network| to use.
The initial network should have matching input and output types, this means that the input channels should be the same as the output channels.

To generate the |PipeList| of channels that will be stored in the initial network a new type class is defined:

\begin{code}
class InitialPipes (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type]) where
  initialPipes :: IO (PipeList inputsS inputsT inputsA)
\end{code}

This type class is able to construct an |initialPipes| based on the type required, in the initial network.
To be able to construct this value two instances are defined:

\begin{code}
instance InitialPipes (Q([])) (Q([])) (Q([])) where
  initialPipes = return PipeNil

instance InitialPipes fs as xs => InitialPipes (f (Q(:)) fs) (a (Q(:)) as) (f a (Q(:)) xs) where
  initialPipes = do
    c <- newChan :: IO (Chan (f a))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as xs))
\end{code}

The first instance deals with the base case: when the type lists are empty, an empty |PipeList| is created.
The later more interesting case deals with a cons in the type lists.
Here a new channel is created with the same type as the type removed from the front of the type list.
The channel is then consed to the front of a |PipeList| with the remaining list generated by a recursive call.

\noindent\begin{minipage}{\linewidth}
Now that there is a method for creating a |PipeList| that matches a type-list, the initial network is defined:

\begin{code}
initialNetwork
  :: forall inputsS inputsT inputsA
   . (InitialPipes inputsS inputsT inputsA)
  => IO (BasicNetwork inputsS inputsT inputsA inputsS inputsT inputsA)
initialNetwork = do
  ps <- initialPipes :: IO (PipeList inputsS inputsT inputsA)
  return $ BasicNetwork [] ps ps
\end{code}
\end{minipage}

This creates a network that has matching input and output types.
To do so a |PipeList| of the initial channels is created, which is then used as both the inputs and outputs of the network.


\subsection{The Translation}
Now that the algebra type class, and the initial input to the accumulating fold is defined, each instance of the type class are defined.

\paragraph{Basic Constructors}
There are several constructors that just manipulate the output |PipeList|, these constructors are |Id|, |Replicate|, |Swap|, |DropL|, and |DropR|.
The |Swap| constructor takes two inputs and then swaps them over:

%format c1
%format c2

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance BuildNetworkAlg BasicNetwork Swap where
  buildNetworkAlg Swap = return $ AccuN
    (\n -> d
      output <- swapOutput (outputs n
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
     swapOutput :: PipeList (Q([f , g])) (Q([a , b])) (Q([f a , g b]))
            -> IO (PipeList (Q([g , f])) (Q([b , a])) (Q([g b , f a])))
     swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) =
        return $ PipeCons c2 (PipeCons c1 PipeNil)
\end{code}
\end{minipage}

The instance for |Swap|, defines a function wrapped by |AccuN|, that takes the current accumulated network, up to this point.
It then is able to transform the outputs and build a new |BasicNetwork|.
To transform the networks it makes use of a function named |swapOutput|, which unpacks the |PipeList| and swaps the two channels |c1| and |c2| over.

Another basic constructor is |Replicate|: the purpose of this constructor is to duplicate the input to produce two outputs.
The instance for |Replicate| is defined as:

%format c'

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance BuildNetworkAlg BasicNetwork Replicate where
  buildNetworkAlg Replicate = return $ AccuN
    (\n -> do
      output <- dupOutput (outputs n)
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
     dupOutput :: PipeList (Q([f])) (Q([a])) (Q([f a]))
           -> IO (PipeList (Q([f , f])) (Q([a , a])) (Q([f a , f a])))
     dupOutput (PipeCons c PipeNil) = do
       c' <- dupChan c
       return $ PipeCons c (PipeCons c' PipeNil)
\end{code}
\end{minipage}

This instance follows a similar pattern to the |Swap| instance --- defining a function which retrieves the accumulated network, then manipulates the outputs.
However, the |dupOutput| function also has to make use of an operation on the channel --- |dupChan|.
This will create a new channel that will mirror the inputs of the original channel.

All other basic constructors will follow this pattern:
\begin{itemize}
  \item |Id|, will return the same outputs as the accumulated network.
  \item |DropL|, will drop the \textit{first} item in the output |PipeList| of the accumulated network.
  \item |DropR|, will drop the \textit{last} item in the output |PipeList| of the accumulated network.
\end{itemize}


\paragraph{Task}
In a |BasicNetwork| a task will run as a separate thread, to do this |forkIO :: IO () -> IO ThreadId| will be used.
Using this function requires some |IO ()| computation to run, this will be defined by |taskExecutor|:

\noindent\begin{minipage}{\linewidth}
\begin{code}
taskExecutor
  :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecutor (Task f outStore) inPipes outPipes = forever
  (do
    taskInput  <- readPipes inPipes
    r           <- f taskInputs outStore
    writePipes (HCons' r HNil') outPipes
  )
\end{code}
\end{minipage}

The |taskExecutor| has three arguements:
\begin{itemize}
  \item The |Task| to be executed on the thread.
  \item A |PipeList| which has channels containing the input values.
  \item A |PipeList| to output the results of the |Task|.
\end{itemize}

A |taskExecutor| will, read a value from each of input channels, execute the task with those inputs, and then write the output to the output channels.
This computation is then repeated forever, using the aptly named function |forever|.

Making use of the |taskExecutor|, the algebra instance for |Task| is defined as:

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance BuildNetworkAlg BasicNetwork Task where
  buildNetworkAlg (Task t out) = return $ AccuN
    (\n -> do
      c <- newChan
      let output = PipeCons c PipeNil
      threadId <- forkIO (taskExecutor (Task t out) (outputs n) output)
      return $ BasicNetwork (threadId : threads n) (inputs n) output
    )
\end{code}
\end{minipage}

This instance first creates a new output channel, this will be given to the task to send its outputs on.
It then forks a new thread with the computation generated by |taskExecutor|.
The executor is given the output values of the accumulated network and the output channel, just created.
The resulting network has the same inputs, but now adds a new thread id to the list and the outputs set to be the output channels from the task.


\paragraph{Then}
The |Then| constructor is responsible for connecting circuits in sequence.
When converting this to a network, this will involve making use of the accumulated network value to generate the next layer.
The instance is defined as:

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance BuildNetworkAlg BasicNetwork Then where
  buildNetworkAlg (Then (AccuN fx) (AccuN fy)) = return $ N
    (\n -> do
      nx <- fx n
      fy nx
    )
\end{code}
\end{minipage}
\ignore{$} % Syntax highlighting is being annoying on my laptop :'(

This instance has an interesting definition: firstly it takes the accumulated network |n| as input.
It then uses the function |fx|, with the input |n| to generate a network for the top half of the |Then| constructor.
Finally, it takes the returned network |nx|, from the top half of the constructor, and generates a network using the function |fy| representing the bottom half of the constructor.

\paragraph{Beside}
The |Beside| constructor places two circuits side by side.
This is the most difficult algebra to define as the accumulated network needs to be split in half to pass to the two recursive sides of |Beside|.
An instance of the algebra is defined as:

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance BuildNetworkAlg BasicNetwork Beside where
  buildNetworkAlg = beside
\end{code}
\end{minipage}

This requires a |beside| function, however to define this function some extra tools are required.
The first is |takeP|, which will take the first |n| elements from a |PipeList|:

\noindent\begin{minipage}{\linewidth}
\begin{code}
takeP :: SNat n -> PipeList fs as xs -> PipeList (Take n fs) (Take n as) (Take n xs)
takeP  SZero      _                =  PipeNil
takeP  (SSucc _)  PipeNil          =  PipeNil
takeP  (SSucc n)  (PipeCons x xs)  =  PipeCons x (takeP n xs)
\end{code}
\end{minipage}

This makes use of the |Take| type family to take |n| elements from each of the type lists: |fs|, |as|, and |xs|.
It follows the same structure as the |take :: Int -> [a] -> [a]| defined in the |Prelude|.

The next function is |dropP|, it drops |n| elements from a |PipeList|:

\noindent\begin{minipage}{\linewidth}
\begin{code}
dropP :: SNat n -> PipeList fs as xs -> PipeList (Drop n fs) (Drop n as) (Drop n xs)
dropP  SZero      l                = l
dropP  (SSucc _)  PipeNil          = PipeNil
dropP  (SSucc n)  (PipeCons _ xs)  = dropP n xs
\end{code}
\end{minipage}

This function again follows the same structure as |drop :: Int -> [a] -> [a]| defined in the |Prelude|.
Both |takeP| and |dropP| are used to split the outputs of a network after |n| elements.
This requires the knowledge of what |n| is at the value level, however n is only stored at the type level as the argument |ninputs|.
To be able to recover this value the |IsNat| type class, as defined in Section~\ref{sec:bg-is-nat} is used.
The |recoverNInputs| function is able to direct the |IsNat| type class to the correct type argument,
and produces an |SNat| with the same value as that stored in the type.

\noindent\begin{minipage}{\linewidth}
\begin{code}
recoverNInputs :: (  Length bsS ~ Length bsT, Length bsT ~ Length bsA, Length bsA ~ Length bsS,
                     ninputs ~ Length bsS, IsNat ninputs, Network n)
  => (N n asS asT asA) bsS bsT bsA csS csT csA (ninputs :: Nat)
  -> SNat (Length bsS)
circuitInputs _ = nat
\end{code}
\end{minipage}


After splitting a network and generating two new networks, the outputs will need to be joined together again: this will require the appending of two |PipeLists|.
To do this an |AppendP| type class is defined:

\noindent\begin{minipage}{\linewidth}
\begin{code}
class AppendP fs as xs gs bs ys where
  appendP :: PipeList fs as xs -> PipeList gs bs ys -> PipeList (fs :++ gs) (as :++ bs) (xs :++ ys)
\end{code}
\end{minipage}

This type class has one function |appendP|, it is able to append two |PipeLists| together.
It makes use of the  |:++| type family to append the type lists together.
The instances for this type class are made up of two cases: the base case and a recursive case.

\noindent\begin{minipage}{\linewidth}
\begin{code}
instance AppendP (Q([])) (Q([])) (Q([])) gs bs ys where
  appendP  PipeNil          ys  = ys

instance (AppendP fs as xs gs bs ys) => AppendP (f (Q(:)) fs) (a (Q(:)) as) (f a (Q(:)) xs) gs bs ys where
  appendP  (PipeCons x xs)  ys  = PipeCons x (appendP xs ys)
\end{code}
\end{minipage}

The base case corresponds to having an empty list on the left, with some other list on the right. Here the list on the right is returned.
The recursive case, simply takes 1 element from the left hand side and conses it onto the from of a recursive call, with the rest of the left hand side.


%format nL'
%format nR'

\noindent\begin{minipage}{\linewidth}
It is now possible to define the |beside| function. The result is calculated in 4 steps, with helper functions for each step:

\begin{enumerate}
  \item Get the number of inputs (|ninputs|) on the left hand side of the |Beside| constructor.
        This will give the information needed to split the inputted accumulated network |n|.
  \item Split the network into a left and right hand side.
        This will retain the same input type to the network, as there is no information on how to split that.
        Only the output |PipeList| will be split into two parts.
  \item Translate the both the left and right network.
        This will perform the recursive step and generate two new networks with the networks from the left and right added to the accumulated network |n|.
  \item Join the networks back together.
        Now that the left and right hand side of this layer has been added to the accumulated network,
        the two sides need to be joined back together to get a single network that can be returned.
\end{enumerate}
\end{minipage}

\todo{This is very long, but I cant really break it up because it needed the scoped type variables from beside }

\noindent\begin{minipage}{\linewidth}
\begin{code}
beside :: forall asS asT asA bsS bsT bsA csS csT csA (nbs :: Nat)
   . Beside (AccuN  BasicNetwork asS asT asA) bsS bsT bsA csS csT csA nbs
  -> IO ((AccuN     BasicNetwork asS asT asA) bsS bsT bsA csS csT csA nbs)
beside (Beside l r) = return $ AccuN
  (\n -> do
    let ninputs = circuitInputs l
    (nL  , nR  ) <- splitNetwork ninputs n
    (newL, newR) <- translate ninputs (nL, nR) (l, r)
    joinNetwork (newL, newR)
  )
 where
  splitNetwork :: SNat nbsL
    -> BasicNetwork asS asT asA bsS bsT bsA
    -> IO (  BasicNetwork asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA),
             BasicNetwork asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA))
  splitNetwork nbs n = return
    (  BasicNetwork (threads n) (inputs n) (takeP nbs (outputs n)),
       BasicNetwork (threads n) (inputs n) (dropP nbs (outputs n)))

  translate :: SNat nbsL
    -> (  BasicNetwork asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA),
          BasicNetwork asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA))
    -> (  (AccuN BasicNetwork asS asT asA)
           (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA) csLS csLT csLA nbsL,
          (AccuN BasicNetwork asS asT asA)
           (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA) csRS csRT csRA nbsR)
    -> IO (  BasicNetwork asS asT asA csLS csLT csLA,
             BasicNetwork asS asT asA csRS csRT csRA)
  translate _ (nL, nR) (N cL, N cR) = do
    nL' <- cL nL
    nR' <- cR nR
    return (nL', nR')

  joinNetwork :: (AppendP csLS csLT csLA csRS csRT csRA)
    => (BasicNetwork asS asT asA csLS csLT csLA, BasicNetwork asS asT asA csRS csRT csRA)
    -> IO (BasicNetwork asS asT asA (csLS :++ csRS) (csLT :++ csRT) (csLA :++ csRA))
  joinNetwork (nL, nR) = return
    $ BasicNetwork (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)
\end{code}
\end{minipage}

The |splitNetwork| function creates two new |BasicNetwork|s. To split the output values, |takeP|, and |dropP| are used.
|translate| performs the recursive step in the accumulating fold, which produces two new networks that include this layer.
|joinNetwork| takes the two new networks and appends the outputs with |appendP|.
It also has to append the thread ids from both sides, however, this will now include duplicates as threads were not split in |splitNetwork|.
To combat this |nub| is used, which returns a list containing all the unique values in the original.

\todo[inline]{Define buildBasicNetwork}

\section{UUIDS}
When inputting multiple values into a |Network| problems can occur.
For example, a task's output pointer is statically defined.
If this were a file, it would result in files being overwritten before they have been read.
This is eliminated through the use of UUIDs, they act as a unique identifier for each input into the network.

\subsection{Modifications}
To be able to support a UUID, several small modifications need to be made.

\paragraph{Data Store}
The value is accessible to the data store when saving by making a small modification:

%format UUID = "\Conid{\textcolor{red}{UUID}}"
%format uuid = "\Conid{\textcolor{red}{uuid}}"

\begin{code}
class DataStore f a where
  fetch :: UUID -> f a -> IO a
  save :: UUID -> f a -> a -> IO (f a)
\end{code}

This can then be made use of when reading or writing to a data store. For example, a filename could be prepended with the unique identifier, or it could be used as a primary key when saving to a database table.

\paragraph{PipeList}
To transfer the value around the network, the |PipeList| data type is modified to store channels of type |(UUID, f a)|, instead of just |f a|:

\begin{code}
data PipeList (fs :: [Type -> Type]) (as :: [Type]) (xs :: [Type]) where
  PipeCons :: Chan (UUID, f a) -> PipeList fs as xs -> PipeList (f (Q(:)) fs) (a (Q(:)) as) (f a (Q(:)) xs)
  PipeNil :: PipeList (Q([])) (Q([])) (Apply (Q([])) (Q([])))
\end{code}

\paragraph{Task Executor}
The task executor needs to be modified, so that it retries the UUID, gives it to the task and passes it on with the output.

\noindent\begin{minipage}{\linewidth}
\begin{code}
taskExecutor :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecutor (Task f outStore) inPipes outPipes = forever
  (do
    (uuid, taskInput)  <- readPipes inPipes
    r                  <- f uuid taskInputs outStore
    writePipes uuid (HCons' r HNil') outPipes
  )
\end{code}
\end{minipage}


\paragraph{Network: Read \& Write}
The read and write methods defined in the |Network| type class are modified to also take a UUID:

\begin{code}
class Network n where
  ...

  read :: n inputsS inputsT inputsA outputsS outputsT outputsA -> IO (UUID, HList' outputsS outputsT)
  write :: UUID -> HList' inputsS inputsT -> n inputsS inputsT inputsA outputsS outputsT outputsA -> IO ()
\end{code}

% reset the highlighting back to normal...
%format UUID = "\Conid{UUID}"
%format uuid = "\Conid{uuid}"

\subsection{Helper Functions}
There are several helper functions for reading and writing into a network:

\begin{spec}
input :: Network n => HList' inputsS inputsT
  -> n inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO UUID

input_ :: Network n => HList' inputsS inputsT
  -> n inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO ()

output_ :: Network n => n inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO (HList' outputsS outputsT)
\end{spec}

Both of the input functions generate a random UUID, meaning the user does not have to specify one.
|input| will return this generated values, whereas |input_| will not.
|output_| fetches values from a network, but does not return the UUID with the outputs.


\section{Failure in the Process Network}
Currently, when an exception occurs in a task the whole network crashes --- this isn't desired behaviour.
There are many ways to model failure in Haskell, one such example could be the |Maybe| monad.

\subsection{Maybe not Maybe}
|Maybe| can capture failure, with |Just x| being the success case, and |Nothing| being the failed case.
When |Nothing| is produced the rest of the computation automatically fails due to the definition of |>>=|.

\begin{code}
instance Monad Maybe where
  return x  = Just x

  (>>=) Nothing   _  = Nothing
  (>>=) (Just x)  f  = f x
\end{code}

This would work well in a network as an error in one task can be propagated to all it dependents, causing them to fail gracefully, and the error propagating further through the network.
Howver, there is one problem with |Maybe|, it does not retain any information about the error that occured.
This information would be very useful to a user, as it helps them debug the issue.

\subsection{Except Monad}
The |Except| monad is based on |Either|, with the |Left| constructor representing failure, and the |Right| constructor indicating success.
This means that an error message can now be stored, when a failure ultimately occurs.
Since tasks already execute in the |IO| monad, a monad transformer |ExceptT| is required, so that failure can be implemented into the network.
This allows for computation in both |IO|, and |Except|, however, any |IO| computation will need to be lifted into the |ExceptT| monad.

The following modifications are required to add modelling of failure in a network.

\paragraph{PipeList}
A pipelist will now need to also transfer information about whether the previous task failed to execute.
To do this it will carry an |Either TaskError (f a)|, with |TaskError| being a custom data type storing the error message text.

%format (EitherTaskError) = "\Conid{\textcolor{red}{Either}}\codeskip \Conid{\textcolor{red}{TaskError}}"

\begin{code}
data PipeList (fs :: [Type -> Type]) (as :: [Type]) (xs :: [Type]) where
  PipeCons :: Chan (UUID, EitherTaskError (f a))
       ->  PipeList fs as xs
       ->  PipeList (f (Q(:)) fs) (a (Q(:)) as) (f a (Q(:)) xs)
  PipeNil :: PipeList (Q([])) (Q([])) (Apply (Q([])) (Q([])))
\end{code}

\paragraph{Task Executor}
The task executor will need to be modified so that it executes the tasks in the |ExceptT| monad.


\begin{code}
taskExecuter :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecuter (Task f outStore) inPipes outPipes = forever
  (do
    (uuid, taskInputs)  <- readPipes inPipes
    r                   <- runExceptT $
        (do
          input  <- (ExceptT . return) taskInputs
          r      <-  catchE (intercept (f uuid input outStore))
                     (throwE . TaskError . ExceptionMessage . displayException)
          return (HCons' (r `deepseq` r) HNil')
        )

    writePipes uuid r outPipes
  )
\end{code}
\ignore{$}

This version of the executor, first reads the values from the input channels.
It then runs some computation in the |ExceptT| monad to get a return value |r :: Either TaskError (HList' outputsS outputsT)|.
The return value is then sent along the output channels.

The |catchE| function can be used to catch an exception thrown in a task, the exception is then converted to a |TaskError|, and re-thrown.
This will mean that only a |Either TaskError (HList' outputsS outputsT)| is returned, rather than another type of error.

There is, however, an error that is caused by Haskell's laziness: if a value is not evaluated inside the |runExceptT| block then it will not be caught, and the program will continue to crash.
To solve this the |deepseq| function is used, which fully evaluates the left argument, before returning the right argument.
This forces any error that could occur to happen inside the |runExceptT| block.


\section{Evaluation}

\begin{itemize}
  \item \textbf{Type-safe} --- This is a continuation of the previous requirement for the language. It is also important that once the user has built a well-typed |Circuit|, that the code also continues to be executed in a well-typed environment, to ensure that all inputs and outputs are correctly typed.
  \item \textbf{Parallel} --- One of the key benefits that comes from dataflow programming is implicit parallelisation.
        With this \ac{DSL} being tailored towards data pipelines, which could be computationally expensive,
        it should be able to benefit from parallel execution.
  \item \textbf{Competitive Speed} --- This library should be able to execute dataflows in a competitive time, with other libraries that already exist.
  \item \textbf{Failure Tolerance} --- It is important that if one invocation of a task crashes, it does not crash the whole program.
        This implementation should be able to gracefully handle errors and propagate them through the circuit.
  \item \textbf{Usable} --- The implementation of the library should not break any of the usability of the language design.
  \item \textbf{Maintainable} --- It should be easy to maintain the library and add new constructors in the future.
\end{itemize}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Type-safe}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Parallel}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Competitive Speed}
The network is, indeed, able to compute results in a time that is competitive with other libraries. More detail on this can be found in Chapter~\ref{chap:benchmarks}.

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Failure Tolerance}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Usable}

\paragraph{$\text{\rlap{$\checkmark$}}\square$ Maintainable}

% ------
% not really as important
% \section{Scheduler \& Typed Graph}
% \subsection{Problems representing a typed graph as a AST}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
