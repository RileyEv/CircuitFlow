\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\chapter{Implementation}\label{chap:intro}

\section{Requirements}
The implementation of the network itself has several requirements that are separate from the API that the user will use:

\begin{itemize}
  \item \textbf{Type-safe} --- This is a continuation of the previous requirement for the language. It is also important that once the user has built a well-typed |Circuit|, that the code also continues to be executed in a well-typed environment, to ensure that all inputs and outputs are correctly typed.
  \item \textbf{Parallel} --- One of the key benefits that comes from dataflow programming is implicit parallelisation.
        With this \ac{DSL} being tailored towards data pipelines, which could be computationally expensive,
        it should be able to benefit from parallel execution.
  \item \textbf{Competitive Speed} --- This library should be able to execute dataflows in a competitive time, with other libraries that already exist.
  \item \textbf{Failure Tolerance} --- It is important that if one invocation of a task crashes, it does not crash the whole program.
        This implementation should be able to gracefully handle errors and propagate them through the circuit.
\end{itemize}
\todo{??? Is there anything else that maybe I haven't achieved? memorisation?}

\section{Circuit AST}
The constructors for the language are actually \textit{smart constructors}.
They provide a more elegant way to build an \ac{AST}, which represents the circuit.
\todo[inline]{flesh this out a bit more, doesnt really fit in the current context}

%format IFunctor7

\subsection{IFunctor}
The \ac{AST} that represents a |Circuit| is built using indexed functors, also known as |IFunctor|.
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

However, any data type that is converted to use an |IFunctor7|, will need a way to tie the recursive knot.
A new type called |IFix7| can be used, it will follow a similar pattern to |IFix|, but with 7 type indicies.

\begin{code}
newtype IFix7 iF a b c d e f g = IIn7 (iF (IFix7 iF) a b c d e f g)
\end{code}


\subsection{Type-Indexed Data Types \`{a} la Carte}
To build the \ac{AST}, the data types \`{a} la carte\todo{cite} approach is taken.
This allows for a modular approach, making the library more extendable later on.
To be able to use this approach, it needs to be modified to support indexed functors.
This modification, changes them to take an indexed functor with 7 type indicies --- |a| through to |g|:

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
This allows the smart constructor to produce an node in the \ac{AST} for any co-product of data types, that includes the |Then| data type.


\paragraph{Representing a Circuit}
Once each constructor has been defined then they can be combined together to form the |CircuitF| type, which can be used to represent a circuit.
\todo{combined = co-product}
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

\paragraph{Interaction with Network}
\todo[inline]{Should I add an example of how to use it?}

\subsection{The Basic Network Representation}
An implementation of the Network typeclass is a |BasicNetwork|. \todo{Does basic make it sound simple? should i got with standard or something like that??}
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

\todo[inline]{Should I add the definition of these?}

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

\subsection{Indexed Monadic Catamorphism --- icataM}
Although a monadic catamorphism exists~\cite{monadic_cata}, this still has the same prior issues noted in Section~\ref{sec:higher-order-functors}.
It lacks the ability retain type arguments of the abstract data type that is being folded.

\noindent\begin{minipage}{\linewidth}
\begin{code}
cataM :: (Traversable f, Monad m) => (forall a . f a -> m a) -> Fix f -> m a
cataM algM (In x) = algM =<< mapM (cataM algM) x
\end{code}
\end{minipage}

To solve the problem of not retaining types, an indexed monadic catamorphism will be introduced.\todo{Check this doesn't exist somewhere..}
However, to be able to define such a fold, a indexed monadic map is required.
This will be added by extending the |IFunctor7| instance to also include a function named |imapM7|.

%format imapM7

\noindent\begin{minipage}{\linewidth}
\begin{code}
class IFunctor7 iF where
  imap7 :: (forall a b c d e f g. f' a b c d e f g -> g' a b c d e f g) -> iF f' a b c d e f g -> iF g' a b c d e f g
  imapM7 :: Monad m  => (forall a b c d e f g. f' a b c d e f g -> m (g' a b c d e f g))
                     -> iF f' a b c d e f g
                     -> m (iF g' a b c d e f g)
\end{code}
\end{minipage}

The definition for this new function |imapM7| for each instance closely follows the non-monadic version.
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

Now that there is a indexed monadic map, it is possible to define the a monadic catamorphism for indexed functors:

\noindent\begin{minipage}{\linewidth}
\begin{code}
icataM7 :: (IFunctor7 iF, Monad m)
  => (forall a b c d e f g . iF f' a b c d e f g -> m (f' a b c d e f g))
  -> IFix7 iF a b c d e f g
  -> m (f' a b c d e f g)
icataM7 algM (IIn7 x) = algM =<< imapM7 (icataM7 algM) x
\end{code}
\end{minipage}


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

In a standard algebra both sides of the |Then| constructor would be evaluated.
In this case it would produce two disjoint networks, both with their own input and output channels.
The algebra for |Then|, would then need to join the output channels of task1 with the input channels of task2.
However, it is not possible to join channels together.
Instead, the output channels from task1 need to be accessible when creating task2.
This is referred to as a \textit{context-sensitive} or \textit{accumulating} fold.

To be able to have an accumulating fold, inside an indexed catamorphism a carrier data type is required to wrap up this function.
This carrier, which shall be named |N|, contains a function that when given a network that has been accumulated up to that point,
then it is able to produce a network including the next layer in a circuit.
The type of the layer being folded will be |Circuit a b c d e f g|.
\todo{this clear enough? maybe a diagram showing the layer and all the types}

\noindent\begin{minipage}{\linewidth}
\begin{code}
newtype N n asS asT asA a b c d e f g = N
  { unN :: n asS asT asA a b c -> IO (n asS asT asA d e f) }
\end{code}
\end{minipage}

This newtype has 3 additional type parameters at the beginning, namely: |asS|, |asT|, |asA|.
They represent the input types to the initial circuit.
Since the accumulating fold will work layer by layer from the top downwards, these types will remain constant and never change throughout the fold.

\paragraph{Classy Algebra}
An algebra type class can now be defined.
This will ensure that the approach remains with modular: a new instance can be made when adding a new constructor to the language.









\paragraph{InitialPipes}

\subsection{The Translation}

\paragraph{Basic Constructors}
Nice and simple

\paragraph{Task}
A lot nicer than you might think

\paragraph{Then}
funny step in the accumulating fold

\paragraph{Beside}
Time to unleash hell...

\section{UUIDS}
\paragraph{Why are they needed?}
\paragraph{How are they added?}

\section{Failure in the Process Network}
\paragraph{Why?}
\subsection{Maybe Monad}
\paragraph{No error messages}
\subsection{Except Monad}
based on |Either|
\paragraph{Propagation}





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
