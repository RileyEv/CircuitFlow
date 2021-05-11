%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE KindSignatures, GADTs, LambdaCase, RankNTypes, TypeOperators, OverlappingInstances, DataKinds, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, PolyKinds, GeneralizedNewtypeDeriving #-}
module Background where
import Prelude hiding (or, length, take, drop)
import Data.Kind (Type)
\end{code}
}

\chapter{Background}\label{chap:background}

\section{Dataflow Programming}
Dataflow programming is a paradigm that models applications as a directed graph.
The nodes of the graph have inputs and outputs and are pure functions, therefore have no side effects.
It is possible for a node to be a: source; sink; or processing node.
A source is a read-only storage: it can be used to feed inputs into processes.
A sink is a write-only storage: it can be used to store the outputs of processes.
Processes will read from either a source or the output of another process,
and then produce a result which is either passed to another process or saved in a sink.
Edges connect these nodes together, and define the flow of information.


\paragraph{Example - Data Pipelines}
A common use of dataflow programming is in pipelines that process data.
This paradigm is particularly helpful as it helps the developer to focus on each specific transformation on the data as a single component.
Avoiding the need for long and laborious scripts that could be hard to maintain.
One example of a data pipeline tool that makes use of dataflow programming is Luigi~\cite{spotify_luigi}.
An example dataflow graph produced by the tool is shown in Figure~\ref{fig:luigi-example}

\begin{figure}[ht]
  \centering
  \includegraphics[scale=0.4]{diagrams/luigi-example.png}
  \caption{Luigi dependency graph~\cite{hu_2015}}
  \label{fig:luigi-example}
\end{figure}



\paragraph{Example - Quartz Composer}
Apple developed a tool included in XCode, named Quartz Composer, which is a node-based visual programming language~\cite{quartz}.
As seen in Figure~\ref{fig:quartz-composer}, it uses a visual approach to programming connecting nodes with edges.
This allows for quick development of programs that process and render graphical data, without the user having to write a single line of code.
This means that even non-programmers are able to use the tool.

\begin{figure}[ht]
  \centering
  \includegraphics[scale=0.3]{diagrams/quartz_composer.png}
  \caption{Quartz composer~\cite{costello_2012}}
  \label{fig:quartz-composer}
\end{figure}


\paragraph{Example - Spreadsheets}
A widely used example of dataflow programming is in spreadsheets.
A cell in a spreadsheet can be thought of as a single node.
It is possible to specify dependencies to other cells through the use of formulas.
Whenever a cell is updated it sends its new value to those who depend on it, and so on.
Work has also done to visualise spreadsheets using dataflow diagrams, to help debug ones that are complex~\cite{hermans2011breviz}.


\subsection{The Benefits}
\paragraph{Visual}
The dataflow paradigm uses graphs, which make programming visual.
It allows the end-user programmer to see how data passes through the program, much easier than in an imperative approach.
In many cases, dataflow programming languages use drag and drop blocks with a graphical user interface to build programs.
For example, Tableau Prep~\cite{tableauPrep}, that makes programming more accessible to users who do not have programming skills.

\paragraph{Implicit Parallelism}
Moore's law states that the number of transistors on a computer chip doubles every two years~\cite{4785860}.
This meant that the chips' processing speeds also increased in alignment with Moore's law.
However, in recent years this is becoming harder for chip manufacturers to achieve~\cite{bentley_2020}.
Therefore, chip manufactures have had to turn to other approaches to increase the speed of new chips, such as multiple cores.
It is this approach the dataflow programming can effectively make use of.
Since each node in a dataflow is a pure function, it is possible to parallelise implicitly.
No node can interact with another node, therefore there are no data dependencies outside of those encoded in the dataflow.
Thus eliminating the ability for a deadlock to occur.

\subsection{Dataflow Diagrams}
Dataflow programs are typically viewed as a graph.
An example dataflow graph along with its corresponding imperative approach, can be found in Figure~\ref{fig:dataflow-example}.
The nodes $100$, $X$, and $Y$ are sources as they are only read from. $C$ is a sink as it is wrote to.
The remaining nodes are all processes, as they have some number of inputs and compute a result.

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

In this diagram is possible to see how implicit parallelisation is possible.
Both $A$ and $B$ can be calculated simultaneously, with $C$ able to be evaluated after they are complete.


\subsection{\acfp{KPN}}
A method introduced by Gilles Kahn, \acfp{KPN} realised the concept of dataflow networks
through the use of threads and unbounded \ac{FIFO} queues~\cite{DBLP:conf/ifip/Kahn74}.
The \ac{FIFO} queue is one where the items are output in the same order that they are added.
A node in the dataflow becomes a thread in the process network.
Each \ac{FIFO} queue represents the edges connecting the nodes in a graph.
The threads are then able to communicate through \ac{FIFO} queues.
The node can have multiple input queues and is able to read any number of values from them.
It will then compute a result and add it to an output queue.
Kahn imposed a restriction on a process in a \acp{KPN} that the thread is suspended if it attempts to fetch a value from an empty queue.
The thread is not allowed to test for the presence of data in a queue.

\begin{figure}[ht]
  \centering
  \input{diagrams/kpn-firing}
  \caption{A sequence of node firings in a \ac{KPN}}
  \label{fig:kpn-firing}
\end{figure}

\paragraph{\acfp{DPN}}
Parks described a variant of \acp{KPN}, called \acfp{DPN}~\cite{381846}.
They recognise that if functions have no side effects then they have no values to be shared between each firing.
Therefore, a pool of threads can be used with a central scheduler instead.



\section{\acfp{DSL}}
A \ac{DSL} is a programming language that has a specialised domain or use-case.
This differs from a \ac{GPL}, which can be applied across a larger set of domains, and are generally turing complete.
HTML is an example of a \ac{DSL}: it is good for describing the appearance of websites, however,
it cannot be used for more generic purposes, such as adding two numbers together.

\paragraph{Approaches to Implementation}
DSLs are typically split into two categories: standalone and embedded.
Standalone DSLs require their own compiler and typically their own syntax; HTML would be an example of a standalone \ac{DSL}.
\acp{EDSL} use an existing language as a host, therefore they use the syntax and compiler from the host.
This means that they are easier to maintain and often quicker to develop than standalone \acp{DSL}.
An \ac{EDSL}, can be implemented using two differing techniques: deep and shallow embeddings.


%if style /= newcode
%format Parser_d
%format Parser_s
%format parse_d
%format parse_s
%format Satisfy_d
%format Or_d
%format aorb_d
%format or_s
%format satisfy_s
%format aorb_s
%format parse_s
%endif

\subsection{Deep Embeddings}\label{sec:bg-deep-embedding}
A deep embedding is when the terms of the \ac{DSL} will construct an \ac{AST} as a host language datatype.
Semantics can then be provided later on with evaluation functions.
Consider the example of a minimal non-deterministic parser combinator library~\cite{wuYoda}, which will be a running example for this chapter.


\begin{code}
data Parser_d (a :: Type) where
  Satisfy_d  :: (Char -> Bool)  -> Parser_d Char
  Or_d       :: Parser_d a   -> Parser_d a -> Parser_d a
\end{code}

\noindent
This can be used to build a parser that can parse the characters |'a'| or |'b'|.

\begin{code}
aorb_d :: Parser_d Char
aorb_d = Satisfy_d (== 'a') `Or_d` Satisfy_d (== 'b')
\end{code}

\noindent
However, this parser does not have any semantics, therefore this needs to be provided by the evaluation function |parse|.

\begin{code}
parse_d :: Parser_d a -> String -> [(a, String)]
parse_d (Satisfy_d p)  = \case
  []       -> []
  (t:ts')  -> [(t, ts') | p t]
parse_d (Or_d px py)   = \ts -> parse_d px ts ++ parse_d py ts
\end{code}

\noindent
The program can then be evaluated by the |parse_d| function.
For example, |parse_d aorb_d "a"| evaluates to \eval{parse_d aorb_d "a"}, and |parse_d aorb_d "c"| evaluates to \eval{parse_d aorb_d "c"}.

A key benefit for deep embeddings is that the structure can be inspected, and then modified to optimise the user code: Parsley~\cite{parsley} makes use of such techniques to create optimised parsers.
Another benefit, is that you can provide multiple interpretations, by specifying different evaluation functions.
However, they also have drawbacks - it can be laborious to add a new constructor to the language.
Since it requires that all functions that use the deep embedding be modified to add a case for the new constructor \cite{SVENNINGSSON2015143}.

\subsection{Shallow Embeddings}
In contrast, a shallow approach is when the terms of the \ac{DSL} are defined as first class components of the language.
For example, a function in Haskell.
Components can then be composed together and evaluated to provide the semantics of the language.
Again a simple parser example can be considered.

%if style /= newcode
%format Parser2
%format aorb2
%format parse2
%endif

\begin{code}
newtype Parser_s a = Parser_s {parse_s :: String -> [(a, String)]}

or_s :: Parser_s a -> Parser_s a -> Parser_s a
or_s (Parser_s px) (Parser_s py) = Parser_s (\ts -> px ts ++ py ts)

satisfy_s :: (Char -> Bool) -> Parser_s Char
satisfy_s p = Parser_s (\case
  []       -> []
  (t:ts')  -> [(t, ts') | p t])
\end{code}

\noindent
The same |aorb_s| parser can be constructed from these functions, avoiding the need for an intermediate \ac{AST}.

\begin{code}
aorb_s :: Parser_s Char
aorb_s = satisfy_s (== 'a') `or_s` satisfy_s (== 'b')
\end{code}


Using a shallow implementation has the benefit of being able add new `constructors' to a \ac{DSL}, without having to modify any other functions.
Since each `constructor', produces the desired result directly.
However, this causes one of the main disadvantages of a shallow embedding - the structure cannot be inspected.
This means that optimisations cannot be made to the structure before evaluating it.


\section{Higher Order Functors}\label{sec:higher-order-functors}
%if style /= newcode
%format Parser_fixed
%format ~> = "\leadsto"
%endif

It is possible to capture the shape of an abstract datatype as a |Functor|.
The use of a |Functor| allows for the specification of where a datatype recurses.
Consider an example on a small expression language:

\begin{code}
data Expr  =  Add  Expr Expr
           |  Val  Int
\end{code}

The recursion within the |Expr| datatype can be removed to form |ExprF|.
The recursive steps can then be specified in the |Functor| instance.

\begin{code}
data ExprF f  =  AddF  f f
              |  ValF  Int

instance Functor ExprF where
  fmap f (AddF x y)  = AddF  (f x) (f y)
  fmap f (ValF x)    = ValF  x
\end{code}

To regain a datatype that is isomorphic to the original datatype, the recursive knot need to be tied.
This can be done with |Fix|, to get the fixed point of |ExprF|:

\begin{code}
data Fix f  = In (f (Fix f))
type Expr'  = Fix ExprF
\end{code}

There is, however, one problem: a |Functor| expressing the a parser language is required to be typed.
Parsers require the type of the tokens being parsed.
For example, a parser reading tokens that make up an expression could have the type |Parser Expr|.
A |Functor| does not retain this type information needed in a parser.

\paragraph{IFunctors}
Instead a type class called |IFunctor|~\cite{mcbride2011functional} --- also known as |HFunctor|~\cite{10.1145/1328438.1328475} --- can be used, which is able to maintain the type indicies.
This makes use of |~>|, which represents a natural transformation~\cite{lane1998categories} from |f| to |g|.
|IFunctor| can be thought of as a functor transformer: it is able to change the structure of a functor, whilst preserving the values inside it.
Whereas a functor changes the values inside a structure.

% \todo{cite this https://strathprints.strath.ac.uk/33726/1/ghani_popl08.pdf}

\begin{code}
type (~>) f g = forall a. f a -> g a
class IFunctor iF where
  imap :: (f ~> g) -> iF f ~> iF g
\end{code}

\noindent
The shape of |Parser| can be seen in |ParserF| where the |f| marks the recursive spots.
The type |f| represents the type of the children of that node.
In most cases this will be itself.

\begin{code}
data ParserF (f :: * -> *) (a :: *) where
  SatisfyF  :: (Char -> Bool) -> ParserF f Char
  OrF       :: f a -> f a -> ParserF f a
\end{code}

\noindent
An |IFunctor| instance can be defined, which follow the same structure as a standard |Functor| instance.

\begin{code}
instance IFunctor ParserF where
  imap  _  (SatisfyF s)  = SatisfyF s
  imap  f  (OrF px py)   = OrF (f px) (f py)
\end{code}

\noindent
|Fix| is used to get the fixed point of a |Functor|, to get the indexed fixed point |IFix| can be used.

\begin{code}
newtype IFix iF a = IIn (iF (IFix iF) a)
\end{code}

\noindent
The fixed point of |ParserF| is |Parser_fixed|.

\begin{code}
type Parser_fixed = IFix ParserF
\end{code}

In a deep embedding, the \ac{AST} can be traversed and modified to make optimisations, however, it may not be the best representation when evaluating it.
This means that it might be transformed to a different representation. In the case of a parser, this could be a stack machine.
Now that the recursion in the datatype has been generalised, it is possible to create a mechanism to perform this transformation.
An indexed \textit{catamorphism} is one such way to do this, it is a generalised way of folding an abstract datatype.
The use of a catamorphism removes the recursion from any folding of the datatype.
This means that the algebra can focus on one layer at a time.
This also ensures that there is no re-computation of recursive calls, as this is all handled by the catamorphism.
The commutative diagram below describes how to define a catamorphism, that folds an |IFix iF a| to a |f a|.

\begin{figure}[h]
\centering
\begin{tikzcd}[column sep=huge]
|iF (IFix iF) a|  \arrow[r, "|imap (icata alg)|"] \arrow[d, shift left=0.15cm, "|IIn|"] & |iF f a| \arrow[d, "|alg|"]\\
|IFix iF a|       \arrow[r, "|icata alg|"]        \arrow[u, shift left=0.15cm, "|inop|"]        & |f a|
\end{tikzcd}
\end{figure}

\noindent
|icata| is able to fold an |IFix iF a| and produce an item of type |f a|.
It uses the algebra argument as a specification of how to transform a single layer of the datatype.

\begin{code}
icata :: IFunctor iF => (iF f ~> f) -> IFix iF ~> f
icata alg (IIn x) = alg (imap (icata alg) x)
\end{code}

\noindent
The resulting type of |icata| is |f a|, therefore the |f| has kind |* -> *|.
This could be |IFix ParserF|, which would be a transformation to the same structure, possibly applying optimisations to the \ac{AST}.

\subsection{Monadic Catamorphism with IFunctors}\label{sec:bg-monadic-cat}
Using an indexed catamorphism, allows for principled recursion and makes it easier to define a fold over a data type, as any recursive step is abstracted from the user.
However, there may be times when the there is a need for monadic computations in the algebra.
To be able to do this a monadic catamorphism~\cite{monadic_cata} is defined:

\begin{code}
cataM :: (Traversable f, Monad m) => (forall a . f a -> m a) -> Fix f -> m a
cataM algM (In x) = algM =<< mapM (cataM algM) x
\end{code}

This catamorphism follows a similar pattern to a standard catamorphism, however, it upgrades the |Functor| constraint to |Traversable|,
which still requires that |f| is a |Functor|, but also provides additional functions such as a monadic map --- |mapM :: Monad m => (a -> m b) -> f a -> m (f b)|.
This allows the monadic catamorphism to be applied recursively on the data type being folded.

This technique can also be applied to indexed catamorphisms to gain a monadic version~\cite{10.1145/2036918.2036930}, however, to do so an indexed monadic map has to be introduced.
This will be included as part of the |IFunctor| type class:

\begin{spec}
class IFunctor iF where
  imap   :: (f ~> g) -> iF f ~> iF g
  imapM  :: Monad m => (forall a . f a -> m (g a)) -> iF f a -> m (iF g a)
\end{spec}

|imapM| is the indexed equivalent of |mapM|, it performs a natural transformation, but is capable of also using monadic computation.

The new |IFunctor| instance for |ParserF| is defined as:

\begin{spec}
instance IFunctor ParserF where
  imap = ... -- previously defined in this section.
  imapM  _  (SatisfyF s)  = return SatisfyF s
  imapM  f  (OrF px py)   = do
    px' <- f px
    py' <- f py
    return (OrF px' py')
\end{spec}

The definition for |imapM| on |ParserF| is intuitively the same, however just uses do-notation instead.
Making use of |imapM|, |icataM| is defined to be:

\begin{spec}
icataM :: (IFunctor iF, Monad m) => (forall a . iF f a -> m (f a)) -> IFix iF a -> m (f a)
icataM algM (IIn x) = algM =<< imapM (icataM algM) x
\end{spec}

|icataM|, has a similar structure to all other catamorphisms defined, however it takes a monadic algebra,
that can be used to transform the structure of the input type.


\section{Data types \`{a} la carte}\label{sec:bg-dtalacarte}
When building a \ac{DSL} one problem that becomes quickly prevalent, the so called \textit{Expression Problem}~\cite{wadler_1998}.
The expression problem is a trade off between a deep and shallow embedding.
In a deep embedding, it is easy to add multiple interpretations to the \ac{DSL} - just add a new evaluation function.
However, it is not easy to add a new constructor, since all functions will need to be modified to add a new case for the constructor.
The opposite is true in a shallow embedding.

One possible attempt at fixing the expression problem is \textit{Data types \`{a} la carte}~\cite{swierstra_2008}.
It combines constructors using the co-product of their signatures.
This technique makes use of standard functors, however, an approach using higher-order functors is described in \textit{Compositional data types}~\cite{10.1145/2036918.2036930}.


This is defined as:

%if style /= newcode
%format SatisfyF2
%format OrF2
%format satisfy_2
%format or_2
%format ApF2
%endif

\begin{code}
data (iF :+: iG) f a = L (iF f a) | R (iG f a)
\end{code}

\noindent
It is also the case that if both |f| and |g| are |IFunctor|s then so is the sum |f :+: g|.

\begin{code}
instance (IFunctor iF, IFunctor iG) => IFunctor (iF :+: iG) where
  imap f (L x) = L (imap f x)
  imap f (R y) = R (imap f y)
\end{code}


\noindent
For each constructor it is possible to define a new data type and a |Functor| instance specifying where is recurses.
This allows for the modularisation of the parser example:

\begin{code}
data SatisfyF2 f a where
  SatisfyF2 :: (Char -> Bool) -> SatisfyF2 f Char

data OrF2 f a where
  OrF2 :: f a -> f a -> OrF2 f a

instance IFunctor SatisfyF2 where
  imap f (SatisfyF2 g) = SatisfyF2 g

instance IFunctor OrF2 where
  imap f (OrF2 px py) = OrF2 (f px) (f py)
\end{code}

\noindent
By using |IFix| to tie the recursive knot, the |IFix (SatisfyF2 :+: OrF2)| data type would be isomorphic to the original |Parser_d| datatype found in Section~\ref{sec:bg-deep-embedding}.

\noindent
One problem that now exists, however, is that it is now rather difficult to create expressions.
Revisiting the simple example of a parser for |'a'| or |'b'|.

\begin{code}
exampleParser :: IFix (SatisfyF2 :+: OrF2) Char
exampleParser = IIn (R (OrF2 (IIn (L (SatisfyF2 (== 'a')))) (IIn (L (SatisfyF2 (== 'b'))))))
\end{code}

\noindent
It would be beneficial if there was a way to add these |L|s and |R|s automatically.
Fortunately, there is a method using injections.
The |:<:| type class captures the notion of subtypes between |IFunctor|s.

\begin{code}
class (IFunctor iF, IFunctor iG) => iF :<: iG where
  inj :: iF f a -> iG f a

instance IFunctor iF => iF :<: iF where
  inj = id

instance (IFunctor iF, IFunctor iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor iF, IFunctor iG, IFunctor iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
\end{code}

\noindent
Using this type class, smart constructors are defined:

\begin{code}
inject :: (iG :<: iF) => iG (IFix iF) a -> IFix iF a
inject = IIn . inj

satisfy_2 :: (SatisfyF2 :<: iF) => (Char -> Bool) -> IFix iF Char
satisfy_2 f = inject (SatisfyF2 f)

or_2 :: (OrF2 :<: iF) => IFix iF a -> IFix iF a -> IFix iF a
or_2 px py = inject (OrF2 px py)
\end{code}

\noindent
Expressions can now be built using the constructors, such as |satisfy_2 (== 'a') `or_2` satisfy_2 (== 'b')|.

A modular algebra can now be defined that provides an interpretation of this datatype.

\begin{code}
newtype Size a = Size {unSize :: Int} deriving Num

class IFunctor iF => SizeAlg iF where
  sizeAlg :: iF Size a -> Size a

instance (SizeAlg iF, SizeAlg iG) => SizeAlg (iF :+: iG) where
  sizeAlg (L x) = sizeAlg x
  sizeAlg (R y) = sizeAlg y

instance SizeAlg OrF2 where
  sizeAlg (OrF2 px py) = px + py

instance SizeAlg SatisfyF2 where
  sizeAlg (SatisfyF2 f) = 1

eval :: SizeAlg iF => IFix iF a -> Size a
eval = icata sizeAlg
\end{code}


The main benefit of this approach is modularity.
Each constructor is given by its interpretation in isolation and only for interpretations that make sense for it.
Additionally, existing interpretations are not affected by the addition of new constructors, such as |ApF2|.
This helps to solve the expression problem.


\section{Dependently Typed Programming}
Although Haskell does not officially support dependently typed programming, there are techniques available that together can be used to replicate some of the experience.

\subsection{DataKinds Language Extension}


Through the use of the DataKinds language extension~\cite{10.1145/2103786.2103795}, all data types can be promoted to also be kinds and their constructors to be type constructors.
When constructors are promoted to type constructors, they are prefixed with a |TICK|.
This allows for more interesting and restrictive types.

Consider the example of a vector that also maintains its length.
Peano numbers can be used to keep track of the length, which prevents a negative length for a vector.
This is where numbers are defined as zero or a number n incremented by 1.

\begin{code}
data Nat = Zero
         | Succ Nat
\end{code}

\noindent
A vector type can now be defined that makes use of the promoted |Nat| kind.

\begin{code}
data Vec :: Type -> Nat -> Type where
  Nil   :: Vec a (Q(Zero))
  Cons  :: a -> Vec a n -> Vec a ((Q(Succ)) n)
\end{code}

The use of DataKinds can enforce stronger types.
For example a function can now require that a specific length of vector is given as an argument.
With standard lists, this would not be possible, which could result in run-time errors when the incorrect length is used.
For example, getting the head of a list. Getting the head of an empty list an error will be thrown.
For a vector, a |safeHead| function can be defined that will not type check if the vector is empty.

\begin{code}
safeHead :: Vec a ((Q(Succ)) n) -> a
safeHead (Cons x _) = x
\end{code}

\subsection{Singletons}
DataKinds are useful for adding extra information back into the types, but how can information be recovered from the types?
For example, could a function that gets the length of a vector be defined?

\begin{spec}
vecLength :: Vec a n -> Nat
\end{spec}

This is enabled through the use of singletons~\cite{10.1145/2364506.2364522}.
A singleton in Haskell is a type that has just one inhabitant.
That is that there is only one possible value for each type.
They are written in such a way that pattern matching reveals the type parameter.
For example, the corresponding singleton instance for |Nat| is |SNat|.
The structure for |SNat| closely flows that of |Nat|.

\begin{code}
data SNat (n :: Nat) where
  SZero :: SNat (Q(Zero))
  SSucc :: SNat n -> SNat ((Q(Succ)) n)
\end{code}

\noindent
A function that fetches the length of a vector can now definable.

%if style /= newcode
%format vecLength2
%format :+ = ":\!\!+"
%format vecLength3
%endif

\begin{code}
vecLength2 :: Vec a n -> SNat n
vecLength2 Nil          = SZero
vecLength2 (Cons x xs)  = SSucc (vecLength2 xs)
\end{code}

\paragraph{Recovering an SNat}\label{sec:bg-is-nat}
Although being able to define a function that can recover the length of a vector is great, there is a more general way this can be approached.
This is to define a new type class that is able to recover an |SNat| from any type level |Nat|:

\begin{code}
class IsNat (n :: Nat) where
  nat :: SNat n
\end{code}

The type class has one value inside it |nat|, which can produce an |SNat| for a type level |Nat|.
There are two instances from this type class: a base case and a recursive case.

\begin{code}
instance IsNat (Q(Zero)) where
  nat = SZero

instance IsNat n => IsNat ((Q(Succ)) n) where
  nat = SSucc nat
\end{code}

The base case matches on the type level |Nat| |(Q(Zero))|, in this case |nat| is defined to be |SZero| --- the singleton equivalent.
The recursive step deals with the |(Q(Succ n))| case, where the singleton equivalent |SSucc| is used to define |nat|.

A new vector length function can then be defined as:



\begin{code}
vecLength3 :: IsNat n => Vec a n -> SNat n
vecLength3 _ = nat
\end{code}


\subsection{Type Families}\label{sec:bg-type-families}
Now consider the possible scenario of appending two vectors together.
How would the type signature look? This leads to the problem where two type-level |Nat|s need to be added together.
This is where Type Families~\cite{10.1145/1411204.1411215} become useful, they allow for the definition of functions on types.
Consider the example of appending two vectors together, this would require type-level arithmetic --- adding the lengths together.

\begin{spec}
vecAppend :: Vec a n -> Vec a m -> Vec a (n :+ m)
\end{spec}

\noindent
This requires a |:+| type family that can add two |Nat|s together.

\begin{code}
type family (a :: Nat) :+ (b :: Nat) where
  a  :+  (Q(Zero))    =  a
  a  :+  (Q(Succ)) b  =  (Q(Succ)) (a :+ b)
\end{code}

\subsection{Heterogeneous Lists}\label{sec:bg-heterogeneous-lists}
Heterogeneous lists~\cite{10.1145/1017472.1017488} are a way of having multiple types in the same list.
Rather than be parameterised by a single type, they instead make use of a type list, which is the list type promoted through DataKinds to be a kind, with its elements being types.
Each element in the type list aligns with the value at that position in the list, giving its type.
A heterogeneous list is defined as:

\begin{code}
data HList (xs :: [Type]) where
  HNil   :: HList (Q([]))
  HCons  :: x -> HList xs -> HList (x (Q(:)) xs)
\end{code}

This data type has two constructors:
\begin{itemize}
  \item |HNil| represents the empty list. The type parameter is the empty type list |(Q([]))|
  \item |HCons| allows a new element to be added to the list. The type parameter is the type of the item inserted consed onto the front of the types of the tail of the list.
\end{itemize}

\subsubsection{Functions on HLists}

\paragraph{Length}
Using singletons and type families, it is possible to get the length of a |HList| in a type-safe way.
Firstly, a type family is defined that is able to return the length of a type list.

\begin{code}
type family Length (l :: [k]) :: Nat where
  Length (Q([]))       =  (Q(Zero))
  Length (e (Q(:)) l)  =  (Q(Succ)) (Length l)
\end{code}

|Length| follows a similar definition to the |length :: [a] -> Int| function defined in the |Prelude|:

\begin{code}
length :: [a] -> Int
length []      = 0
length (x:xs)  = 1 + length xs
\end{code}

The base case of |Length| defines the length to be |(Q(Zero))|.
The recursive case increments the length by 1 for each item in the list, until it reaches the base case.

Now a function is defined that returns the length of a |HList|:

\begin{code}
lengthH :: HList xs -> SNat (Length xs)
lengthH HNil          = SZero
lengthH (HCons _ xs)  = SSucc (lengthH xs)
\end{code}

This follows the same structure as the |Length| type family, however instead, of working with types it uses singleton values.

\paragraph{Take}
Another function that may be helpful with |HList|s is |take|.
This will return the first n items from the list.
If n is larger than the length of the list, then the whole list will be returned.
Again, to be able to do this a new type family is needed -- |Take|:

\begin{code}
type family Take (n :: Nat) (l :: [k]) :: [k] where
  Take (Q(Zero))      l             =  (Q([]))
  Take ((Q(Succ)) n)  (Q([]))       =  (Q([]))
  Take ((Q(Succ)) n)  (e (Q(:)) l)  =  e (Q(:)) Take n l
\end{code}

The type family follows the same definition as the standard |take :: Int -> [a] -> [a]| as defined in the |Prelude|.
Similar to the |lengthH| function, |takeH| follows the same structure as the type family:

\begin{code}
takeH :: SNat n -> HList xs -> HList (Take n xs)
takeH SZero      l             = HNil
takeH (SSucc n)  HNil          = HNil
takeH (SSucc n)  (HCons x xs)  = HCons x (takeH n xs)
\end{code}


\paragraph{Drop}
The final function used in this project on |Hlist|s is one that can drop the first n elements.
The |Drop| type family can be defined as:

\begin{code}
type family Drop (n :: Nat) (l :: [k]) :: [k] where
  Drop  (Q(Zero))      l             = l
  Drop  ((Q(Succ)) _)  (Q([]))       = (Q([]))
  Drop  ((Q(Succ)) n)  (_ (Q(:)) l)  = Drop n l
\end{code}

The |Drop| type family also closely follows the definition of |drop :: Int -> [a] -> [a]| from the |Prelude|, and its result is reflected in the values level just as with |lengthH| and |takeH|.

\begin{code}
dropH :: SNat n -> HList xs -> HList (Drop n xs)
dropH SZero l = l
dropH (SSucc _) HNil = HNil
dropH (SSucc n) (HCons _ xs) = dropH n xs
\end{code}



\subsection{Summary}
Together these features allow for dependently typed programming constructs in Haskell:

\begin{itemize}
  \item DataKinds allow for values to be promoted to types
  \item Singletons allow types to be demoted to values
  \item Type Families can be used to define functions that manipulate types.
\end{itemize}



\section{Existential Types}
Typically, when defining a data type in Haskell, every type variable that exists on the right hand side of the equals, must also be on the left hand side.
For example, this is not allowed:

\begin{spec}
newtype Bad = Bad a
\end{spec}

Existential types~\cite{10.1145/44501.45065} are a way to allow this to happen, for example:

\begin{code}
data Good = forall a. Good a
\end{code}

One benefit of existential types is that the type variable no longer needs to be on the left hand side of the equals.

There is however, one problem with this approach, the variable will be a random unknown type.
To avoid this problem constraints are typically added to the signature, so that there can be a set of functions that work with that variable.
For example, the variable could have the |Show| constraint, so that we are able to use the |show| function with it:

\begin{code}
data Showy = forall a. Show a => Showy a
\end{code}

It would now be possible to build a list of items that can use the |show| function.

\begin{code}
showList :: [Showy]
showList = [Showy 123, Showy "abc"]
\end{code}

This list can now store any value with a |Show| instance defined, by wrapping it in the |Showy| constructor.

\section{Phantom Type Parameters}
Phantom type parameters~\cite{phantom_types} could be considered the opposite of existential types.
This is when a type variable only appears on the left hand side of the equals.
The most basic example is |Const|, it has two type arguments, but only |a| is used on the right hand side:

\begin{code}
newtype Const a b = Const a
\end{code}

Phantom type parameters can be used to store information in the types, which can act as further static constraints on the types.
Consider an example revolving around locking doors: it should not be possible to lock a door that is open, first it has to be closed and then it can be locked.
The state of the door can be represented by a data type that is promoted to a kind with the DataKinds extension.
A door can be represented as a type with a phantom type variable, with kind |DoorState| that record the state of the door:

\begin{code}
data DoorState = Open | Closed | Locked

data Door (state :: DoorState) where
  Door :: Door state
\end{code}

It would then be possible to define functions that can close and lock doors:

\begin{spec}
closeDoor :: Door (Q(Open)) -> Door (Q(Closed))
lockDoor :: Door (Q(Closed)) -> Door (Q(Locked))
\end{spec}

The |closeDoor| function, enforces that only an open door can be given as input, similarly |lockDoor| prevents an open door from being locked.


\section{Monoidal Resource Theories}\label{sec:bg-mrt}
\todo[inline]{Motivate a bit more...}
Resource theories~\cite{Coecke_2016} are a branch of mathematics that allow for the reasoning of questions surrounding resources, for example:
\begin{itemize}
  \item If I have some resources, can I make something?
  \item If I have some resources, how can I get what I want?
\end{itemize}
Resource theories provide a way to answer these questions.

\subsection{Preorders}
A preorder relation on a set $X$ is denoted by $\le$.
The relation must obey two laws:
\begin{enumerate}
  \item Reflexivity --- $x \le x$
  \item Transitivity --- if $x \le y$ and $y \le z$ then $ x \le z$.
\end{enumerate}

A preorder is a pair $(X, \le)$ made up of a set and a preorder relation on that set.
Preorders can represent many different things, such as, the less than relationship on numbers, or even dependencies between different values.
For example, $A \le B$ would mean that $A$ is required to calculate $B$.


\subsection{Symmetric Monoidal Preorders}\label{sec:bg-sym-monoidal-preorders}
To be a symmetric monoid then the following laws must be satisfied:
\begin{enumerate}
  \item Monotonicity --- $\forall x_1, x_2, y_1, y_2 \in X$, if $x_1 \le y_1$ and $x_2 \le y_2$, then $x_1 \otimes x_2 \le y_1 \otimes y_2$
  \item Unitality --- $\forall x \in X$, $I \otimes x = x$ and $x \otimes I = x$
  \item Associativity --- $\forall x, y, z \in X$, $(x \otimes y) \otimes z = x \otimes (y \otimes z)$
  \item Symmetry --- $\forall x, y \in X$, $x \otimes y = y \otimes x$
\end{enumerate}
\noindent
A symmetric monoidal structure $(X, \le, I, \otimes)$ on a preorder $(X, \le)$ has two additional components:
\begin{enumerate}
  \item The monoidal unit --- an element $I \in X$
  \item The monoidal product --- a function $\otimes : X \times X \to X$
\end{enumerate}

One example is where $X$ is a collection of resources, $\otimes$ combines resources together, and $\le$ defines dependencies between resources.

\subsection{Wiring Diagrams}\label{sec:bg-wiring-diagrams}
A graphical representation of symmetric monoidal preorders is a wiring diagram.
A wiring diagram is made up of: boxes that can have multiple inputs and outputs.
The boxes can be arranged in series or in parallel.
Figure~\ref{fig:bg-wiring-diagram-example}, shows an example wiring diagram.

\begin{figure}[ht]
  \centering
  \input{diagrams/wiring-diagram-example}
  \caption{An example wiring diagram}
  \label{fig:bg-wiring-diagram-example}
\end{figure}

A wiring diagram formalises a symmetric monoidal preorder, with each element $x \in X$ existing as the label on a wire.
Two wires, $x$ and $y$, drawn in parallel are considered to be the monoidal product $x \otimes y$.
The monodial unit is defined as a wire with the label $I$ or no wire.

\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (1.5, 0) node[midway, below] {$y$};
\draw (0, 0.25) -- (1.5, 0.25) node[midway, above] {$x$};
\end{tikzpicture}
\end{center}

A box connects parallel wires on the left to parallel wires on the right.
A wiring diagram is considered valid if the monoidal product of the left is less than the right.

\begin{center}
\begin{tikzpicture}
\draw (0, -0.2) -- (1.5, -0.2) node[midway, below] {$x_3$};
\draw (0, 0.125) -- (1.5, 0.125) node[midway, fill=white] {$x_2$};
\draw (0, 0.45) -- (1.5, 0.45) node[midway, above] {$x_1$};
\draw[rounded corners] (1.5, -0.5) rectangle (3, 0.75) node[pos=0.5] {$\le$};
\draw (3, -0.1) -- (4.5, -0.1) node[midway, below] {$y_2$};
\draw (3, 0.35) -- (4.5, 0.35) node[midway, above] {$y_1$};
\end{tikzpicture}
\end{center}

This example wiring diagram corresponds to the inequality $x_1 \otimes x_2 \otimes x_3 \le y_1 \otimes y_2$, which corresponds to the idea that $x_1$, $x_2$, and $x_3$ are required to get $y_1$, and $y_2$.

Each axiom in a symmetric monoidal preorder has a corresponding graphical form, using wiring diagrams.

\paragraph{Reflexivity}
The reflexivity law states that $x \le x$, this states that a diagram of one wire is valid.

\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (2, 0) node[midway, below] {$x$};
\end{tikzpicture}
\end{center}

This law corresponds to the idea that a resource is preserved.

\paragraph{Transitivity}
The transitivity law says that if $x \le y$ and $y \le z$ then $x \le y$. This corresponds to connecting two diagrams together in sequence.
If both of the diagrams

\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (1.5, 0) node[midway, below] {$x$};
\draw[rounded corners] (1.5, -0.25) rectangle (2.5, 0.25) node[pos=0.5] {$\le$};
\draw (2.5, 0) -- (4, 0) node[midway, below] {$y$};

\node at (5, 0) {and};


\draw (6, 0) -- (7.5, 0) node[midway, below] {$y$};
\draw[rounded corners] (7.5, -0.25) rectangle (8.5, 0.25) node[pos=0.5] {$\le$};
\draw (8.5, 0) -- (10, 0) node[midway, below] {$z$};
\end{tikzpicture}
\end{center}

\noindent
are valid, then they can be joined together to obtain another valid diagram.

\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (1.5, 0) node[midway, below] {$x$};
\draw[rounded corners] (1.5, -0.25) rectangle (2.5, 0.25) node[pos=0.5] {$\le$};
\draw (2.5, 0) -- (4, 0) node[midway, below] {$y$};
\draw[rounded corners] (4, -0.25) rectangle (5, 0.25) node[pos=0.5] {$\le$};
\draw (5, 0) -- (6.5, 0) node[midway, below] {$z$};
\end{tikzpicture}
\end{center}

If a box is considered a task that can transform values, then this law corresponds to the idea that two tasks can be composed in sequence, with the output of one being the input to the next.

\paragraph{Monotonicity}
Monotonicity states that, if $x_1 \le y_1$ and $x_2 \le y_2$, then $x_1 \otimes x_2 \le y_1 \otimes y_2$. This can be thought of as stacking two boxes on top of each other:


\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (1.5, 0) node[midway, below] {$x_1$};
\draw[rounded corners] (1.5, -0.25) rectangle (2.5, 0.25) node[pos=0.5] {$\le$};
\draw (2.5, 0) -- (4, 0) node[midway, below] {$y_1$};

\draw (0, 1) -- (1.5, 1) node[midway, below] {$x_2$};
\draw[rounded corners] (1.5, 0.75) rectangle (2.5, 1.25) node[pos=0.5] {$\le$};
\draw (2.5, 1) -- (4, 1) node[midway, below] {$y_2$};

\node at (5, 0.5) {$\leadsto$};

\draw (6, 0.25) -- (7.5, 0.25) node[midway, below] {$x_2$};
\draw (6, 0.75) -- (7.5, 0.75) node[midway, above] {$x_1$};
\draw[rounded corners] (7.5, 0) rectangle (8.5, 1) node[pos=0.5] {$\le$};
\draw (8.5, 0.25) -- (10, 0.25) node[midway, below] {$y_2$};
\draw (8.5, 0.75) -- (10, 0.75) node[midway, above] {$y_1$};
\end{tikzpicture}
\end{center}

This law conceptualises the idea that when resources are combined, the dependencies are respected.


\paragraph{Unitality}
The unitality law states that $I \otimes x = x$ and $x \otimes I = x$, this means that a blank space can be ignored and that diagrams such as


\begin{center}
\begin{tikzpicture}
\draw (0, 0) -- (1.5, 0) node[midway, below] {$x$};
\node at (0.75, 0.5) {Nothing};

\draw (1.5, 0) edge[out=0,in=180] (2.5, 0.25);

\draw (2.5, 0.25) -- (4, 0.25) node[midway, below] {$x$};

\draw (4, 0.25) edge[out=0,in=180] (5, 0.5);

\node at (5.75, 0) {Nothing};
\draw (5, 0.5) -- (6.5, 0.5) node[midway, above] {$x$};
\end{tikzpicture}
\end{center}

\noindent
are valid.

\paragraph{Associativity}
The associativity law says that $(x \otimes y) \otimes z = x \otimes (y \otimes z)$, this states that diagrams can be built from either the top or bottom.
This means that the order of grouping resources does not matter.
In reality it is trivial to see how this is true with wires:

\begin{center}
\begin{tikzpicture}
\draw (0, -0.4) -- (1.5, -0.4) node[midway, below] {$z$};
\draw (0, 0.125) -- (1.5, 0.125) node[midway, below] {$y$};
\draw (0, 0.45) -- (1.5, 0.45) node[midway, above] {$x$};

\node at (2.5, 0.125) {$=$};

\draw (3.5, -0.4) -- (5, -0.4) node[midway, below] {$z$};
\draw (3.5, -0.075) -- (5, -0.075) node[midway, above] {$y$};
\draw (3.5, 0.45) -- (5, 0.45) node[midway, above] {$x$};
\end{tikzpicture}
\end{center}

\paragraph{Symmetry}
The symmetry law states that $x \otimes y = y \otimes x$, this encodes the notion that a diagram is still valid even if the wires cross.

\begin{center}
\begin{tikzpicture}
\node at (-0.2,0.4) {$x$};
\node at (-0.2,0) {$y$};
\draw (0, 0) edge[out=0,in=180] (2.5, 0.4) ;
\draw (0, 0.4) edge[out=0,in=180] (2.5, 0);
\node at (2.7,0.4) {$y$};
\node at (2.7,0) {$x$};
\end{tikzpicture}
\end{center}


\paragraph{Discard Axiom}
There are times when there is no longer need to keep a value, it would be beneficial if it could be discarded.
In a wiring diagram this is represented as:

\begin{center}
\begin{tikzpicture}
\draw[-{Circle[black]}] (0,0) -- (1.5, 0);
\end{tikzpicture}
\end{center}

This can be added as an additional axiom to the definition of a symmetric monoidal preorder: $\forall x \in X, x \le I$
It corresponds to the idea that resources can be destroyed when they are no longer needed.

\paragraph{Copy Axiom}
The final axiom to add is the notion of copying a value: $\forall x \in X, x \le x + x$.
This can be represented in wiring diagram as a split wire:

\begin{center}
\begin{tikzpicture}

\node at (-0.2,0) {$x$};
\draw[-{Circle[black]}] (0,0) -- (1, 0);

\draw (1, 0) edge[out=0,in=180] (2.5, 0.25);
\draw (1, 0) edge[out=0,in=180] (2.5, -0.25);

\node at (2.7,0.25) {$x$};
\node at (2.7,-0.25) {$x$};
\end{tikzpicture}
\end{center}

This embodies the idea that it is possible to duplicate a resource.

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
