
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE KindSignatures, GADTs, LambdaCase, RankNTypes, TypeOperators, OverlappingInstances, DataKinds, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, PolyKinds #-}
module Background where
import Prelude hiding (or)
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

Parks described a variant of \acp{KPN}, called \acp{DPN}~\cite{381846}.
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

\subsection{Deep Embeddings}
A deep embedding is when the terms of the \ac{DSL} will construct an \ac{AST} as a host language datatype.
Semantics can then be provided later on with evaluation functions.
Consider the example of a minimal non-deterministic parser combinator library~\cite{wuYoda}.


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
Instead a type class called |IFunctor| --- also known as |HFunctor| --- can be used, which is able to maintain the type indicies~\cite{mcbride2011functional}.
This makes use of |~>|, which represents a natural transformation from |f| to |g|.
|IFunctor| can be thought of as a functor transformer: it is able to change the structure of a functor, whilst preserving the values inside it~\cite{lane1998categories}.
Whereas a functor changes the values inside a structure.


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
The fixed point of |ParserF| is |Parser3|.

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
The resulting type of |icata| is |f a|, therefore the |f| is a syntactic |Functor|.
\todo{is this the right terminology?}
This could be |IFix ParserF|, which would be a transformation to the same structure, possibly applying optimisations to the \ac{AST}.


\section{Data types \`{a} la carte}
When building a \ac{DSL} one problem that becomes quickly prevalent, the so called \textit{Expression Problem}~\cite{wadler_1998}.
The expression problem is a trade off between a deep and shallow embedding.
In a deep embedding, it is easy to add multiple interpretations to the \ac{DSL} - just add a new evaluation function.
However, it is not easy to add a new constructor, since all functions will need to be modified to add a new case for the constructor.
The opposite is true in a shallow embedding.

One possible attempt at fixing the expression problem is data types \`{a} la carte~\cite{swierstra_2008}.
It combines constructors using the co-product of their signatures.
This is defined as:

%if style /= newcode
%format :+: = ":\!\!+\!\!:"
%format :<: = ":\prec:"
%format ValF2
%format MulF2
%endif

\begin{code}
data (f :+: g) a = L (f a) | R (g a)
\end{code}

\noindent
It is also the case that if both |f| and |g| are |Functor|s then so is |f :+: g|.

\begin{code}
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R y) = R (fmap f y)
\end{code}


\noindent
For each constructor it is possible to define a new data type and a |Functor| instance specifying where is recurses.

\begin{code}
data ValF2 f = ValF2 Int
data MulF2 f = MulF2 f f

instance Functor ValF2 where
  fmap f (ValF2 x) = ValF2 x

instance Functor MulF2 where
  fmap f (MulF2 x y) = MulF2 (f x) (f y)
\end{code}

\noindent
By using |Fix| to tie the recursive knot, the |Fix (Val :+: Mul)| data type would be isomorphic to the original |Expr| datatype found in Section~\ref{sec:higher-order-functors}.

\noindent
One problem that now exist, however, is that it is now rather difficult to create expressions, take a simple example of $12 \times 34$.

\begin{code}
exampleExpr :: Fix (ValF2 :+: MulF2)
exampleExpr = In (R (MulF2 (In (L (ValF2 12))) (In (L (ValF2 34)))))
\end{code}

\noindent
It would be beneficial if there was a way to add these |L|s and |R|s automatically. Fortunately there is a method using injections.
The |:<:| type class captures the notion of subtypes between |Functor|s.

\begin{code}
class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = L

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = R . inj
\end{code}

\noindent
Using this type class, smart constructors can be defined.

\begin{code}
inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

val :: (ValF2 :<: f) => Int -> Fix f
val x = inject (ValF2 x)

mul :: (MulF2 :<: f) => Fix f -> Fix f -> Fix f
mul x y = inject (MulF2 x y)
\end{code}

\noindent
Expressions can now be built using the constructors, such as |val 12 `mul` val 34|.

A modular algebra can now be defined that provides an interpretation of this datatype.

\begin{code}
class Functor f => EvalAlg f where
  evalAlg :: f Int -> Int

instance (EvalAlg f, EvalAlg g) => EvalAlg (f :+: g) where
  evalAlg (L x) = evalAlg x
  evalAlg (R y) = evalAlg y

instance EvalAlg MulF2 where
  evalAlg (MulF2 x y) = x * y

instance EvalAlg ValF2 where
  evalAlg (ValF2 x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

eval :: EvalAlg f => Fix f -> Int
eval = cata evalAlg
\end{code}

One benefit to this approach is that is an interpretation is only needed for expressions that only use |MulF| and |ValF|.
If a new constructor such as |SubF| was added to the language and it would never be given to this fold, then it would not require an instance.
This helps to solve the expression problem.




\section{Dependently Typed Programming}
Although Haskell does not officially support dependently typed programming, there are techniques available that together can be used to replicate the experience.

\subsection{DataKinds Language Extension}

%if style == newcode
%format (Q(x)) = "''" x
%else
%format TICK = "''"
%format (Q(x)) = TICK x
%endif

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
%endif

\begin{code}
vecLength2 :: Vec a n -> SNat n
vecLength2 Nil          = SZero
vecLength2 (Cons x xs)  = SSucc (vecLength2 xs)
\end{code}


\subsection{Type Families}
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

\subsection{Summary}
Together these features allow for dependently typed programming in Haskell:

\begin{itemize}
  \item DataKinds allow for values to be promoted to types
  \item Singletons allow types to be demoted to values
  \item Type Families can be used to define functions that manipulate types.
\end{itemize}


\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
