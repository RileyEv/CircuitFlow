%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}

%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}


\chapter{Introduction}\label{chap:intro}

%% \section{Motivation}
It is estimated that every day over 2.5 quintillion bytes of data is generated~\cite{karki_2020} by people around the world.
With such vast quantities of data, being able to process it is now becoming more important then ever before.

Applications that process this data and perform analysis, such as machine learning, tend to be a series of tasks that need computing.
Tasks have a set of inputs and produce some outputs.
These tasks form data workflows in a \acf{DAG}.
There are existing libraries that make use of the \ac{DAG} structure to orchestrate tasks, such as the transformation of data or training a machine learning model: Spotify's Luigi~\cite{spotify_luigi} or Apache's Airflow~\cite{airflow}.


These \acp{DAG} encode dependencies between each task, but these libraries have no mechanism to ensure the dependencies are valid.
For example, in Luigi a task can output a file delimited by commas, but the dependent task may expect a file delimited by new lines.
This bug, will cause the pipeline to crash at runtime, which is not desirable.
Even worse, the bug could go unnoticed and cause havoc in later tasks.
The libraries put the onus on the user to make sure these inputs and outputs of a task align and do not cause errors.
With good development practices, the risk is reduced, but it the problem will never be eliminated.
If only there was something that could help...

Any tool that could perform this type of static analysis could help speed up the feedback loop, and possibly avoid buggy code from being deployed to production environments.
Once such tool is types!
However, both of these libraries are developed using Python --- the antithesis of types.
This promotes the need for a new library that can safely compose tasks and make use of types to perform static analysis to ensure that dependencies are valid.


\section{Contributions}

\begin{itemize}
    \item A new Haskell \acs{e-DSL} for constructing dataflow programs that:
        \begin{itemize}
          \item is able to exceed the performance of other competing libraries --- outperforming Luigi by almost 4x on larger numbers of inputs.
          \item leverages state of the art Haskell methods, ranging from DataKinds to Data types \`{a} la carte, to construct a language that is type-safe and modular.
          \item makes use of indexed functors, to construct a type-indexed \acs{AST}.
        \end{itemize}
    \item Provides a range of examples that demonstrates the language's applicability to many different problems.
    \item The first known implementation of a \acl{KPN} in Haskell.
\end{itemize}


\section{Outline}
The story begins at Chapter~\ref{chap:background}, where dataflow programming is defined and the benefits that it brings to the table are described.
The chapter also defines all the Haskell goodies needed to understand and implement the CircuitFlow \ac{DSL}.

As already hinted, there are many differing approaches to the new design of a data workflow \ac{DSL}.
In Chapter~\ref{chap:the-language}, two possible implementations are described: Chains and Circuits.
They are both evaluated against a desirable criteria for the design of the language.

Once settled on a good design for the language it needs to be implemented: this will be described in Chapter~\ref{chap:implementation}.
It will outline how a circuit is constructed under the hood, using a deep embedding, that allows for multiple interpretations to be given to the circuit.
Then it describes a method that can be used for transforming a circuit into a process network in a type-safe way, using principled recursion schemes.
With a working network in place, it will describe how a network will be modified to capture errors and propagate them through the network.
Finally, the implementation is evaluated against a set of criteria.

Now with a working language, two different example applications will be explored in Chapter~\ref{chap:examples},
working through their construction step-by-step and describing each step of the translation.
A final example pits CircuitFlow against Luigi, demonstrating CircuitFlow's ability to catch errors during compilation that would have caused runtime errors in Luigi.

Chapter~\ref{chap:benchmarks}, will aim to compare the performance of CircuitFlow against Luigi and serial implementations.
It then investigates deeper into the possible reasons for the results that are obtained.

\todo[inline]{Add something for the final chapter when it actually exists}

%% \newpage

%% Intro
%%  - describe a situation % \checkmark
%%  - describe a problem that comes from that situation % \checkmark
%%  - describe how others have approached it
%%  - explain the need for a new approach % \checkmark
%%  - say what you aim to do
%% This structure can be reused with all the baby steps that you need to take to succeed in your aim.
\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
