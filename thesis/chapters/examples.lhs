\documentclass[dissertation.tex]{subfiles}


%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Examples}\label{chap:intro}

\section{How to build a Circuit}
car manufacturer example
\newpage

\section{Song Data Aggregation}
Spotify for discover weekly. add in some sort of ML style model
\newpage

\section{lhs2TeX Build System}
Showing a use case for mapC and external tasks.
\newpage

\section{Types saving the day}
Consider an example shown in the docs for Luigi~\cite{spotify_luigi_docs_2020}, that is made up of two tasks.
The first generates a list of words and saves it to a file and second counts the number of letters in each of those words.
The counting letters task is dependent on the words being generated.

Figure~\ref{fig:broken-luigi}, shows an implementation of such a system, in the Python library called Luigi.
However, this implementation has a very subtle bug!
\texttt{GenerateWords} writes the words to a file separated by new lines, but \texttt{CountLetters} reads that same file as a comma-separated list.
This shows a key flaw in this system, it is up to the programmer to ensure that they write the outputs correctly,
and then that they read that same file in the same way.
This error, would not even cause a run-time error, instead, it will just produce the incorrect result.
For a developer this is extremely unhelpful, it means more of time is used writing tests --- something that no one enjoys.

\begin{figure}[ht]
\centering
\begin{lstlisting}[language=Python]
import luigi

class GenerateWords(luigi.Task):
    def output(self):
        return luigi.LocalTarget('words.txt')

    def run(self):
        # write a dummy list of words to output file
        words = ['apple', 'banana', 'grapefruit']

        with self.output().open('w') as f:
            for word in words:
                f.write('{word}\n'.format(word=word))

class CountLetters(luigi.Task):
    def requires(self):
        return GenerateWords()

    def output(self):
        return luigi.LocalTarget('letter_counts.txt')

    def run(self):
        # read in file as list
        with self.input().open('r') as infile:
            words = infile.read().split(',')

        # write each word to output file with its corresponding letter count
        with self.output().open('w') as outfile:
            for word in words:
                outfile.write('{word}:{letter_count}\n'.format(
                    word=word,
                    letter_count=len(word)
                ))
\end{lstlisting}
\caption{A Broken Luigi Example}
\label{fig:broken-luigi}
\end{figure}

\paragraph{The Fix}
Why not eliminate the need for all of this with |DataStore|s and types.
As previously mentioned in Section \ref{sec:data-stores}, a |DataStore| can be used to abstract the reading and writing of many different sources.
This will help to ensure correctness of this step, by eliminating any possible duplicated code.
Instead, just having the |fetch| and |save| methods to test.

The second greater benefit, is to use |DataStore|s in combination with the type system.
Each constructor for a |Circuit| will, enforce that the types of a |DataStore| align correctly.
It would not be possible to feed the output of one task, with the type |FileStore [String]| into a task that expects a |CommaSepFile [String]|.
The same example as before can be seen in Figure~\ref{fig:broken-circuit}.
In this example it will fail to compile, giving the error:

\vspace{3mm}
@> Couldn't match type `@|CommaSepFile|@' with `@|FileStore|@'@
\vspace{3mm}

\noindent
This will benefit the user as it reduces the feedback loop of knowing if the program will succeed.
Previously the whole data pipeline had to be run, whereas now this information can be informed to the user at compile-time.

\begin{figure}[ht]
\begin{spec}
 generateWords :: Circuit  (Q([VariableStore]))  (Q([()]))        (Q([VariableStore  ()]))
                           (Q([FileStore]))      (Q([[String]]))  (Q([FileStore      [String]]))
                           N1
generateWords = functionTask (const ["apple", "banana", "grapefruit"]) (FileStore "fruit.txt")
\end{spec}
\begin{spec}
countLetters :: Circuit  (Q([CommaSepFile]))  (Q([[String]]))  (Q([CommaSepFile  [String]]))
                         (Q([FileStore]))     (Q([[String]]))  (Q([FileStore     [String]]))
                         N1
countLetters = functionTask (foldr f []) (FileStore "count.txt")
  where
    f word cs = (concat [word, ":", show (length word)]) : cs
\end{spec}
\begin{spec}
circuit :: Circuit  (Q([VariableStore]))  (Q([()]))        (Q([VariableStore  ()]))
                    (Q([FileStore]))      (Q([[String]]))  (Q([FileStore      [String]]))
                    N1
circuit = generateWords <-> countLetters
\end{spec}
\caption{A Broken |Circuit| Example}
\label{fig:broken-circuit}
\end{figure}



\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
