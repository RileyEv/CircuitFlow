%TC:envir hscode [] ignore
\documentclass[dissertation.tex]{subfiles}


%include format.fmt
%options ghci -pgmL lhs2tex -optL--pre

\begin{document}

\chapter{Examples}\label{chap:examples}

% \section{How to build a Circuit}
% car manufacturer example
% \newpage

\section{Machine Learning (Audio Playlist Generation)}
A use case for CircuitFlow is when building data pipelines.
Here there are many tasks that can only be executed when other data files have been produced.
A data pipeline will also benefit from being parallel to improve run-times.
CircuitFlow can help to build a parallel data pipeline, without the user having to worry about how to combine all their data manipulations together, in a way that will not run into concurrency problems.

Consider the example where an audio streaming service would like to create a playlist full of new songs to listen to.
This could require a machine learning model that can predict your songs based on the top 10 artists and songs that you have listened to over the last 3 months.
However, each of the months data is stored in different files that need aggregating together, before they can be input into the model.


\begin{figure}[ht]
  \centering
  \input{diagrams/example-song-hi-lev}
  \caption{A dataflow diagram for playlist generation}
  \label{fig:example-song-pre-hi-lev}
\end{figure}



\subsection{Building the pre-processing Circuit}\label{example-pre-proc-pipeline}
This circuit will need to have 3 different inputs --- each months listening history.
It will also have 2 outputs: the top 10 songs and artists.
To begin with a dataflow diagram can be constructed --- seen in Figure~\ref{fig:example-song-pre-proc}.
This will model all the dependencies between each of the pre-processing tasks.

\begin{figure}[ht]
  \centering
  \input{diagrams/example-song-pre-proc}
  \caption{A dataflow diagram for pre-processing the song data}
  \label{fig:example-song-pre-proc}
\end{figure}

To write this in CircuitFlow, the first step is to create all the tasks that will be used.
Both the |AggSongs| and |AggArtists| tasks will require three inputs, which will be CSVs.
This means that it is possible to make use of the pre-defined data store: |NamedCSVStore|.
The type instances that are required to parse the CSV will be omitted, as they are not relevant to the discussion.

%format count_ = count"\_"

\noindent\begin{minipage}{\linewidth}
\begin{spec}
aggSongsTask :: Circuit
       (Q([NamedCSVStore,            NamedCSVStore,            NamedCSVStore]))
       (Q([               [Listen],                 [Listen],                 [Listen]]))
       (Q([NamedCSVStore  [Listen],  NamedCSVStore  [Listen],  NamedCSVStore  [Listen]]))
       (Q([VariableStore]))
       (Q([                [TrackCount]]))
       (Q([VariableStore   [TrackCount]]))
       N3
aggSongsTask = multiInputTask f Empty
 where
  f :: HList (Q([[Listen], [Listen], [Listen]]) -> [TrackCount]
  f (HCons month1 (HCons month2 (HCons month3 HNil))) =
    (map (uncurry TrackCount) . reverse . count_ . map track) (month1 ++ month2 ++ month3)
\end{spec}
\end{minipage}

This task applies a composition of functions:

\begin{enumerate}
  \item Firstly, the track is extracted from the |Listen| data type using the function |track :: Listen -> Track|.
  \item Next, a list of unique tracks are extracted, along with the number of appearaces they made in the original list.
        This makes use of a special variant |count_ :: [a] -> [(a, Int)]|, which will also sort the list in ascending order.
  \item This list is then flipped to descending order with |reverse :: [a] -> [a]|.
  \item Finally, a TrackCount value is made from the previous tuple |uncurry TrackCount :: (Track, Int) -> TrackCount|.
\end{enumerate}


\noindent\begin{minipage}{\linewidth}
\begin{spec}
aggArtistsTask :: Circuit
       (Q([NamedCSVStore,            NamedCSVStore,            NamedCSVStore]))
       (Q([               [Listen],                 [Listen],                 [Listen]]))
       (Q([NamedCSVStore  [Listen],  NamedCSVStore  [Listen],  NamedCSVStore  [Listen]]))
       (Q([VariableStore]))
       (Q([                [ArtistCount]]))
       (Q([VariableStore   [ArtistCount]]))
       N3
aggArtistsTask = multiInputTask f Empty
 where
  f :: HList (Q([[Listen], [Listen], [Listen]])) -> [ArtistCount]
  f (HCons month1 (HCons month2 (HCons month3 HNil))) =
     (map (uncurry ArtistCount) . reverse . count_ . map (artist . track)) (day1 ++ day2 ++ day3)
\end{spec}
\end{minipage}

The |aggArtistsTask| is constructed in a similar way to the |aggSongsTask|, however, it extracts the artist from the track before aggregating the data.

The final task to define is |Top10|, however, this task is used multiple times.
Therefore, it would be beneficial if the type was polymorphic so that it can receive and input of both |[ArtistCount]| and |[TrackCount]|.
This is possible to do:

\noindent\begin{minipage}{\linewidth}
\begin{spec}
top10Task :: (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a)
  => FilePath
  -> Circuit  (Q([VariableStore]))  (Q([[a]]))  (Q([VariableStore  [a]]))
              (Q([NamedCSVStore]))  (Q([[a]]))  (Q([NamedCSVStore  [a]]))
              N1
top10Task filename = functionTask (take 10) (NamedCSVStore filename)
\end{spec}
\end{minipage}

The |top10Task| takes an input list and then returns the first 10 from the list.

Now that all of the tasks have been defined, they need to be combined into a circuit.
The dataflow diagram seen in Figure~\ref{fig:example-song-pre-proc}, will prove to be helpful.
The first obvious problem is how to create a circuit that can achieve the top part of the diagram, transforming the inputs so that they can be passed into two tasks.
This can be dealt with in layers: using the diagrams associated with each constructor, it is possible to convert a layer into a composition of |Circuit|s.
The first layer, seen in Figure~\ref{fig:example-song-layer1} will be responsible for duplicating each of the three inputs.

\begin{figure}[ht]
\centering
\begin{subfigure}{0.7\textwidth}
\centering
\begin{spec}
layer1 = replicate <> replicate <> replicate
\end{spec}
\vspace{-7mm}
\caption{}
\vspace{5mm}
\end{subfigure}
\begin{subfigure}{0.7\textwidth}
\centering
\input{diagrams/example-song-layer1}
\caption{}
\end{subfigure}
\caption{The first layer (a) and its corresponding dataflow diagram (b).}
\label{fig:example-song-layer1}
\end{figure}

 Scanning down the dataflow diagram the next layer to deal with is the two swaps that occur: month 1 and 2, and month 2 and 3.
 This can been seen in Figure~\ref{fig:example-song-layer2}

\begin{figure}[ht]
\centering
\begin{subfigure}{0.7\textwidth}
\centering
\begin{spec}
layer2 = id <> swap <> swap <> id
\end{spec}
\vspace{-7mm}
\caption{}
\vspace{5mm}
\end{subfigure}
\begin{subfigure}{\textwidth}
\centering
\input{diagrams/example-song-layer2}
\caption{}
\end{subfigure}
\caption{The second layer (a) and its corresponding dataflow diagram (b).}
\label{fig:example-song-layer2}
\end{figure}

\newpage % force it so the stuff below stays grouped with the figure

The final layer, seen in Figure~\ref{fig:example-song-layer3}, will swap month 1 with month 3.

\begin{figure}[ht]
\centering
\begin{subfigure}{0.7\textwidth}
\centering
\begin{spec}
layer3 = id <> id <> swap <> id <> id
\end{spec}
\vspace{-7mm}
\caption{}
\vspace{5mm}
\end{subfigure}
\begin{subfigure}{0.7\textwidth}
\centering
\input{diagrams/example-song-layer3}
\caption{}
\end{subfigure}
\caption{The third layer (a) and its corresponding dataflow diagram (b).}
\label{fig:example-song-layer3}
\end{figure}


\noindent\begin{minipage}{\linewidth}
These layers can be stacked on top of each other to provide a full transformation:

\begin{spec}
organiseInputs =  layer1
                  <->
                  layer2
                  <->
                  layer3
\end{spec}
\end{minipage}

\noindent\begin{minipage}{\linewidth}
The tasks can now be combined with this transformation to provide the full circuit:

\begin{spec}
preProcPipeline :: Circuit
       (Q([NamedCSVStore,            NamedCSVStore,            NamedCSVStore]))
       (Q([               [Listen],                 [Listen],                 [Listen]]))
       (Q([NamedCSVStore  [Listen],  NamedCSVStore  [Listen],  NamedCSVStore  [Listen]]))
       (Q([NamedCSVStore,                 NamedCSVStore]))
       (Q([                [ArtistCount],                 [TrackCount]]))
       (Q([NamedCSVStore   [ArtistCount],  NamedCSVStore  [TrackCount]]))
       N3
preProcPipeline =  organiseInputs
                   <->
                   aggSongsTask                  <> aggArtistsTask
                   <->
                   top10Task "top10Artists.csv"  <> top10Task "top10Artists.csv"
\end{spec}
\end{minipage}

Again it can be seen how this structure of tasks directly correlates with the dataflow diagram previously seen in Figure~\ref{fig:example-song-pre-proc}.
This helps to make it easier when designing circuits as it can be constructed visually level by level.


\subsection{Building the prediction Circuit}
Now that there is a pipeline to pre-process the data for the model, a new playlist can be created.
Again it is possible to build a circuit that combines with the pre-processing circuit.

The audio company train models for each user and store them in a cloud storage service.
This would be a good use-case for creating a new |DataStore| --- called a |ModelStore|.
Say that there are two types defined |ModelStore| and |NewSongPlaylist|: a new |DataStore| instance can be defined for each different model.
Here is the |NewSongsPlaylist| as an example:

\begin{spec}
instance DataStore ModelStore NewSongsPlaylist where
  -- Fetch from cloud storage and load into the program
  fetch  (ModelStore NewSongsPlaylist)    = ...
  save   (ModelStore NewSongsPlaylist) _  = ...
\end{spec}

|fetch| is used to download a trained model from the cloud store.
|save| will be used when training a new model, it will allow the model to be saved for use in the future.

With this |DataStore| a new task can be defined that will, take a users top 10 songs and artists, and a model as input.
It will then output a list of songs to create a playlist.

\noindent\begin{minipage}{\linewidth}
\begin{spec}
predictTask :: Circuit
       (Q([NamedCSVStore,                 NamedCSVStore,             ,  ModelStore]))
       (Q([               [ArtistCount],                 [TrackCount],              NewSongsPlayList]))
       (Q([NamedCSVStore  [ArtistCount],  NamedCSVStore  [TrackCount],  ModelStore  NewSongsPlaylist]))
       (Q([NamedCSVStore]))
       (Q([                [Track]]))
       (Q([NamedCSVStore   [Track]]))
       N3
predictTask = multiInputTask f (NamesCSVStore "newSongsPlaylist.csv")
  where
    f :: HList (Q([[ArtistCount], [TrackCount], NewSongsPlayList])) -> [Track]
    f (HCons topArtists (HCons topSongs (HCons model HNil))) =
       predict model topArtists topSongs
\end{spec}
\end{minipage}


\noindent\begin{minipage}{\linewidth}
The |predictTask| can now be combined with the pre-processing circuit to create a circuit that is able to create a new playlist from listening history.

\begin{spec}
createPlaylist =  preProcPipeline <> id
                  <->
                  predictTask
\end{spec}
\end{minipage}

The |createPlaylist|, circuit can now be converted into a |Network| and used on user data to generate new playlists, based on their top songs.
|preProcPipeline| will be revisited in Chapter~\ref{chap:benchmarks}, to act as a benchmark to compare the performance of the CircuitFlow library.



\section{Build System (lhs2TeX)}
Another use case for a task based dependency system is a build system.
For example, a Makefile is a way of specifying the target files from some source files, with a command that can be used to generate the target file.
CircuitFlow: could also be used to model such a system.

Consider this dissertation, which is made using \LaTeX.
This project is made up of multiple subfiles, each written in a literate Haskell format.
Each of these files needs to be pre-processed by the \texttt{lhs2TeX} command to produce the \texttt{.tex} source file.
Once each of these files has been generated, then the \LaTeX project can be built into a PDF file.


\subsection{Building the Circuit}
The |Circuit| defined here makes use of the |mapC| operator.
To do so a |Circuit| is defined that is able to build a single \texttt{.tex} file from a \texttt{.lhs}.
This has to make use of the standard |task| constructor:

%format HList'
%format HCons'
%format HNil'
%format -<.> = "~-\!\!<\!.\!>"
\noindent\begin{minipage}{\linewidth}
\begin{spec}
buildLhsTask :: Circuit  (Q([VariableStore]))  (Q([String]))  (Q([VariableStore  String]))
                         (Q([VariableStore]))  (Q([String]))  (Q([VariableStore  String]))
                         N1
buildLhsTask = task f Empty
  where
    f  :: UUID
       -> HList' (Q([VariableStore])) (Q([String]))
       -> VariableStore String
       -> ExceptT SomeException IO (VariableStore String)
    f _ (HCons' (Var fInName) HNil') _ = do
      let fOutName = fInName -<.> "tex"
      lift (callCommand ("lhs2tex -o " ++ fOutName ++ " " ++ fInName))
      return (Var fOutName)
\end{spec}
\end{minipage}

This |Circuit| makes use of the |callCommand| function from the |System.Process| library.
This allows the task to execute external commands, that may not necessarily be defined in Haskell.
A similar |Circuit| can be defined that will compile the \texttt{.tex} files and produce a PDF.

\noindent\begin{minipage}{\linewidth}
\begin{spec}
buildTexTask :: String -> Circuit  (Q([VariableStore]))  (Q([[String]]))  (Q([VariableStore  [String]]))
                                   (Q([VariableStore]))  (Q([String]))    (Q([VariableStore  String]))
                                   N1
buildTexTask name = task f Empty
 where
  f  :: UUID
     -> HList' '[VariableStore] '[[String]]
     -> VariableStore String
     -> ExceptT SomeException IO (VariableStore String)
  f mainFileName (HCons' (Var _) HNil') _ = do
    lift
      (callCommand
        ("texfot --no-stderr latexmk -interaction=nonstopmode -pdf "
        ++ "-no-shell-escape -bibtex -jobname="
        ++ name
        ++ " "
        ++ mainFileName
        )
      )
    return (Var "dissertation.pdf")
\end{spec}
\end{minipage}

The |buildTexTask| also demonstrates how it is possible to pass a global parameter into a task.
This can allow tasks to be made in a more reusable way.
Saving a user from defining multiple variations of the same task.


\noindent\begin{minipage}{\linewidth}
These two tasks can now be combined into a |Circuit|.
This |Circuit| can be interpreted as inputting a list of \texttt{.lhs} files, which are sequentially compiled to \texttt{.tex} files by the |mapC| operator.
These files are \textit{then} built by the |buildTexTask| to produce a PDF file as output.


\begin{spec}
buildDiss :: String -> Circuit  (Q([VariableStore]))  (Q([[String]]))  (Q([VariableStore  [String]]))
                                (Q([VariableStore]))  (Q([String]))    (Q([VariableStore  String]))
                                N1
buildDiss name =  mapC buildLhsTask Empty
                  <->
                  buildTexTask name
\end{spec}
\end{minipage}

|mapC| takes two arguments, the inner circuit to execute and a pointer to the location it should store its results.
In this case the output data store is a |VariableStore|, therefore, the pointer to this location is just an |Empty| variable.


\subsection{Using the Circuit}
To use this |Circuit|, a |Config| data type is used to store the information needed within the system to build the project:

\noindent\begin{minipage}{\linewidth}
\begin{spec}
data Config = Config
  {  mainFile    ::  FilePath
  ,  outputName  ::  String
  ,  lhsFiles    ::  [FilePath]
  }
  deriving (Generic, FromJSON, Show)
\end{spec}
\end{minipage}

This data type uses record syntax to have name fields:
\begin{itemize}
  \item |mainFile| is the name of the root file that should be used for compilation.
  \item |outputName| is the desired name for the output PDF file.
  \item |lhsFiles| are all the literate haskell files required to build the \LaTeX document.
\end{itemize}

The |Config| data type also derives the |Generic| and |FromJSON| instance.
This allows it to be used in conjunction with a YAML file to specify these parameters.
The config can be loaded with:

\begin{spec}
loadConfig :: IO Config
loadConfig = loadYamlSettings ["dissertation.tex-build"] [] ignoreEnv
\end{spec}

An example config file can be seen in Figure~\ref{fig:examples-lhs2tex-config-file}.

\begin{figure}[ht]
\centering
\begin{lstlisting}
mainFile: dissertation.lhs
outputName: dissertation
lhsFiles: [dissertation.lhs
         , chapters/introduction.lhs
         , chapters/background.lhs
         , chapters/the-language.lhs
         , chapters/implementation.lhs
         , chapters/evaluation.lhs
         , chapters/examples.lhs]
\end{lstlisting}
\caption{An example config file for the lhs2TeX build system}
\label{fig:examples-lhs2tex-config-file}
\end{figure}

To be able to use the system a |main| function is defined, which will serve as the entry point to the executable:

\noindent\begin{minipage}{\linewidth}
\begin{spec}
main :: IO ()
main = do
  config  <- loadConfig
  n       <- startNetwork (buildDiss (outputName config)) :: IO
    (BasicNetwork  (Q([VariableStore]))  (Q([[String]]))  (Q([VariableStore [String]]))
                   (Q([VariableStore]))  (Q([String]))    (Q([VariableStore String])))

  write (mainFile config -<.> "tex") (HCons' (Var (lhsFiles config)) HNil') n

  _ <- read n
  stopNetwork n
\end{spec}
\end{minipage}

The |main| function in the build system has 5 steps:

\begin{enumerate}
  \item The config file is loaded.
  \item A network is started based on the |buildDiss| circuit.
        The type of network to be started has to be annotated, so that the type system knows which |Network| instance to use.
  \item The build job is input into the network, with the input values being a list of \texttt{.lhs} files to compile.
  \item A call is made to the blocking function |read|, although the outputs are not needed, the call to |read| is.
        This prevents the program ending before the network has complete processing values.
  \item Finally, the network is destroyed.
\end{enumerate}

This dissertation makes use of this build system to be able include literate Haskell files.


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
countLetters = functionTask (map f) (FileStore "count.txt")
  where
    f word = (concat [word, ":", show (length word)])
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
