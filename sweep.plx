\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}					% Output directory
\htmlmainfile{sweep}				% Main document file
\bodycolor{white}				% Page colour

\begin{document}

\title{sweep: SWI-Prolog Embedded in Emacs}
\author{Eshel Yaron\email{me@eshelyaron.com}}

\maketitle

\begin{abstract}
  Sweep is an embedding of SWI-Prolog in GNU Emacs.  It provides an
  interface for executing Prolog queries and consuming their results
  from Emacs Lisp.  Sweep further builds on top of this interface and
  on top of the standard Emacs facilities to provide advanced features
  for developing SWI-Prolog programs in Emacs.
\end{abstract}

\pagebreak
\tableofcontents

\section{Installation}
\label{sec:sweep-installation}
Installing Sweep requires:

\begin{itemize}
\item Emacs 27 or later, and
\item SWI-Prolog 8.5.18 or later.
\end{itemize}

Sweep is available from NonGNU ELPA, to install it simply type in
Emacs \texttt{M-x package-install RET sweeprolog RET}.

Note that in Emacs prior to version 28, you need to explicitly enable
NonGNU ELPA by adding something like the following to your Emacs
configuration:

\begin{code}
  (with-eval-after-load 'package
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
\end{code}

\section{Getting started}
\label{sec:sweep-getting-started}
After installing the \texttt{sweeprolog} Elisp library, load it into
Emacs:

\begin{code}
  (require 'sweeprolog)
\end{code}

All set!  You can now use Sweep for Prolog development and for
integrating Prolog into your Emacs Lisp code.  For a full description
of the different features of Sweep, see
\href{https://eshelyaron.com/sweep.html}{the Sweep manual}.

\printindex

\end{document}
