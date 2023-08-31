# Overview

Sweep is an embedding of SWI-Prolog in Emacs.  It provides an
interface for executing Prolog queries and consuming their results
from Emacs Lisp.  Sweep further builds on top of this interface and on
top of the standard Emacs facilities to provide advanced features for
developing SWI-Prolog programs in Emacs.

## Main Features

Some of the main benefits that Sweep brings to working with Prolog
code in Emacs are:

- Semantic highlighting
- Automatic indentation
- Structural editing and navigation
- Jumping to predicate definitions and references
- On-the-fly diagnostics
- Intelligent code completion
- Refactoring support
- Integrated SWI-Prolog top-level
- Ability to run Prolog queries directly from Emacs Lisp

These features and others, along with many options that Sweep provides
for you to customize its behavior, are documented in [the Sweep manual](https://eshelyaron.com/sweep.html).

# Installation

Installing Sweep requires:

-   Emacs 27 or later, and
-   SWI-Prolog 8.5.18 or later.

Sweep is available from NonGNU ELPA, to install it simply type in
Emacs `M-x package-install RET sweeprolog RET`.

Note that in Emacs prior to version 28, you need to explicitly enable
NonGNU ELPA by adding something like the following to your Emacs
configuration:

    (with-eval-after-load 'package
      (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

To upgrade Sweep to a newer version, do `M-x package-upgrade sweeprolog RET`.

# Getting Started

After installing the `sweeprolog` Elisp library, load it into Emacs:

    (require 'sweeprolog)

Sweep tries to find SWI-Prolog by looking for the `swipl` executable
in the directories listed in the Emacs variable `exec-path`.  When
Emacs is started from a shell, `exec-path` is initialized from the
`PATH` environment variable which normally includes the location of
`swipl` in common SWI-Prolog installations.  If the `swipl` executable
cannot be found via `exec-path`, you can tell Sweep where to find it
by setting the variable `sweeprolog-swipl-path` to point to it:

    (setq sweeprolog-swipl-path "/path/to/swipl")

All set!  You can now use Sweep for Prolog development and for integrating
Prolog into your Emacs Lisp code.  For a full description of the different
features of Sweep, see [the Sweep manual](https://eshelyaron.com/sweep.html).
