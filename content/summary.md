+++
weight = 2
title = "Summary"
menu = "main"
+++
# Summary

The report gives a defining description of the programming language
Scheme.  Scheme is a statically scoped and properly tail recursive
dialect of the Lisp programming language~\cite{McCarthy} invented by Guy Lewis
Steele~Jr. and Gerald Jay~Sussman.  It was designed to have
exceptionally clear and simple semantics and few different ways to
form expressions.  A wide variety of programming paradigms, including
imperative, functional, and object-oriented styles, find convenient
expression in Scheme.

The introduction offers a brief history of the language and of
the report.

The first three chapters present the fundamental ideas of the
language and describe the notational conventions used for describing the
language and for writing programs in the language.

Chapters~\ref{expressionchapter} and~\ref{programchapter} describe
the syntax and semantics of expressions, definitions, programs, and libraries.

Chapter~\ref{builtinchapter} describes Scheme's built-in
procedures, which include all of the language's data manipulation and
input/output primitives.

Chapter~\ref{formalchapter} provides a formal syntax for Scheme
written in extended BNF, along with a formal denotational semantics.
An example of the use of the language follows the formal syntax and
semantics.

Appendix~\ref{stdlibraries} provides a list of the standard libraries
and the identifiers that they export.

Appendix~\ref{stdfeatures} provides a list of optional but standardized
implementation feature names.


The report concludes with a list of references and an
alphabetic index.

\begin{note}
The editors of the {{< rnrs 5 >}} and {{< rnrs 6 >}} reports are
listed as authors of this report in recognition of the substantial
portions of this report that are copied directly from {{< rnrs 5 >}} and {{< rnrs 6 >}}.
There is no intended implication that those editors, individually or
collectively, support or do not support this report.
\end{note}



\vfill
\eject

\chapter*{Contents}
\addvspace{3.5pt}                  % don't shrink this gap
\renewcommand{\tocshrink}{-3.5pt}  % value determined experimentally
{\footnotesize
\tableofcontents
}

\vfill
\eject
