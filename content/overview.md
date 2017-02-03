+++
weight = 5
title = "Overview of Scheme"
menu = "main"
chapter = 1
+++
# 1. Overview of Scheme

## 1.1. Semantics
{{< label "semanticsection" >}}

This section gives an overview of Scheme's semantics.  A
detailed informal semantics is the subject of
chapters 3 through 6.  For reference
purposes, section 7.2 provides a formal
semantics of Scheme.

Scheme is a statically scoped programming
language.  Each use of a variable is associated with a lexically
apparent binding of that variable.

Scheme is a dynamically typed language.  Types
are associated with values (also called objects) rather than
with variables.
Statically typed languages, by contrast, associate types with
variables and expressions as well as with values.

All objects created in the course of a Scheme computation, including
procedures and continuations, have unlimited extent.
No Scheme object is ever destroyed.  The reason that
implementations of Scheme do not (usually!) run out of storage is that
they are permitted to reclaim the storage occupied by an object if
they can prove that the object cannot possibly matter to any future
computation.

Implementations of Scheme are required to be properly tail-recursive.
This allows the execution of an iterative computation in constant space,
even if the iterative computation is described by a syntactically
recursive procedure.  Thus with a properly tail-recursive implementation,
iteration can be expressed using the ordinary procedure-call
mechanics, so that special iteration constructs are useful only as
syntactic sugar.  See section 3.5.

Scheme procedures are objects in their own right.  Procedures can be
created dynamically, stored in data structures, returned as results of
procedures, and so on.

One distinguishing feature of Scheme is that continuations, which
in most other languages only operate behind the scenes, also have
"first-class" status.  Continuations are useful for implementing a
wide variety of advanced control constructs, including non-local exits,
backtracking, and coroutines.  See section 6.10.

Arguments to Scheme procedures are always passed by value, which
means that the actual argument expressions are evaluated before the
procedure gains control, regardless of whether the procedure needs the
result of the evaluation.

Scheme's model of arithmetic is designed to remain as independent as
possible of the particular ways in which numbers are represented within a
computer. In Scheme, every integer is a rational number, every rational is a
real, and every real is a complex number.  Thus the distinction between integer
and real arithmetic, so important to many programming languages, does not
appear in Scheme.  In its place is a distinction between exact arithmetic,
which corresponds to the mathematical ideal, and inexact arithmetic on
approximations.  Exact arithmetic is not limited to integers.

## 1.2. Syntax

Scheme, like most dialects of Lisp, employs a fully parenthesized prefix
notation for programs and other data; the grammar of Scheme generates a
sublanguage of the language used for data.  An important
consequence of this simple, uniform representation is that
Scheme programs and data can easily be treated uniformly by other Scheme programs.
For example, the ``eval`` procedure evaluates a Scheme program expressed
as data.

The ``read`` procedure performs syntactic as well as lexical decomposition of
the data it reads.  The ``read`` procedure parses its input as data
(section 7.1.2), not as program.

The formal syntax of Scheme is described in section 7.1.


## 1.3. Notation and terminology

### 1.3.1. Base and optional features
{{< label "qualifiers" >}}

Every identifier defined in this report appears in one or more of several
\defining{libraries}.  Identifiers defined in the \defining{base library}
are not marked specially in the body of the report.
This library includes the core syntax of Scheme
and generally useful procedures that manipulate data.  For example, the
variable ``abs`` is bound to a
procedure of one argument that computes the absolute value of a
number, and the variable ``+`` is bound to a procedure that computes
sums.  The full list
all the standard libraries and the identifiers they export is given in
Appendix A.

All implementations of Scheme:

- Must provide the base library and all the identifiers exported from it.

- May provide or omit the other libraries given in this report, but each
  library must either be provided in its entirety, exporting no additional
  identifiers, or else omitted altogether.

- May provide other libraries not described in this report.

- May also extend the function of any identifier in this report, provided
  the extensions are not in conflict with the language reported here.

- Must support portable code by providing a mode of operation in which
  the lexical syntax does not conflict with the lexical syntax described
  in this report.


### 1.3.2. Error situations and unspecified behavior
{{< label "errorsituations" >}}

When speaking of an error situation, this report uses the phrase "an
error is signaled" to indicate that implementations must detect and
report the error.
An error is signaled by raising a non-continuable exception, as if by
the procedure ``raise`` as described in section 6.11.
The object raised is implementation-dependent
and need not be distinct from objects previously used for the same purpose.
In addition to errors signaled in situations described in this
report, programmers can signal their own errors and handle signaled errors.

The phrase "an error that satisfies _predicate_ is signaled" means that an error is
signaled as above.  Furthermore, if the object that is signaled is
passed to the specified predicate (such as ``file-error?`` or
``read-error?``), the predicate returns {{< tt "#t" >}}.

If such wording does not appear in the discussion of
an error, then implementations are not required to detect or report the
error, though they are encouraged to do so.
Such a situation is sometimes, but not always, referred to with the phrase
"an error."
In such a situation, an implementation may or may not signal an error;
if it does signal an error, the object that is signaled may or may not
satisfy the predicates ``error-object?``, ``file-error?``, or
``read-error?``.
Alternatively, implementations may provide non-portable extensions.

For example, it is an error for a procedure to be passed an argument of
a type that
the procedure is not explicitly specified to handle, even though such
domain errors are seldom mentioned in this report.  Implementations may
signal an error,
extend a procedure's domain of definition to include such arguments,
or fail catastrophically.

This report uses the phrase "may report a violation of an
implementation restriction" to indicate circumstances under which an
implementation is permitted to report that it is unable to continue
execution of a correct program because of some restriction imposed by the
implementation.  Implementation restrictions are discouraged,
but implementations are encouraged to report violations of implementation
restrictions.

For example, an implementation may report a violation of an
implementation restriction if it does not have enough storage to run a
program,
or if an arithmetic operation would produce an exact number that is
too large for the implementation to represent.

If the value of an expression is said to be "unspecified," then
the expression must evaluate to some object without signaling an error,
but the value depends on the implementation; this report explicitly does
not say what value is returned.

Finally, the words and phrases "must," "must not," "shall,"
"shall not," "should," "should not," "may," "required,"
"recommended," and "optional," although not capitalized in this
report, are to be interpreted as described in RFC 2119 {{< cite "rfc2119" >}}.
They are used only with reference to implementer or implementation behavior,
not with reference to programmer or program behavior.


### 1.3.3. Entry format

Chapters 4 and 6 are organized into entries.  Each entry describes one
language feature or a group of related features, where a feature is
either a syntactic construct or a procedure.  An entry begins with one
or more header lines of the form

\noindent\pproto{_template_}{_category_}\unpenalty

for identifiers in the base library, or

\noindent\pproto{_template_}{_name_ library _category_}\unpenalty

where _name_ is the short name of a library
as defined in Appendix A.

If _category_ is "syntax," the entry describes an expression
type, and the template gives the syntax of the expression type.
Components of expressions are designated by syntactic variables, which
are written using angle brackets, for example {{< hyper "expression" >}} and
{{< hyper "variable" >}}.  Syntactic variables are intended to denote segments of
program text; for example, {{< hyper "expression" >}} stands for any string of
characters which is a syntactically valid expression.  The notation
\begin{tabbing}
\qquad \hyperi{thing} $\ldots$
\end{tabbing}
indicates zero or more occurrences of a {{< hyper "thing" >}}, and
\begin{tabbing}
\qquad \hyperi{thing} \hyperii{thing} $\ldots$
\end{tabbing}
indicates one or more occurrences of a {{< hyper "thing" >}}.

If _category_ is "auxiliary syntax," then the entry describes a
syntax binding that occurs only as part of specific surrounding
expressions. Any use as an independent syntactic construct or
variable is an error.

If _category_ is "procedure," then the entry describes a procedure, and
the header line gives a template for a call to the procedure.  Argument
names in the template are _italicized_.  Thus the header line

\noindent\pproto{(vector-ref _vector_ _k_)}{procedure}\unpenalty

indicates that the procedure bound to the {\tt vector-ref} variable takes
two arguments, a vector _vector_ and an exact non-negative integer
_k_ (see below).  The header lines

\noindent
\pproto{(make-vector _k_)}{procedure}
\pproto{(make-vector _k_ _fill_)}{procedure}\unpenalty

indicate that the {\tt make-vector} procedure must be defined to take
either one or two arguments.

{{< label "typeconventions" >}}
It is an error for a procedure to be presented with an argument that it
is not specified to handle.  For succinctness, we follow the convention
that if an argument name is also the name of a type listed in
section~\ref{disjointness}, then it is an error if that argument is not of the named type.
For example, the header line for {\tt vector-ref} given above dictates that the
first argument to {\tt vector-ref} is a vector.  The following naming
conventions also imply type restrictions:
\newcommand{\foo}[1]{\vr{#1}, \vri{#1}, $\ldots$ \vrj{#1}, $\ldots$}
$$
\begin{tabular}{ll}
\vr{alist}&association list (list of pairs)\\
\vr{boolean}&boolean value ({{< tt "#t" >}} or {{< tt "#f" >}})\\
\vr{byte}&exact integer $0 \leq byte < 256$\\
\vr{bytevector}&bytevector\\
\vr{char}&character\\
\vr{end}&exact non-negative integer\\
\foo{k}&exact non-negative integer\\
\vr{letter}&alphabetic character\\
\foo{list}&list (see section~\ref{listsection})\\
\foo{n}&integer\\
_obj_&any object\\
\vr{pair}&pair\\
\vr{port}&port\\
\vr{proc}&procedure\\
\foo{q}&rational number\\
\vr{start}&exact non-negative integer\\
\vr{string}&string\\
\vr{symbol}&symbol\\
\vr{thunk}&zero-argument procedure\\
\vr{vector}&vector\\
\foo{x}&real number\\
\foo{y}&real number\\
\foo{z}&complex number\\
\end{tabular}
$$

The names \vr{start} and \vr{end} are used as indexes into strings,
vectors, and bytevectors.  Their use implies the following:



\item{It is an error if _start_ is greater than _end_.}

\item{It is an error if _end_ is greater than the length of the
string, vector, or bytevector.}

\item{If _start_ is omitted, it is assumed to be zero.}

\item{If _end_ is omitted, it assumed to be the length of the string,
vector, or bytevector.}

\item{The index _start_ is always inclusive and the index _end_ is always
exclusive.  As an example, consider a string.  If
_start_ and _end_ are the same, an empty
substring is referred to, and if _start_ is zero and _end_ is
the length of _string_, then the entire string is referred to.}



### 1.3.4. Evaluation examples

The symbol "\evalsto" used in program examples is read
"evaluates to."  For example,

```
(* 5 8)      \ev  40
```

means that the expression {\tt(* 5 8)} evaluates to the object {\tt 40}.
Or, more precisely:  the expression given by the sequence of characters
"{\tt(* 5 8)}" evaluates, in the initial environment, to an object
that can be represented externally by the sequence of characters "{\tt
40}."  See section~\ref{externalreps} for a discussion of external
representations of objects.

### 1.3.5. Naming conventions

By convention, \ide{?} is the final character of the names
of procedures that always return a boolean value.
Such procedures are called \defining{predicates}.
Predicates are generally understood to be side-effect free, except that they
may raise an exception when passed the wrong type of argument.

Similarly, \ide{!} is the final character of the names
of procedures that store values into previously
allocated locations (see section~\ref{storagemodel}).
Such procedures are called \defining{mutation procedures}.
The value returned by a mutation procedure is unspecified.

By convention, "\ide{->}" appears within the names of procedures that
take an object of one type and return an analogous object of another type.
For example, ``list->vector`` takes a list and returns a vector whose
elements are the same as those of the list.

A \defining{command} is a procedure that does not return useful values
to its continuation.

A \defining{thunk} is a procedure that does not accept arguments.
