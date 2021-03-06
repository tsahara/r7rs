+++
weight = 9
title = "3 Basic concepts"
menu = "main"
+++
# 3. Basic concepts
{{< label "basicchapter" >}}

## 3.1. Variables, syntactic keywords, and regions
{{< label "specialformsection" >}}
{{< label "variablesection" >}}

An identifier can name either a type of syntax or
a location where a value can be stored.  An identifier that names a type
of syntax is called a _syntactic keyword_
and is said to be _bound_ to a transformer for that syntax.  An identifier that names a
location is called a _variable_ and is said to be
_bound_ to that location.  The set of all visible
bindings in effect at some point in a program is
known as the _environment_ in effect at that point.  The value
stored in the location to which a variable is bound is called the
variable's value.  By abuse of terminology, the variable is sometimes
said to name the value or to be bound to the value.  This is not quite
accurate, but confusion rarely results from this practice.

Certain expression types are used to create new kinds of syntax
and to bind syntactic keywords to those new syntaxes, while other
expression types create new locations and bind variables to those
locations.  These expression types are called _binding constructs_.

Those that bind syntactic keywords are listed in section~\ref{macrosection}.
The most fundamental of the variable binding constructs is the
``lambda`` expression, because all other variable binding constructs
can be explained in terms of ``lambda`` expressions.  The other
variable binding constructs are ``let``, ``let*``, ``letrec``,
``letrec*``, ``let-values``, ``let*-values``,
and ``do`` expressions (see sections~\ref{lambda}, \ref{letrec}, and
\ref{do}).


Scheme is a language with
block structure.  To each place where an identifier is bound in a program
there corresponds a \defining{region} of the program text within which
the binding is visible.  The region is determined by the particular
binding construct that establishes the binding; if the binding is
established by a ``lambda`` expression, for example, then its region
is the entire ``lambda`` expression.  Every mention of an identifier
refers to the binding of the identifier that established the
innermost of the regions containing the use.  If there is no binding of
the identifier whose region contains the use, then the use refers to the
binding for the variable in the global environment, if any
(chapters 4 and \ref{initialenv}); if there is no
binding for the identifier,
it is said to be \defining{unbound}.

## 3.2. Disjointness of types
{{< label "disjointness" >}}

No object satisfies more than one of the following predicates:

```
boolean?          bytevector?
char?             eof-object?
null?             number?
pair?             port?
procedure?        string?
symbol?           vector?
```

and all predicates created by ``define-record-type``.

These predicates define the types
_boolean, bytevector, character_, the empty list object,
_eof-object, number, pair, port, procedure, string, symbol, vector_,
and all record types.
\schindex{boolean?}\schindex{pair?}\schindex{symbol?}
\schindex{number?}\schindex{char?}\schindex{string?}\schindex{vector?}
\schindex{bytevector?}\schindex{port?}\schindex{procedure?}
\schindex{eof-object?}

Although there is a separate boolean type,
any Scheme value can be used as a boolean value for the purpose of a
conditional test.  As explained in section~\ref{booleansection}, all
values count as true in such a test except for {{< tt "#f" >}}.
This report uses the word "true" to refer to any
Scheme value except {{< tt "#f" >}}, and the word "false" to refer to
{{< tt "#f" >}}.

## 3.3. External representations
{{< label "externalreps" >}}

An important concept in Scheme (and Lisp) is that of the _external
representation_ of an object as a sequence of characters.  For example,
an external representation of the integer 28 is the sequence of
characters "{\tt 28}", and an external representation of a list consisting
of the integers 8 and 13 is the sequence of characters "{\tt(8 13)}".

The external representation of an object is not necessarily unique.  The
integer 28 also has representations "{\tt #e28.000}" and "{\tt#x1c}", and the
list in the previous paragraph also has the representations "{\tt( 08 13
)}" and "{\tt(8 . (13 . ()))}" (see section~\ref{listsection}).

Many objects have standard external representations, but some, such as
procedures, do not have standard representations (although particular
implementations may define representations for them).

An external representation can be written in a program to obtain the
corresponding object (see ``quote``, section~\ref{quote}).

External representations can also be used for input and output.  The
procedure ``read`` (section~\ref{read}) parses external
representations, and the procedure ``write`` (section~\ref{write})
generates them.  Together, they provide an elegant and powerful
input/output facility.

Note that the sequence of characters "{\tt(+ 2 6)}" is _not_ an
external representation of the integer 8, even though it _is_ an
expression evaluating to the integer 8; rather, it is an external
representation of a three-element list, the elements of which are the symbol
{\tt +} and the integers 2 and 6.  Scheme's syntax has the property that
any sequence of characters that is an expression is also the external
representation of some object.  This can lead to confusion, since it is not always
obvious out of context whether a given sequence of characters is
intended to denote data or program, but it is also a source of power,
since it facilitates writing programs such as interpreters and
compilers that treat programs as data (or vice versa).

The syntax of external representations of various kinds of objects
accompanies the description of the primitives for manipulating the
objects in the appropriate sections of chapter~\ref{initialenv}.

## 3.4. Storage model
{{< label "storagemodel" >}}

Variables and objects such as pairs, strings, vectors, and bytevectors implicitly
denote locations or sequences of locations.  A string, for
example, denotes as many locations as there are characters in the string.
A new value can be
stored into one of these locations using the {\tt string-set!} procedure, but
the string continues to denote the same locations as before.

An object fetched from a location, by a variable reference or by
a procedure such as ``car``, ``vector-ref``, or ``string-ref``, is
equivalent in the sense of \ide{eqv?}
(section~\ref{equivalencesection})
to the object last stored in the location before the fetch.

Every location is marked to show whether it is in use.
No variable or object ever refers to a location that is not in use.

Whenever this report speaks of storage being newly allocated for a variable
or object, what is meant is that an appropriate number of locations are
chosen from the set of locations that are not in use, and the chosen
locations are marked to indicate that they are now in use before the variable
or object is made to denote them.
Notwithstanding this,
it is understood that the empty list cannot be newly allocated, because
it is a unique object.  It is also understood that empty strings, empty
vectors, and empty bytevectors, which contain no locations, may or may
not be newly allocated.

Every object that denotes locations is
either mutable or
immutable.  Literal constants, the strings
returned by \ide{symbol->string},
and possibly the environment returned by ``scheme-report-environment``
are immutable objects.  All objects
created by the other procedures listed in this report are mutable.
It is an
error to attempt to store a new value into a location that is denoted by an
immutable object.



These locations are to be understood as conceptual, not physical.
Hence, they do not necessarily correspond to memory addresses,
and even if they do, the memory address might not be constant.

\begin{rationale}
In many systems it is desirable for constants (i.e. the values of
literal expressions) to reside in read-only memory.
Making it an error to alter constants permits this implementation strategy,
while not requiring other systems to distinguish between
mutable and immutable objects.
\end{rationale}

## 3.5. Proper tail recursion
{{< label "proper tail recursion" >}}

Implementations of Scheme are required to be
_properly tail-recursive_.
Procedure calls that occur in certain syntactic
contexts defined below are _tail calls_.  A Scheme implementation is
properly tail-recursive if it supports an unbounded number of active
tail calls.  A call is _active_ if the called procedure might still
return.  Note that this includes calls that might be returned from either
by the current continuation or by continuations captured earlier by
``call-with-current-continuation`` that are later invoked.
In the absence of captured continuations, calls could
return at most once and the active calls would be those that had not
yet returned.
A formal definition of proper tail recursion can be found
in {{< cite "propertailrecursion" >}}.

\begin{rationale}

Intuitively, no space is needed for an active tail call because the
continuation that is used in the tail call has the same semantics as the
continuation passed to the procedure containing the call.  Although an improper
implementation might use a new continuation in the call, a return
to this new continuation would be followed immediately by a return
to the continuation passed to the procedure.  A properly tail-recursive
implementation returns to that continuation directly.

Proper tail recursion was one of the central ideas in Steele and
Sussman's original version of Scheme.  Their first Scheme interpreter
implemented both functions and actors.  Control flow was expressed using
actors, which differed from functions in that they passed their results
on to another actor instead of returning to a caller.  In the terminology
of this section, each actor finished with a tail call to another actor.

Steele and Sussman later observed that in their interpreter the code
for dealing with actors was identical to that for functions and thus
there was no need to include both in the language.

\end{rationale}

A _tail call_ is a procedure call that occurs
in a _tail context_.  Tail contexts are defined inductively.  Note
that a tail context is always determined with respect to a particular lambda
expression.


- The last expression within the body of a lambda expression,
  shown as {{< hyper "tail expression" >}} below, occurs in a tail context.
  The same is true of all the bodies of ``case-lambda`` expressions.
\begin{grammar}
(l\=ambda \meta{formals}
  \>\arbno{\meta{definition}} \arbno{\meta{expression}} \meta{tail expression})

(case-lambda \arbno{(\meta{formals} \meta{tail body})})
\end{grammar}

- If one of the following expressions is in a tail context,
then the subexpressions shown as \meta{tail expression} are in a tail context.
These were derived from rules in the grammar given in
chapter 7 by replacing some occurrences of \meta{body}
with \meta{tail body},  some occurrences of \meta{expression}
with \meta{tail expression},  and some occurrences of \meta{sequence}
with \meta{tail sequence}.  Only those rules that contain tail contexts
are shown here.

\begin{grammar}
(if \meta{expression} \meta{tail expression} \meta{tail expression})
(if \meta{expression} \meta{tail expression})

(cond \atleastone{\meta{cond clause}})
(cond \arbno{\meta{cond clause}} (else \meta{tail sequence}))

(c\=ase \meta{expression}
  \>\atleastone{\meta{case clause}})
(c\=ase \meta{expression}
  \>\arbno{\meta{case clause}}
  \>(else \meta{tail sequence}))

(and \arbno{\meta{expression}} \meta{tail expression})
(or \arbno{\meta{expression}} \meta{tail expression})

(when \meta{test} \meta{tail sequence})
(unless \meta{test} \meta{tail sequence})

(let (\arbno{\meta{binding spec}}) \meta{tail body})
(let \meta{variable} (\arbno{\meta{binding spec}}) \meta{tail body})
(let* (\arbno{\meta{binding spec}}) \meta{tail body})
(letrec (\arbno{\meta{binding spec}}) \meta{tail body})
(letrec* (\arbno{\meta{binding spec}}) \meta{tail body})
(let-values (\arbno{\meta{mv binding spec}}) \meta{tail body})
(let*-values (\arbno{\meta{mv binding spec}}) \meta{tail body})

(let-syntax (\arbno{\meta{syntax spec}}) \meta{tail body})
(letrec-syntax (\arbno{\meta{syntax spec}}) \meta{tail body})

(begin \meta{tail sequence})

(d\=o \=(\arbno{\meta{iteration spec}})
  \>  \>(\meta{test} \meta{tail sequence})
  \>\arbno{\meta{expression}})

{\rm where}

\meta{cond clause} \: (\meta{test} \meta{tail sequence})
\meta{case clause} \: ((\arbno{\meta{datum}}) \meta{tail sequence})

\meta{tail body} \: \arbno{\meta{definition}} \meta{tail sequence}
\meta{tail sequence} \: \arbno{\meta{expression}} \meta{tail expression}
\end{grammar}

\item
If a ``cond`` or ``case`` expression is in a tail context, and has
a clause of the form ``(\hyperi{expression`` => \hyperii{expression})}
then the (implied) call to
the procedure that results from the evaluation of \hyperii{expression} is in a
tail context.  \hyperii{expression} itself is not in a tail context.




Certain procedures defined in this report are also required to perform tail calls.
The first argument passed to \ide{apply} and to
\ide{call-with-current-continuation}, and the second argument passed to
\ide{call-with-values}, must be called via a tail call.
Similarly, \ide{eval} must evaluate its first argument as if it
were in tail position within the \ide{eval} procedure.

In the following example the only tail call is the call to ``f``.
None of the calls to ``g`` or ``h`` are tail calls.  The reference to
``x`` is in a tail context, but it is not a call and thus is not a
tail call.

```
(lambda ()
  (if (g)
      (let ((x (h)))
        x)
      (and (g) (f))))
```

*Note:&nbsp;*
Implementations may
recognize that some non-tail calls, such as the call to ``h``
above, can be evaluated as though they were tail calls.
In the example above, the ``let`` expression could be compiled
as a tail call to ``h``. (The possibility of ``h`` returning
an unexpected number of values can be ignored, because in that
case the effect of the ``let`` is explicitly unspecified and
implementation-dependent.)
