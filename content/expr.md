+++
weight = 12
title = "4 Expressions"
menu = "main"
+++
# 4. Expressions
{{< label "expressionchapter" >}}

\newcommand{_Syntax:_}{_Syntax: _}
\newcommand{_Semantics:_}{_Semantics: _}

Expression types are categorized as _primitive_ or _derived_.
Primitive expression types include variables and procedure calls.
Derived expression types are not semantically primitive, but can instead
be defined as macros.
Suitable syntax definitions of some of the derived expressions are
given in section~\ref{derivedsection}.

The procedures ``force``, ``promise?``, ``make-promise``, and ``make-parameter``
are also described in this chapter because they are intimately associated
with the ``delay``, ``delay-force``, and ``parameterize`` expression types.

## 4.1. Primitive expression types
{{< label "primitivexps" >}}

### 4.1.1. Variable references

\begin{entry}{
\pproto{{{< hyper "variable" >}}}{syntax}}

An expression consisting of a variable
(section~\ref{variablesection}) is a variable reference.  The value of
the variable reference is the value stored in the location to which the
variable is bound.  It is an error to reference an
unbound variable.

```
(define x 28)
x   \ev  28
```
\end{entry}

### 4.1.2. Literal expressions
{{< label "literalsection" >}}

\begin{entry}{
\proto{quote}{ {{< hyper "datum" >}}}{syntax}
\pproto{\singlequote{{< hyper "datum" >}}}{syntax}
\pproto{{{< hyper "constant" >}}}{syntax}}

``(quote {{< hyper "datum``)" >}} evaluates to {{< hyper "datum" >}}.\mainschindex{'}
{{< hyper "Datum" >}}
can be any external representation of a Scheme object (see
section~\ref{externalreps}).  This notation is used to include literal
constants in Scheme code.

```
(quote a)                     \ev  a
(quote \sharpsign(a b c))     \ev  #(a b c)
(quote (+ 1 2))               \ev  (+ 1 2)
```

``(quote {{< hyper "datum``)" >}} can be abbreviated as
\singlequote{{< hyper "datum" >}}.  The two notations are equivalent in all
respects.

```
'a                   \ev  a
'#(a b c)           \ev  #(a b c)
'()                  \ev  ()
'(+ 1 2)             \ev  (+ 1 2)
'(quote a)           \ev  (quote a)
''a                  \ev  (quote a)
```

Numerical constants, string constants, character constants, vector
constants, bytevector constants, and boolean constants evaluate to
themselves; they need not be quoted.

```
'145932    \ev  145932
145932     \ev  145932
'"abc"     \ev  "abc"
"abc"      \ev  "abc"
'#         \ev  #
#          \ev  #
'#(a 10)   \ev  #(a 10)
#(a 10)    \ev  #(a 10)
'#u8(64 65)\ev  #u8(64 65)
#u8(64 65) \ev  #u8(64 65)
'#t        \ev  #t
#t         \ev  #t
```

As noted in section~\ref{storagemodel}, it is an error to attempt to alter a constant
(i.e.~the value of a literal expression) using a mutation procedure like
``set-car!`` or ``string-set!``.

\end{entry}

### 4.1.3. Procedure calls

\begin{entry}{
\pproto{({{< hyper "operator" >}} \hyperi{operand} ...)}{syntax}}

A procedure call is written by enclosing in parentheses an
expression for the procedure to be called followed by expressions for the arguments to be
passed to it.  The operator and operand expressions are evaluated (in an
unspecified order) and the resulting procedure is passed the resulting
arguments.

```
(+ 3 4)                   \ev  7
((if #f + *) 3 4)         \ev  12
```

The procedures in this document are available as the values of variables exported by the
standard libraries.  For example, the addition and multiplication
procedures in the above examples are the values of the variables ``+``
and ``*`` in the base library.  New procedures are created by evaluating \lambdaexp{}s
(see section~\ref{lambda}).

Procedure calls can return any number of values (see \ide{values} in
section~\ref{proceduresection}).
Most of the procedures defined in this report return one
value or, for procedures such as ``apply``, pass on the values returned
by a call to one of their arguments.
Exceptions are noted in the individual descriptions.


*Note:&nbsp;* In contrast to other dialects of Lisp, the order of
evaluation is unspecified, and the operator expression and the operand
expressions are always evaluated with the same evaluation rules.


*Note:&nbsp;*
Although the order of evaluation is otherwise unspecified, the effect of
any concurrent evaluation of the operator and operand expressions is
constrained to be consistent with some sequential order of evaluation.
The order of evaluation may be chosen differently for each procedure call.


*Note:&nbsp;* In many dialects of Lisp, the empty list, {\tt
()}, is a legitimate expression evaluating to itself.  In Scheme, it is an error.


\end{entry}


### 4.1.4. Procedures
{{< label "lamba" >}}

\begin{entry}{
\proto{lambda}{ {{< hyper "formals" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
{{< hyper "Formals" >}} is a formal arguments list as described below,
and {{< hyper "body" >}} is a sequence of zero or more definitions
followed by one or more expressions.

_Semantics:_
A \lambdaexp{} evaluates to a procedure.  The environment in
effect when the \lambdaexp{} was evaluated is remembered as part of the
procedure.  When the procedure is later called with some actual
arguments, the environment in which the \lambdaexp{} was evaluated will
be extended by binding the variables in the formal argument list to
fresh locations, and the corresponding actual argument values will be stored
in those locations.
(A \defining{fresh} location is one that is distinct from every previously
existing location.)
Next, the expressions in the
body of the lambda expression (which, if it contains definitions,
represents a ``letrec*`` form --- see section~\ref{letrecstar})
will be evaluated sequentially in the extended environment.
The results of the last expression in the body will be returned as
the results of the procedure call.

```
(lambda (x) (+ x x))      \ev  {\em{}a procedure}
((lambda (x) (+ x x)) 4)  \ev  8

(define reverse-subtract
  (lambda (x y) (- y x)))
(reverse-subtract 7 10)         \ev  3

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(add4 6)                        \ev  10
```

{{< hyper "Formals" >}} have one of the following forms:


- {\tt(\hyperi{variable} ...)}:
The procedure takes a fixed number of arguments; when the procedure is
called, the arguments will be stored in fresh locations
that are bound to the corresponding variables.

- {{< hyper "variable" >}}:
The procedure takes any number of arguments; when the procedure is
called, the sequence of actual arguments is converted into a newly
allocated list, and the list is stored in a fresh location
that is bound to
{{< hyper "variable" >}}.

- {\tt(\hyperi{variable} ...{} {{< hyper "variable$_{n" >}}$} {\bf.}\
{{< hyper "variable$_{n+1" >}}$})}:
If a space-delimited period precedes the last variable, then
the procedure takes $n$ or more arguments, where $n$ is the
number of formal arguments before the period (it is an error if there is not
at least one).
The value stored in the binding of the last variable will be a
newly allocated
list of the actual arguments left over after all the other actual
arguments have been matched up against the other formal arguments.


It is an error for a {{< hyper "variable" >}} to appear more than once in
{{< hyper "formals" >}}.

```
((lambda x x) 3 4 5 6)          \ev  (3 4 5 6)
((lambda (x y . z) z)
 3 4 5 6)                       \ev  (5 6)
```

\end{entry}

Each procedure created as the result of evaluating a \lambdaexp{} is
(conceptually) tagged
with a storage location, in order to make \ide{eqv?} and
\ide{eq?} work on procedures (see section~\ref{equivalencesection}).


### 4.1.5. Conditionals

\begin{entry}{
\proto{if}{ {{< hyper "test" >}} {{< hyper "consequent" >}} {{< hyper "alternate" >}}}{syntax}
\rproto{if}{ {{< hyper "test" >}} {{< hyper "consequent" >}}}{syntax}}

_Syntax:_
{{< hyper "Test" >}}, {{< hyper "consequent" >}}, and {{< hyper "alternate" >}} are
expressions.

_Semantics:_
An ``if`` expression is evaluated as follows: first,
{{< hyper "test" >}} is evaluated.  If it yields a true value (see
section~\ref{booleansection}), then {{< hyper "consequent" >}} is evaluated and
its values are returned.  Otherwise {{< hyper "alternate" >}} is evaluated and its
values are returned.  If {{< hyper "test" >}} yields a false value and no
{{< hyper "alternate" >}} is specified, then the result of the expression is
unspecified.

```
(if (> 3 2) 'yes 'no)           \ev  yes
(if (> 2 3) 'yes 'no)           \ev  no
(if (> 3 2)
    (- 3 2)
    (+ 3 2))                    \ev  1
```

\end{entry}


### 4.1.6. Assignments
{{< label "assignment" >}}

\begin{entry}{
\proto{set!}{ {{< hyper "variable" >}} {{< hyper "expression" >}}}{syntax}}

_Semantics:_
{{< hyper "Expression" >}} is evaluated, and the resulting value is stored in
the location to which {{< hyper "variable" >}} is bound.  It is an error if {{< hyper "variable" >}} is not
bound either in some region enclosing the ``set!`` expression
or else globally.
The result of the ``set!`` expression is
unspecified.

```
(define x 2)
(+ x 1)                 \ev  3
(set! x 4)              \ev  \unspecified
(+ x 1)                 \ev  5
```

\end{entry}

### 4.1.7. Inclusion
{{< label "inclusion" >}}
\begin{entry}{
\proto{include}{ \hyperi{string} \hyperii{string} ...}{syntax}
\rproto{include-ci}{ \hyperi{string} \hyperii{string} ...}{syntax}}

_Semantics:_
Both \ide{include} and
\ide{include-ci} take one or more filenames expressed as string literals,
apply an implementation-specific algorithm to find corresponding files,
read the contents of the files in the specified order as if by repeated applications
of ``read``,
and effectively replace the ``include`` or ``include-ci`` expression
with a ``begin`` expression containing what was read from the files.
The difference between the two is that \ide{include-ci} reads each file
as if it began with the {\cf{}#!fold-case} directive, while \ide{include}
does not.

*Note:&nbsp;*
Implementations are encouraged to search for files in the directory
which contains the including file, and to provide a way for users to
specify other directories to search.


\end{entry}

## 4.2. Derived expression types
{{< label "derivedexps" >}}

The constructs in this section are hygienic, as discussed in
section~\ref{macrosection}.
For reference purposes, section~\ref{derivedsection} gives syntax definitions
that will convert most of the constructs described in this section
into the primitive constructs described in the previous section.


### 4.2.1. Conditionals

\begin{entry}{
\proto{cond}{ \hyperi{clause} \hyperii{clause} ...}{syntax}
\pproto{else}{\auxiliarytype}
\pproto{=>}{\auxiliarytype}}

_Syntax:_
{{< hyper "Clauses" >}} take one of two forms, either
```
({{< hyper "test" >}} \hyperi{expression} ...)
```
where {{< hyper "test" >}} is any expression, or
```
({{< hyper "test" >}} => {{< hyper "expression" >}})
```
The last {{< hyper "clause" >}} can be
an "else clause," which has the form
```
(else \hyperi{expression} \hyperii{expression} ...)\rm.
```
\mainschindex{else}
\mainschindex{=>}

_Semantics:_
A ``cond`` expression is evaluated by evaluating the {{< hyper "test" >}}
expressions of successive {{< hyper "clause" >}}s in order until one of them
evaluates to a true value (see
section~\ref{booleansection}).  When a {{< hyper "test" >}} evaluates to a true
value, the remaining {{< hyper "expression" >}}s in its {{< hyper "clause" >}} are
evaluated in order, and the results of the last {{< hyper "expression" >}} in the
{{< hyper "clause" >}} are returned as the results of the entire ``cond``
expression.

If the selected {{< hyper "clause" >}} contains only the
{{< hyper "test" >}} and no {{< hyper "expression" >}}s, then the value of the
{{< hyper "test" >}} is returned as the result.  If the selected {{< hyper "clause" >}} uses the
\ide{=>} alternate form, then the {{< hyper "expression" >}} is evaluated.
It is an error if its value is not a procedure that accepts one argument.  This procedure is then
called on the value of the {{< hyper "test" >}} and the values returned by this
procedure are returned by the ``cond`` expression.

If all {{< hyper "test" >}}s evaluate
to {{< tt "#f" >}}, and there is no else clause, then the result of
the conditional expression is unspecified; if there is an else
clause, then its {{< hyper "expression" >}}s are evaluated in order, and the values of
the last one are returned.

```
(cond ((> 3 2) 'greater)
      ((< 3 2) 'less))         \ev  greater

(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))            \ev  equal

(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else {{< tt "#f" >}}))         \ev  2
```


\end{entry}


\begin{entry}{
\proto{case}{ {{< hyper "key" >}} \hyperi{clause} \hyperii{clause} ...}{syntax}}

_Syntax:_
{{< hyper "Key" >}} can be any expression.  Each {{< hyper "clause" >}} has
the form
```
((\hyperi{datum} ...) \hyperi{expression} \hyperii{expression} ...)\rm,
```
where each {{< hyper "datum" >}} is an external representation of some object.
It is an error if any of the {{< hyper "datum" >}}s are the same anywhere in the expression.
Alternatively, a {{< hyper "clause" >}} can be of the form
```
((\hyperi{datum} ...) => {{< hyper "expression" >}})
```
The last {{< hyper "clause" >}} can be an "else clause," which has one of the forms
```
(else \hyperi{expression} \hyperii{expression} ...)
```
or
```
(else => {{< hyper "expression" >}})\rm.
```
\schindex{else}

_Semantics:_
A ``case`` expression is evaluated as follows.  {{< hyper "Key" >}} is
evaluated and its result is compared against each {{< hyper "datum" >}}.  If the
result of evaluating {{< hyper "key" >}} is the same (in the sense of
``eqv?``; see section~\ref{eqv?}) to a {{< hyper "datum" >}}, then the
expressions in the corresponding {{< hyper "clause" >}} are evaluated in order
and the results of the last expression in the {{< hyper "clause" >}} are
returned as the results of the ``case`` expression.

If the result of
evaluating {{< hyper "key" >}} is different from every {{< hyper "datum" >}}, then if
there is an else clause, its expressions are evaluated and the
results of the last are the results of the ``case`` expression;
otherwise the result of the ``case`` expression is unspecified.

If the selected {{< hyper "clause" >}} or else clause uses the
\ide{=>} alternate form, then the {{< hyper "expression" >}} is evaluated.
It is an error if its value is not a procedure accepting one argument.
This procedure is then
called on the value of the {{< hyper "key" >}} and the values returned by this
procedure are returned by the ``case`` expression.

```
(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))     \ev  composite
(case (car '(c d))
  ((a) 'a)
  ((b) 'b))                     \ev  \unspecified
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else => (lambda (x) x)))     \ev  c
```

\end{entry}


\begin{entry}{
\proto{and}{ \hyperi{test} ...}{syntax}}

_Semantics:_
The {{< hyper "test" >}} expressions are evaluated from left to right, and if
any expression evaluates to {{< tt "#f" >}} (see
section~\ref{booleansection}), then {{< tt "#f" >}} is returned.  Any remaining
expressions are not evaluated.  If all the expressions evaluate to
true values, the values of the last expression are returned.  If there
are no expressions, then {{< tt "#t" >}} is returned.

```
(and (= 2 2) (> 2 1))           \ev  \schtrue
(and (= 2 2) (< 2 1))           \ev  \schfalse
(and 1 2 'c '(f g))             \ev  (f g)
(and)                           \ev  \schtrue
```

\end{entry}


\begin{entry}{
\proto{or}{ \hyperi{test} ...}{syntax}}

_Semantics:_
The {{< hyper "test" >}} expressions are evaluated from left to right, and the value of the
first expression that evaluates to a true value (see
section~\ref{booleansection}) is returned.  Any remaining expressions
are not evaluated.  If all expressions evaluate to {{< tt "#f" >}}
or if there are no expressions, then {{< tt "#f" >}} is returned.

```
(or (= 2 2) (> 2 1))            \ev  \schtrue
(or (= 2 2) (< 2 1))            \ev  \schtrue
(or \schfalse \schfalse \schfalse) \ev  \schfalse
(or (memq 'b '(a b c))
    (/ 3 0))                    \ev  (b c)
```

\end{entry}

\begin{entry}{
\proto{when}{ {{< hyper "test" >}} \hyperi{expression} \hyperii{expression} ...}{syntax}}

_Syntax:_
The {{< hyper "test" >}} is an expression.

_Semantics:_
The test is evaluated, and if it evaluates to a true value,
the expressions are evaluated in order.  The result of the ``when``
expression is unspecified.

```
(when (= 1 1.0)
  (display "1")
  (display "2"))  \ev  \unspecified
 \>_and prints_  12
```
\end{entry}

\begin{entry}{
\proto{unless}{ {{< hyper "test" >}} \hyperi{expression} \hyperii{expression} ...}{syntax}}

_Syntax:_
The {{< hyper "test" >}} is an expression.

_Semantics:_
The test is evaluated, and if it evaluates to {{< tt "#f" >}},
the expressions are evaluated in order.  The result of the ``unless``
expression is unspecified.

```
(unless (= 1 1.0)
  (display "1")
  (display "2"))  \ev  \unspecified
 \>_and prints nothing_
```
\end{entry}

\begin{entry}{
\proto{cond-expand}{ \hyperi{ce-clause} \hyperii{ce-clause} ...}{syntax}}

_Syntax:_
The \ide{cond-expand} expression type
provides a way to statically
expand different expressions depending on the
implementation.  A
{{< hyper "ce-clause" >}} takes the following form:

{\tt({{< hyper "feature requirement" >}} {{< hyper "expression" >}} ...)}

The last clause can be an "else clause," which has the form

{\tt(else {{< hyper "expression" >}} ...)}

A {{< hyper "feature requirement" >}} takes one of the following forms:


- {\tt{{< hyper "feature identifier" >}}}
- {\tt(library {{< hyper "library name" >}})}
- {\tt(and {{< hyper "feature requirement" >}} ...)}
- {\tt(or {{< hyper "feature requirement" >}} ...)}
- {\tt(not {{< hyper "feature requirement" >}})}


_Semantics:_
Each implementation maintains a list of feature identifiers which are
present, as well as a list of libraries which can be imported.  The
value of a {{< hyper "feature requirement" >}} is determined by replacing
each {{< hyper "feature identifier" >}} and {\tt(library {{< hyper "library name" >}})}
on the implementation's lists with \schtrue, and all other feature
identifiers and library names with \schfalse, then evaluating the
resulting expression as a Scheme boolean expression under the normal
interpretation of ``and``, ``or``, and ``not``.

A \ide{cond-expand} is then expanded by evaluating the
{{< hyper "feature requirement" >}}s of successive {{< hyper "ce-clause" >}}s
in order until one of them returns \schtrue.  When a true clause is
found, the corresponding {{< hyper "expression" >}}s are expanded to a
``begin``, and the remaining clauses are ignored.
If none of the {{< hyper "feature requirement" >}}s evaluate to \schtrue, then
if there is an else clause, its {{< hyper "expression" >}}s are
included.  Otherwise, the behavior of the \ide{cond-expand} is unspecified.
Unlike ``cond``, ``cond-expand`` does not depend on the value
of any variables.

The exact features provided are implementation-defined, but for
portability a core set of features is given in
appendix B.

\end{entry}

### 4.2.2. Binding constructs
{{< label "bindingsection" >}}

The binding constructs ``let``, ``let*``, ``letrec``, ``letrec*``,
``let-values``, and ``let*-values``
give Scheme a block structure, like Algol 60.  The syntax of the first four
constructs is identical, but they differ in the regions they establish
for their variable bindings.  In a ``let`` expression, the initial
values are computed before any of the variables become bound; in a
``let*`` expression, the bindings and evaluations are performed
sequentially; while in ``letrec`` and ``letrec*`` expressions,
all the bindings are in
effect while their initial values are being computed, thus allowing
mutually recursive definitions.
The ``let-values`` and ``let*-values`` constructs are analogous to ``let`` and ``let*``
respectively, but are designed to handle multiple-valued expressions, binding
different identifiers to the returned values.

\begin{entry}{
\proto{let}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
{{< hyper "Bindings" >}} has the form
```
((\hyperi{variable} \hyperi{init}) ...)\rm,
```
where each {{< hyper "init" >}} is an expression, and {{< hyper "body" >}} is a
sequence of zero or more definitions followed by a
sequence of one or more expressions as described in section~\ref{lambda}.  It is
an error for a {{< hyper "variable" >}} to appear more than once in the list of variables
being bound.

_Semantics:_
The {{< hyper "init" >}}s are evaluated in the current environment (in some
unspecified order), the {{< hyper "variable" >}}s are bound to fresh locations
holding the results, the {{< hyper "body" >}} is evaluated in the extended
environment, and the values of the last expression of {{< hyper "body" >}}
are returned.  Each binding of a {{< hyper "variable" >}} has {{< hyper "body" >}} as its
region.

```
(let ((x 2) (y 3))
  (* x y))                      \ev  6

(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))                   \ev  35
```

See also "named ``let``," section \ref{namedlet}.

\end{entry}


\begin{entry}{
\proto{let*}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}\nobreak

\nobreak
_Syntax:_
{{< hyper "Bindings" >}} has the form
```
((\hyperi{variable} \hyperi{init}) ...)\rm,
```
and {{< hyper "body" >}} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}.

_Semantics:_
The ``let*`` binding construct is similar to ``let``, but the bindings are performed
sequentially from left to right, and the region of a binding indicated
by {\cf({{< hyper "variable" >}} {{< hyper "init" >}})} is that part of the ``let*``
expression to the right of the binding.  Thus the second binding is done
in an environment in which the first binding is visible, and so on.
The {{< hyper "variable" >}}s need not be distinct.

```
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))             \ev  70
```

\end{entry}


\begin{entry}{
\proto{letrec}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
{{< hyper "Bindings" >}} has the form
```
((\hyperi{variable} \hyperi{init}) ...)\rm,
```
and {{< hyper "body" >}} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}. It is an error for a {{< hyper "variable" >}} to appear more
than once in the list of variables being bound.

_Semantics:_
The {{< hyper "variable" >}}s are bound to fresh locations holding unspecified
values, the {{< hyper "init" >}}s are evaluated in the resulting environment (in
some unspecified order), each {{< hyper "variable" >}} is assigned to the result
of the corresponding {{< hyper "init" >}}, the {{< hyper "body" >}} is evaluated in the
resulting environment, and the values of the last expression in
{{< hyper "body" >}} are returned.  Each binding of a {{< hyper "variable" >}} has the
entire ``letrec`` expression as its region, making it possible to
define mutually recursive procedures.

```
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                \schtrue
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                \schfalse
                (even? (- n 1))))))
  (even? 88))
		\ev  \schtrue
```

One restriction on ``letrec`` is very important: if it is not possible
to evaluate each {{< hyper "init" >}} without assigning or referring to the value of any
{{< hyper "variable" >}}, it is an error.  The
restriction is necessary because
``letrec`` is defined in terms of a procedure
call where a ``lambda`` expression binds the {{< hyper "variable" >}}s to the values
of the {{< hyper "init" >}}s.
In the most common uses of ``letrec``, all the {{< hyper "init" >}}s are
\lambdaexp{}s and the restriction is satisfied automatically.

\end{entry}


\begin{entry}{
\proto{letrec*}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}
{{< label "letrecstar" >}}

_Syntax:_
{{< hyper "Bindings" >}} has the form
```
((\hyperi{variable} \hyperi{init}) ...)\rm,
```
and {{< hyper "body" >}} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}. It is an error for a {{< hyper "variable" >}} to appear more
than once in the list of variables being bound.

_Semantics:_
The {{< hyper "variable" >}}s are bound to fresh locations,
each {{< hyper "variable" >}} is assigned in left-to-right order to the
result of evaluating the corresponding {{< hyper "init" >}}, the {{< hyper "body" >}} is
evaluated in the resulting environment, and the values of the last
expression in {{< hyper "body" >}} are returned.
Despite the left-to-right evaluation and assignment order, each binding of
a {{< hyper "variable" >}} has the entire ``letrec*`` expression as its
region, making it possible to define mutually recursive
procedures.

If it is not possible to evaluate each {{< hyper "init" >}} without assigning or
referring to the value of the corresponding {{< hyper "variable" >}} or the
{{< hyper "variable" >}} of any of the bindings that follow it in
{{< hyper "bindings" >}}, it is an error.
Another restriction is that it is an error to invoke the continuation
of an {{< hyper "init" >}} more than once.

```
(letrec* ((p
           (lambda (x)
             (+ 1 (q (- x 1)))))
          (q
           (lambda (y)
             (if (zero? y)
                 0
                 (+ 1 (p (- y 1))))))
          (x (p 5))
          (y x))
  y)
                \ev  5
```

\begin{entry}{
\proto{let-values}{ {{< hyper "mv binding spec" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
{{< hyper "Mv binding spec" >}} has the form
```
((\hyperi{formals} \hyperi{init}) ...)\rm,
```

where each {{< hyper "init" >}} is an expression, and {{< hyper "body" >}} is
zero or more definitions followed by a sequence of one or
more expressions as described in section~\ref{lambda}.  It is an error for a variable to appear more than
once in the set of {{< hyper "formals" >}}.

_Semantics:_
The {{< hyper "init" >}}s are evaluated in the current environment (in some
unspecified order) as if by invoking ``call-with-values``, and the
variables occurring in the {{< hyper "formals" >}} are bound to fresh locations
holding the values returned by the {{< hyper "init" >}}s, where the
{{< hyper "formals" >}} are matched to the return values in the same way that
the {{< hyper "formals" >}} in a ``lambda`` expression are matched to the
arguments in a procedure call.  Then, the {{< hyper "body" >}} is evaluated in
the extended environment, and the values of the last expression of
{{< hyper "body" >}} are returned.  Each binding of a {{< hyper "variable" >}} has
{{< hyper "body" >}} as its region.

It is an error if the {{< hyper "formals" >}} do not match the number of
values returned by the corresponding {{< hyper "init" >}}.

```
(let-values (((root rem) (exact-integer-sqrt 32)))
  (* root rem))                \ev  35
```

\end{entry}


\begin{entry}{
\proto{let*-values}{ {{< hyper "mv binding spec" >}} {{< hyper "body" >}}}{syntax}}\nobreak

\nobreak
_Syntax:_
{{< hyper "Mv binding spec" >}} has the form
```
(({{< hyper "formals" >}} {{< hyper "init" >}}) ...)\rm,
```
and {{< hyper "body" >}} is a sequence of zero or more
definitions followed by one or more expressions as described in section~\ref{lambda}.  In each {{< hyper "formals" >}},
it is an error if any variable appears more than once.

_Semantics:_
The ``let*-values`` construct is similar to ``let-values``, but the
{{< hyper "init" >}}s are evaluated and bindings created sequentially from
left to right, with the region of the bindings of each {{< hyper "formals" >}}
including the {{< hyper "init" >}}s to its right as well as {{< hyper "body" >}}.  Thus the
second {{< hyper "init" >}} is evaluated in an environment in which the first
set of bindings is visible and initialized, and so on.

```
(let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y)))     \ev (x y x y)
```

\end{entry}

\end{entry}


### 4.2.3. Sequencing
{{< label "sequencing" >}}

Both of Scheme's sequencing constructs are named ``begin``, but the two
have slightly different forms and uses:

\begin{entry}{
\proto{begin}{ {{< hyper "expression or definition" >}} ...}{syntax}}

This form of ``begin`` can appear as part of a {{< hyper "body" >}}, or at the
outermost level of a {{< hyper "program" >}}, or at the REPL, or directly nested
in a ``begin`` that is itself of this form.
It causes the contained expressions and definitions to be evaluated
exactly as if the enclosing ``begin`` construct were not present.

\begin{rationale}
This form is commonly used in the output of
macros (see section~\ref{macrosection})
which need to generate multiple definitions and
splice them into the context in which they are expanded.
\end{rationale}

\end{entry}

\begin{entry}{
\rproto{begin}{ \hyperi{expression} \hyperii{expression} ...}{syntax}}

This form of ``begin`` can be used as an ordinary expression.
The {{< hyper "expression" >}}s are evaluated sequentially from left to right,
and the values of the last {{< hyper "expression" >}} are returned. This
expression type is used to sequence side effects such as assignments
or input and output.

```
(define x 0)

(and (= x 0)
     (begin (set! x 5)
            (+ x 1)))              \ev  6

(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))      \ev  \unspecified
 \>_and prints_  4 plus 1 equals 5
```

\end{entry}

Note that there is a third form of ``begin`` used as a library declaration:
see section~\ref{librarydeclarations}.

### 4.2.4. Iteration

\noindent
\pproto{(do ((\hyperi{variable} \hyperi{init} \hyperi{step})}{syntax}
\mainschindex{do}{\tt\obeyspaces
     ...)\\
    ({{< hyper "test" >}} {{< hyper "expression" >}} ...)\\
  {{< hyper "command" >}} ...)}

_Syntax:_
All of {{< hyper "init" >}}, {{< hyper "step" >}}, {{< hyper "test" >}}, and {{< hyper "command" >}}
are expressions.

_Semantics:_
A ``do`` expression is an iteration construct.  It specifies a set of variables to
be bound, how they are to be initialized at the start, and how they are
to be updated on each iteration.  When a termination condition is met,
the loop exits after evaluating the {{< hyper "expression" >}}s.

A ``do`` expression is evaluated as follows:
The {{< hyper "init" >}} expressions are evaluated (in some unspecified order),
the {{< hyper "variable" >}}s are bound to fresh locations, the results of the
{{< hyper "init" >}} expressions are stored in the bindings of the
{{< hyper "variable" >}}s, and then the iteration phase begins.

Each iteration begins by evaluating {{< hyper "test" >}}; if the result is
false (see section~\ref{booleansection}), then the {{< hyper "command" >}}
expressions are evaluated in order for effect, the {{< hyper "step" >}}
expressions are evaluated in some unspecified order, the
{{< hyper "variable" >}}s are bound to fresh locations, the results of the
{{< hyper "step" >}}s are stored in the bindings of the
{{< hyper "variable" >}}s, and the next iteration begins.

If {{< hyper "test" >}} evaluates to a true value, then the
{{< hyper "expression" >}}s are evaluated from left to right and the values of
the last {{< hyper "expression" >}} are returned.  If no {{< hyper "expression" >}}s
are present, then the value of the ``do`` expression is unspecified.

The region of the binding of a {{< hyper "variable" >}}
consists of the entire ``do`` expression except for the {{< hyper "init" >}}s.
It is an error for a {{< hyper "variable" >}} to appear more than once in the
list of ``do`` variables.

A {{< hyper "step" >}} can be omitted, in which case the effect is the
same as if {\cf({{< hyper "variable" >}} {{< hyper "init" >}} {{< hyper "variable" >}})} had
been written instead of {\cf({{< hyper "variable" >}} {{< hyper "init" >}})}.

```
(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i))          \ev  #(0 1 2 3 4)

(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum)))             \ev  25
```



\begin{entry}{
\rproto{let}{ {{< hyper "variable" >}} {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}

{{< label "namedlet" >}}
_Semantics:_
"Named ``let``" is a variant on the syntax of \ide{let} which provides
a more general looping construct than ``do`` and can also be used to express
recursion.
It has the same syntax and semantics as ordinary ``let``
except that {{< hyper "variable" >}} is bound within {{< hyper "body" >}} to a procedure
whose formal arguments are the bound variables and whose body is
{{< hyper "body" >}}.  Thus the execution of {{< hyper "body" >}} can be repeated by
invoking the procedure named by {{< hyper "variable" >}}.

```
(let loop ((numbers '(3 -2 1 6 -5))
           (nonneg '())
           (neg '()))
  (cond ((null? numbers) (list nonneg neg))
        ((>= (car numbers) 0)
         (loop (cdr numbers)
               (cons (car numbers) nonneg)
               neg))
        ((< (car numbers) 0)
         (loop (cdr numbers)
               nonneg
               (cons (car numbers) neg)))))
  \lev  ((6 1 3) (-5 -2))
```

\end{entry}


### 4.2.5. Delayed evaluation

\begin{entry}{
\proto{delay}{ {{< hyper "expression" >}}}{lazy library syntax}}



_Semantics:_
The ``delay`` construct is used together with the procedure \ide{force} to
implement \defining{lazy evaluation} or \defining{call by need}.
{\tt(delay~{{< hyper "expression" >}})} returns an object called a
\defining{promise} which at some point in the future can be asked (by
the ``force`` procedure) to evaluate
{{< hyper "expression" >}}, and deliver the resulting value.

The effect of {{< hyper "expression" >}} returning multiple values
is unspecified.

\end{entry}

\begin{entry}{
\proto{delay-force}{ {{< hyper "expression" >}}}{lazy library syntax}}



_Semantics:_
The expression ``(delay-force _expression``)_ is conceptually similar to
``(delay (force _expression``))_,
with the difference that forcing the result
of ``delay-force`` will in effect result in a tail call to
``(force _expression``)_,
while forcing the result of
``(delay (force _expression``))_
might not.  Thus
iterative lazy algorithms that might result in a long series of chains of
``delay`` and ``force``
can be rewritten using ``delay-force`` to prevent consuming
unbounded space during evaluation.

\end{entry}

\begin{entry}{
\proto{force}{ promise}{lazy library procedure}}

The ``force`` procedure forces the value of a _promise_ created
by \ide{delay}, \ide{delay-force}, or \ide{make-promise}.
If no value has been computed for the promise, then a value is
computed and returned.  The value of the promise must be cached (or
"memoized") so that if it is forced a second time, the previously
computed value is returned.
Consequently, a delayed expression is evaluated using the parameter
values and exception handler of the call to ``force`` which first
requested its value.
If _promise_ is not a promise, it may be returned unchanged.

```
(force (delay (+ 1 2)))   \ev  3
(let ((p (delay (+ 1 2))))
  (list (force p) (force p)))
                               \ev  (3 3)

(define integers
  (letrec ((next
            (lambda (n)
              (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))

(head (tail (tail integers)))
                               \ev  2
```

The following example is a mechanical transformation of a lazy
stream-filtering algorithm into Scheme.  Each call to a constructor is
wrapped in ``delay``, and each argument passed to a deconstructor is
wrapped in ``force``.  The use of ``(delay-force ...)`` instead of {\cf
(delay (force ...))} around the body of the procedure ensures that an
ever-growing sequence of pending promises does not
exhaust available storage,
because ``force`` will in effect force such sequences iteratively.

```
(define (stream-filter p? s)
  (delay-force
   (if (null? (force s))
       (delay '())
       (let ((h (car (force s)))
             (t (cdr (force s))))
         (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))

(head (tail (tail (stream-filter odd? integers))))
                               \ev 5
```

The following examples are not intended to illustrate good programming
style, as ``delay``, ``force``, and ``delay-force`` are mainly intended
for programs written in the functional style.
However, they do illustrate the property that only one value is
computed for a promise, no matter how many times it is forced.

```
(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
p                     \ev  {\it{}a promise}
(force p)             \ev  6
p                     \ev  {\it{}a promise, still}
(begin (set! x 10)
       (force p))     \ev  6
```

Various extensions to this semantics of ``delay``, ``force`` and
``delay-force`` are supported in some implementations:


- Calling ``force`` on an object that is not a promise may simply
return the object.

- It may be the case that there is no means by which a promise can be
operationally distinguished from its forced value.  That is, expressions
like the following may evaluate to either {{< tt "#t" >}} or to {{< tt "#f" >}},
depending on the implementation:

```
(eqv? (delay 1) 1)          \ev  \unspecified
(pair? (delay (cons 1 2)))  \ev  \unspecified
```

- Implementations may implement "implicit forcing," where
the value of a promise is forced by procedures
that operate only on arguments of a certain type, like ``cdr``
and ``*``.  However, procedures that operate uniformly on their
arguments, like ``list``, must not force them.

```
(+ (delay (* 3 7)) 13)  \ev  \unspecified
(car
  (list (delay (* 3 7)) 13))    \ev {\it{}a promise}
```

\end{entry}

\begin{entry}{
\proto{promise?} { _obj_}{lazy library procedure}}

The ``promise?`` procedure returns
{{< tt "#t" >}} if its argument is a promise, and {{< tt "#f" >}} otherwise.  Note
that promises are not necessarily disjoint from other Scheme types such
as procedures.

\end{entry}

\begin{entry}{
\proto{make-promise} { _obj_}{lazy library procedure}}

The ``make-promise`` procedure returns a promise which, when forced, will return
_obj_.  It is similar to ``delay``, but does not delay
its argument: it is a procedure rather than syntax.
If _obj_ is already a promise, it is returned.

\end{entry}

### 4.2.6. Dynamic bindings

The \defining{dynamic extent} of a procedure call is the time between
when it is initiated and when it returns.  In Scheme, {\cf
  call-with-current-continuation} (section~\ref{continuations}) allows
reentering a dynamic extent after its procedure call has returned.
Thus, the dynamic extent of a call might not be a single, continuous time
period.

This sections introduces \defining{parameter objects}, which can be
bound to new values for the duration of a dynamic extent.  The set of
all parameter bindings at a given time is called the \defining{dynamic
  environment}.

\begin{entry}{
\proto{make-parameter}{ init}{procedure}
\rproto{make-parameter}{ init converter}{procedure}}

Returns a newly allocated parameter object,
which is a procedure that accepts zero arguments and
returns the value associated with the parameter object.
Initially, this value is the value of
``(_converter`` _init_)_, or of _init_
if the conversion procedure _converter_ is not specified.
The associated value can be temporarily changed
using ``parameterize``, which is described below.

The effect of passing arguments to a parameter object is
implementation-dependent.
\end{entry}

\begin{entry}{
\pproto{(parameterize ((\hyperi{param} \hyperi{value}) ...)}{syntax}
{\tt\obeyspaces
\hspace*{1em}{{< hyper "body" >}})}}
\mainschindex{parameterize}

_Syntax:_
Both \hyperi{param} and \hyperi{value} are expressions.

\domain{It is an error if the value of any {{< hyper "param" >}} expression is not a parameter object.}
_Semantics:_
A ``parameterize`` expression is used to change the values returned by
specified parameter objects during the evaluation of the body.

The {{< hyper "param" >}} and {{< hyper "value" >}} expressions
are evaluated in an unspecified order.  The {{< hyper "body" >}} is
evaluated in a dynamic environment in which calls to the
parameters return the results of passing the corresponding values
to the conversion procedure specified when the parameters were created.
Then the previous values of the parameters are restored without passing
them to the conversion procedure.
The results of the last
expression in the {{< hyper "body" >}} are returned as the results of the entire
``parameterize`` expression.

*Note:&nbsp;*
If the conversion procedure is not idempotent, the results of
``(parameterize ((x (x))) ...)``,
which appears to bind the parameter _x_ to its current value,
might not be what the user expects.


If an implementation supports multiple threads of execution, then
``parameterize`` must not change the associated values of any parameters
in any thread other than the current thread and threads created
inside {{< hyper "body" >}}.

Parameter objects can be used to specify configurable settings for a
computation without the need to pass the value to every
procedure in the call chain explicitly.

```
(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (exact-integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))

(define (f n) (number->string n (radix)))

(f 12)                                       \ev "12"
(parameterize ((radix 2))
  (f 12))                                    \ev "1100"
(f 12)                                       \ev "12"

(radix 16)                                   \ev \unspecified

(parameterize ((radix 0))
  (f 12))                                    \ev \scherror
```
\end{entry}


### 4.2.7. Exception handling

\begin{entry}{
\pproto{(guard ({{< hyper "variable" >}}}{syntax}
{\tt\obeyspaces
\hspace*{4em}\hyperi{cond clause} \hyperii{cond clause} ...)\\
\hspace*{2em}{{< hyper "body" >}})}\\
}
\mainschindex{guard}

_Syntax:_
Each {{< hyper "cond clause" >}} is as in the specification of ``cond``.

_Semantics:_
The {{< hyper "body" >}} is evaluated with an exception
handler that binds the raised object (see \ide{raise} in section~\ref{exceptionsection})
to {{< hyper "variable" >}} and, within the scope of
that binding, evaluates the clauses as if they were the clauses of a
``cond`` expression. That implicit ``cond`` expression is evaluated with the
continuation and dynamic environment of the ``guard`` expression. If every
{{< hyper "cond clause" >}}'s {{< hyper "test" >}} evaluates to {{< tt "#f" >}} and there
is no else clause, then
``raise-continuable`` is invoked on the raised object within the dynamic
environment of the original call to ``raise``
or ``raise-continuable``, except that the current
exception handler is that of the ``guard`` expression.


See section~\ref{exceptionsection} for a more complete discussion of
exceptions.

```
(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'a 42))))
\ev 42

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'b 23))))
\ev (b . 23)
```
\end{entry}


### 4.2.8. Quasiquotation
{{< label "quasiquotesection" >}}

\begin{entry}{
\proto{quasiquote}{ {{< hyper "qq template" >}}}{syntax} \nopagebreak
\pproto{\backquote{{< hyper "qq template" >}}}{syntax}
\pproto{unquote}{\auxiliarytype}
\pproto{\comma}{\auxiliarytype}
\pproto{unquote-splicing}{\auxiliarytype}
\pproto{\commaatsign}{\auxiliarytype}}

"Quasiquote" expressions are useful
for constructing a list or vector structure when some but not all of the
desired structure is known in advance.  If no
commas appear within the {{< hyper "qq template" >}}, the result of
evaluating
\backquote{{< hyper "qq template" >}} is equivalent to the result of evaluating
\singlequote{{< hyper "qq template" >}}.  If a comma\mainschindex{,} appears within the
{{< hyper "qq template" >}}, however, the expression following the comma is
evaluated ("unquoted") and its result is inserted into the structure
instead of the comma and the expression.  If a comma appears followed
without intervening whitespace by a commercial at-sign (\atsign),\mainschindex{,@} then it is an error if the following
expression does not evaluate to a list; the opening and closing parentheses
of the list are then "stripped away" and the elements of the list are
inserted in place of the comma at-sign expression sequence.  A comma
at-sign normally appears only within a list or vector {{< hyper "qq template" >}}.

*Note:&nbsp;*
In order to unquote an identifier beginning with ``@``, it is necessary
to use either an explicit ``unquote`` or to put whitespace after the comma,
to avoid colliding with the comma at-sign sequence.


```
`(list ,(+ 1 2) 4)  \ev  (list 3 4)
(let ((name 'a)) `(list ,name ',name))
          \lev  (list a (quote a))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
          \lev  (a 3 4 5 6 b)
`((``foo`` ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
          \lev  ((foo 7) . cons)
`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
          \lev  #(10 5 2 4 3 8)
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))
          \lev  (list foo bar baz)
```

Quasiquote expressions can be nested.  Substitutions are made only for
unquoted components appearing at the same nesting level
as the outermost quasiquote.  The nesting level increases by one inside
each successive quasiquotation, and decreases by one inside each
unquotation.

```
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
          \lev  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))
          \lev  (a `(b ,x ,'y d) e)
```

A quasiquote expression may return either newly allocated, mutable objects or
literal structure for any structure that is constructed at run time
during the evaluation of the expression. Portions that do not need to
be rebuilt are always literal. Thus,

```
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))
```

may be treated as equivalent to either of the following expressions:

```
`((1 2) 3 4 five 6)

(let ((a 3))
  (cons '(1 2)
        (cons a (cons 4 (cons 'five '(6))))))
```

However, it is not equivalent to this expression:

```
(let ((a 3)) (list (list 1 2) a 4 'five 6))
```

The two notations
 \backquote{{< hyper "qq template" >}} and {\tt (quasiquote {{< hyper "qq template" >}})}
 are identical in all respects.
 {\cf,{{< hyper "expression" >}}} is identical to ``(unquote {{< hyper "expression``)" >}},
 and
 {\cf,@{{< hyper "expression" >}}} is identical to ``(unquote-splicing {{< hyper "expression``)" >}}.
The \ide{write} procedure may output either format.
\mainschindex{`}

```
(quasiquote (list (unquote (+ 1 2)) 4))
          \lev  (list 3 4)
'(quasiquote (list (unquote (+ 1 2)) 4))
          \lev  `(list ,(+ 1 2) 4)
     {\em{}i.e.,} (quasiquote (list (unquote (+ 1 2)) 4))
```


It is an error if any of the identifiers ``quasiquote``, ``unquote``,
or ``unquote-splicing`` appear in positions within a {{< hyper "qq template" >}}
otherwise than as described above.

\end{entry}

### 4.2.9. Case-lambda
{{< label "caselambdasection" >}}
\begin{entry}{
\proto{case-lambda}{ {{< hyper "clause" >}} ...}{case-lambda library syntax}}

_Syntax:_
Each {{< hyper "clause" >}} is of the form
({{< hyper "formals" >}} {{< hyper "body" >}}),
where {{< hyper "formals" >}} and {{< hyper "body" >}} have the same syntax
as in a \lambdaexp.

_Semantics:_
A ``case-lambda`` expression evaluates to a procedure that accepts
a variable number of arguments and is lexically scoped in the same
manner as a procedure resulting from a \lambdaexp. When the procedure
is called, the first {{< hyper "clause" >}} for which the arguments agree
with {{< hyper "formals" >}} is selected, where agreement is specified as for
the {{< hyper "formals" >}} of a \lambdaexp. The variables of {{< hyper "formals" >}} are
bound to fresh locations, the values of the arguments are stored in those
locations, the {{< hyper "body" >}} is evaluated in the extended environment,
and the results of {{< hyper "body" >}} are returned as the results of the
procedure call.

It is an error for the arguments not to agree with
the {{< hyper "formals" >}} of any {{< hyper "clause" >}}.

```
(define range
  (case-lambda
   ((e) (range 0 e))
   ((b e) (do ((r '() (cons e r))
               (e (- e 1) (- e 1)))
              ((< e b) r)))))

(range 3)    \ev (0 1 2)
(range 3 5)  \ev (3 4)
```

\end{entry}

## 4.3. Macros
{{< label "macrosection" >}}

Scheme programs can define and use new derived expression types,
 called _macros_.
Program-defined expression types have the syntax
```
({{< hyper "keyword" >}} {{{< hyper "datum" >}}} ...)
```
where {{< hyper "keyword" >}} is an identifier that uniquely determines the
expression type.  This identifier is called the _syntactic
keyword_, or simply {\em
keyword}, of the macro.  The
number of the {{< hyper "datum" >}}s, and their syntax, depends on the
expression type.

Each instance of a macro is called a _use_
of the macro.
The set of rules that specifies
how a use of a macro is transcribed into a more primitive expression
is called the _transformer_
of the macro.

The macro definition facility consists of two parts:


- A set of expressions used to establish that certain identifiers
are macro keywords, associate them with macro transformers, and control
the scope within which a macro is defined, and

- a pattern language for specifying macro transformers.


The syntactic keyword of a macro can shadow variable bindings, and local
variable bindings can shadow syntactic bindings.
Two mechanisms are provided to prevent unintended conflicts:



- If a macro transformer inserts a binding for an identifier
(variable or keyword), the identifier will in effect be renamed
throughout its scope to avoid conflicts with other identifiers.
Note that a global variable definition may or may not introduce a binding;
see section~\ref{defines}.

- If a macro transformer inserts a free reference to an
identifier, the reference refers to the binding that was visible
where the transformer was specified, regardless of any local
bindings that surround the use of the macro.



In consequence, all macros
defined using the pattern language  are "hygienic" and "referentially
transparent" and thus preserve Scheme's lexical scoping. {{< cite "Kohlbecker86,hygienic,Bawden88,macrosthatwork,syntacticabstraction" >}}


Implementations may provide macro facilities of other types.

### 4.3.1. Binding constructs for syntactic keywords
{{< label "bindsyntax" >}}

The ``let-syntax`` and ``letrec-syntax`` binding constructs are
analogous to ``let`` and ``letrec``, but they bind
syntactic keywords to macro transformers instead of binding variables
to locations that contain values.  Syntactic keywords can also be
bound globally or locally with ``define-syntax``;
see section~\ref{define-syntax}.

\begin{entry}{
\proto{let-syntax}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
{{< hyper "Bindings" >}} has the form
```
(({{< hyper "keyword" >}} {{< hyper "transformer spec" >}}) ...)
```
Each {{< hyper "keyword" >}} is an identifier,
each {{< hyper "transformer spec" >}} is an instance of ``syntax-rules``, and
{{< hyper "body" >}} is a sequence of one or more definitions followed
by one or more expressions.  It is an error
for a {{< hyper "keyword" >}} to appear more than once in the list of keywords
being bound.

_Semantics:_
The {{< hyper "body" >}} is expanded in the syntactic environment
obtained by extending the syntactic environment of the
``let-syntax`` expression with macros whose keywords are
the {{< hyper "keyword" >}}s, bound to the specified transformers.
Each binding of a {{< hyper "keyword" >}} has {{< hyper "body" >}} as its region.

```
(let-syntax ((given-that (syntax-rules ()
                     ((given-that test stmt1 stmt2 ...)
                      (if test
                          (begin stmt1
                                 stmt2 ...))))))
  (let ((if \schtrue))
    (given-that if (set! if 'now))
    if))                           \ev  now

(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))                       \ev  outer
```

\end{entry}

\begin{entry}{
\proto{letrec-syntax}{ {{< hyper "bindings" >}} {{< hyper "body" >}}}{syntax}}

_Syntax:_
Same as for ``let-syntax``.

_Semantics:_
 The {{< hyper "body" >}} is expanded in the syntactic environment obtained by
extending the syntactic environment of the ``letrec-syntax``
expression with macros whose keywords are the
{{< hyper "keyword" >}}s, bound to the specified transformers.
Each binding of a {{< hyper "keyword" >}} has the {{< hyper "transformer spec" >}}s
as well as the {{< hyper "body" >}} within its region,
so the transformers can
transcribe expressions into uses of the macros
introduced by the ``letrec-syntax`` expression.

```
(letrec-syntax
    ((my-or (syntax-rules ()
              ((my-or) \schfalse)
              ((my-or e) e)
              ((my-or e1 e2 ...)
               (let ((temp e1))
                 (if temp
                     temp
                     (my-or e2 ...)))))))
  (let ((x \schfalse)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y)))        \ev  7
```

\end{entry}

### 4.3.2. Pattern language
{{< label "patternlanguage" >}}

A {{< hyper "transformer spec" >}} has one of the following forms:

\begin{entry}{
\pproto{(syntax-rules ({{< hyper "literal" >}} ...)}{syntax}
{\tt\obeyspaces
\hspace*{1em}{{< hyper "syntax rule" >}} ...)\\
}
\pproto{(syntax-rules {{< hyper "ellipsis" >}} ({{< hyper "literal" >}} ...)}{syntax}
{\tt\obeyspaces
\hspace*{1em}{{< hyper "syntax rule" >}} ...)}\\
\pproto{_}{\auxiliarytype}
\pproto{...}{\auxiliarytype}}
\mainschindex{_}

_Syntax:_
It is an error if any of the {{< hyper "literal" >}}s, or the {{< hyper "ellipsis" >}} in the second form,
is not an identifier.
It is also an error if
{{< hyper "syntax rule" >}} is not of the form
```
({{< hyper "pattern" >}} {{< hyper "template" >}})
```
The {{< hyper "pattern" >}} in a {{< hyper "syntax rule" >}} is a list {{< hyper "pattern" >}}
whose first element is an identifier.

A {{< hyper "pattern" >}} is either an identifier, a constant, or one of the
following
```
({{< hyper "pattern" >}} \ldots)
({{< hyper "pattern" >}} {{< hyper "pattern" >}} \ldots . {{< hyper "pattern" >}})
({{< hyper "pattern" >}} \ldots {{< hyper "pattern" >}} {{< hyper "ellipsis" >}} {{< hyper "pattern" >}} \ldots)
({{< hyper "pattern" >}} \ldots {{< hyper "pattern" >}} {{< hyper "ellipsis" >}} {{< hyper "pattern" >}} \ldots
  . {{< hyper "pattern" >}})
#({{< hyper "pattern" >}} \ldots)
#({{< hyper "pattern" >}} \ldots {{< hyper "pattern" >}} {{< hyper "ellipsis" >}} {{< hyper "pattern" >}} \ldots)
```
and a {{< hyper "template" >}} is either an identifier, a constant, or one of the following
```
({{< hyper "element" >}} \ldots)
({{< hyper "element" >}} {{< hyper "element" >}} \ldots . {{< hyper "template" >}})
({{< hyper "ellipsis" >}} {{< hyper "template" >}})
#({{< hyper "element" >}} \ldots)
```
where an {{< hyper "element" >}} is a {{< hyper "template" >}} optionally
followed by an {{< hyper "ellipsis" >}}.
An {{< hyper "ellipsis" >}} is the identifier specified in the second form
of ``syntax-rules``, or the default identifier ``...``
(three consecutive periods) otherwise.\schindex{...}

_Semantics:_ An instance of ``syntax-rules`` produces a new macro
transformer by specifying a sequence of hygienic rewrite rules.  A use
of a macro whose keyword is associated with a transformer specified by
``syntax-rules`` is matched against the patterns contained in the
{{< hyper "syntax rule" >}}s, beginning with the leftmost {{< hyper "syntax rule" >}}.
When a match is found, the macro use is transcribed hygienically
according to the template.

An identifier appearing within a {{< hyper "pattern" >}} can be an underscore
(``_``), a literal identifier listed in the list of {{< hyper "literal" >}}s,
or the {{< hyper "ellipsis" >}}.
All other identifiers appearing within a {{< hyper "pattern" >}} are
_pattern variables_.

The keyword at the beginning of the pattern in a
{{< hyper "syntax rule" >}} is not involved in the matching and
is considered neither a pattern variable nor a literal identifier.

Pattern variables match arbitrary input elements and
are used to refer to elements of the input in the template.
It is an error for the same pattern variable to appear more than once in a
{{< hyper "pattern" >}}.

Underscores also match arbitrary input elements but are not pattern variables
and so cannot be used to refer to those elements.  If an underscore appears
in the {{< hyper "literal" >}}s list, then that takes precedence and
underscores in the {{< hyper "pattern" >}} match as literals.
Multiple underscores can appear in a {{< hyper "pattern" >}}.

Identifiers that appear in \texttt{({{< hyper "literal" >}} ...)} are
interpreted as literal
identifiers to be matched against corresponding elements of the input.
An element in the input matches a literal identifier if and only if it is an
identifier and either both its occurrence in the macro expression and its
occurrence in the macro definition have the same lexical binding, or
the two identifiers are the same and both have no lexical binding.

A subpattern followed by {{< hyper "ellipsis" >}} can match zero or more elements of
the input, unless {{< hyper "ellipsis" >}} appears in the {{< hyper "literal" >}}s, in which
case it is matched as a literal.

More formally, an input expression $E$ matches a pattern $P$ if and only if:


- $P$ is an underscore (``_``).

- $P$ is a non-literal identifier; or

- $P$ is a literal identifier and $E$ is an identifier with the same
      binding; or

- $P$ is a list ``($P_1$ $\dots$ $P_n$)`` and $E$ is a
      list of $n$
      elements that match $P_1$ through $P_n$, respectively; or

- $P$ is an improper list
      ``($P_1$ $P_2$ $\dots$ $P_n$ . $P_{n+1``$)}
      and $E$ is a list or
      improper list of $n$ or more elements that match $P_1$ through $P_n$,
      respectively, and whose $n$th tail matches $P_{n+1}$; or

- $P$ is of the form
      ``($P_1$ $\dots$ $P_k$ $P_e$ \meta{ellipsis`` $P_{m+1}$ ...{} $P_n$)}
      where $E$ is
      a proper list of $n$ elements, the first $k$ of which match
      $P_1$ through $P_k$, respectively,
      whose next $m-k$ elements each match $P_e$,
      whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$; or

- $P$ is of the form
      ``($P_1$ $\dots$ $P_k$ $P_{e``$ \meta{ellipsis} $P_{m+1}$ ...{} $P_n$ . $P_x$)}
      where $E$ is
      a list or improper list of $n$ elements, the first $k$ of which match
      $P_1$ through $P_k$,
      whose next $m-k$ elements each match $P_e$,
      whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$,
      and whose $n$th and final cdr matches $P_x$; or

- $P$ is a vector of the form ``#($P_1$ $\dots$ $P_n$)``
      and $E$ is a vector
      of $n$ elements that match $P_1$ through $P_n$; or

- $P$ is of the form
      ``#($P_1$ $\dots$ $P_k$ $P_{e``$ \meta{ellipsis} $P_{m+1}$ ... $P_n$)}
      where $E$ is a vector of $n$
      elements the first $k$ of which match $P_1$ through $P_k$,
      whose next $m-k$ elements each match $P_e$,
      and whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$; or

- $P$ is a constant and $E$ is equal to $P$ in the sense of
      the ``equal?`` procedure.


It is an error to use a macro keyword, within the scope of its
binding, in an expression that does not match any of the patterns.

When a macro use is transcribed according to the template of the
matching {{< hyper "syntax rule" >}}, pattern variables that occur in the
template are replaced by the elements they match in the input.
Pattern variables that occur in subpatterns followed by one or more
instances of the identifier
{{< hyper "ellipsis" >}} are allowed only in subtemplates that are
followed by as many instances of {{< hyper "ellipsis" >}}.
They are replaced in the
output by all of the elements they match in the input, distributed as
indicated.  It is an error if the output cannot be built up as
specified.


Identifiers that appear in the template but are not pattern variables
or the identifier
{{< hyper "ellipsis" >}} are inserted into the output as literal identifiers.  If a
literal identifier is inserted as a free identifier then it refers to the
binding of that identifier within whose scope the instance of
``syntax-rules`` appears.
If a literal identifier is inserted as a bound identifier then it is
in effect renamed to prevent inadvertent captures of free identifiers.

A template of the form
``({{< hyper "ellipsis`` {{< hyper "template" >" >}}})} is identical to {{< hyper "template" >}},
except that
ellipses within the template have no special meaning.
That is, any ellipses contained within {{< hyper "template" >}} are
treated as ordinary identifiers.
In particular, the template ``({{< hyper "ellipsis`` {{< hyper "ellipsis" >" >}}})} produces
a single {{< hyper "ellipsis" >}}.
This allows syntactic abstractions to expand into code containing
ellipses.

```
(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))

(be-like-begin sequence)
(sequence 1 2 3 4) \ev 4
```

As an example, if \ide{let} and \ide{cond} are defined as in
section~\ref{derivedsection} then they are hygienic (as required) and
the following is not an error.

```
(let ((=> \schfalse))
  (cond (\schtrue => 'ok)))           \ev ok
```

The macro transformer for ``cond`` recognizes ``=>``
as a local variable, and hence an expression, and not as the
base identifier ``=>``, which the macro transformer treats
as a syntactic keyword.  Thus the example expands into

```
(let ((=> \schfalse))
  (if \schtrue (begin => 'ok)))
```

instead of

```
(let ((=> \schfalse))
  (let ((temp \schtrue))
    (if temp ('ok temp))))
```

which would result in an invalid procedure call.

\end{entry}

### 4.3.3. Signaling errors in macro transformers


\begin{entry}{
\pproto{(syntax-error {{< hyper "message" >}} {{< hyper "args" >}} ...)}{syntax}}
\mainschindex{syntax-error}

``syntax-error`` behaves similarly to ``error`` (\ref{exceptionsection}) except that implementations
with an expansion pass separate from evaluation should signal an error
as soon as ``syntax-error`` is expanded.  This can be used as
a ``syntax-rules`` {{< hyper "template" >}} for a {{< hyper "pattern" >}} that is
an invalid use of the macro, which can provide more descriptive error
messages.  {{< hyper "message" >}} is a string literal, and {{< hyper "args" >}}
arbitrary expressions providing additional information.
Applications cannot count on being able to catch syntax errors with
exception handlers or guards.

```
(define-syntax simple-let
  (syntax-rules ()
    ((_ (head ... ((x . y) val) . tail)
        body1 body2 ...)
     (syntax-error
      "expected an identifier but got"
      (x . y)))
    ((_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
       val ...))))
```

\end{entry}
