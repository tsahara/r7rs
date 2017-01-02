+++
weight = 12
title = "Expressions"
menu = "main"
chapter = 4
+++
# 4. Expressions
\label{expressionchapter}

\newcommand{\syntax}{{\em Syntax: }}
\newcommand{\semantics}{{\em Semantics: }}

Expression types are categorized as {\em primitive} or {\em derived}.
Primitive expression types include variables and procedure calls.
Derived expression types are not semantically primitive, but can instead
be defined as macros.
Suitable syntax definitions of some of the derived expressions are
given in section~\ref{derivedsection}.

The procedures {\cf force}, {\cf promise?}, {\cf make-promise}, and {\cf make-parameter}
are also described in this chapter because they are intimately associated
with the {\cf delay}, {\cf delay-force}, and {\cf parameterize} expression types.

## 4.1. Primitive expression types
\label{primitivexps}

### 4.1.1. Variable references\unsection

\begin{entry}{%
\pproto{\hyper{variable}}{\exprtype}}

An expression consisting of a variable\index{variable}
(section~\ref{variablesection}) is a variable reference.  The value of
the variable reference is the value stored in the location to which the
variable is bound.  It is an error to reference an
unbound\index{unbound} variable.

\begin{scheme}
(define x 28)
x   \ev  28%
\end{scheme}
\end{entry}

### 4.1.2. Literal expressions\unsection
\label{literalsection}

\begin{entry}{%
\proto{quote}{ \hyper{datum}}{\exprtype}
\pproto{\singlequote\hyper{datum}}{\exprtype}
\pproto{\hyper{constant}}{\exprtype}}

{\cf (quote \hyper{datum})} evaluates to \hyper{datum}.\mainschindex{'}
\hyper{Datum}
can be any external representation of a Scheme object (see
section~\ref{externalreps}).  This notation is used to include literal
constants in Scheme code.

\begin{scheme}%
(quote a)                     \ev  a
(quote \sharpsign(a b c))     \ev  \#(a b c)
(quote (+ 1 2))               \ev  (+ 1 2)%
\end{scheme}

{\cf (quote \hyper{datum})} can be abbreviated as
\singlequote\hyper{datum}.  The two notations are equivalent in all
respects.

\begin{scheme}
'a                   \ev  a
'\#(a b c)           \ev  \#(a b c)
'()                  \ev  ()
'(+ 1 2)             \ev  (+ 1 2)
'(quote a)           \ev  (quote a)
''a                  \ev  (quote a)%
\end{scheme}

Numerical constants, string constants, character constants, vector
constants, bytevector constants, and boolean constants evaluate to
themselves; they need not be quoted.

\begin{scheme}
'145932    \ev  145932
145932     \ev  145932
'"abc"     \ev  "abc"
"abc"      \ev  "abc"
'\#\space   \ev  \#\space
\#\space   \ev  \#\space
'\#(a 10)  \ev  \#(a 10)
\#(a 10)  \ev  \#(a 10)
'\#u8(64 65)  \ev  \#u8(64 65)
\#u8(64 65)  \ev  \#u8(64 65)
'\schtrue  \ev  \schtrue
\schtrue   \ev  \schtrue%
\end{scheme}

As noted in section~\ref{storagemodel}, it is an error to attempt to alter a constant
(i.e.~the value of a literal expression) using a mutation procedure like
{\cf set-car!}\ or {\cf string-set!}.

\end{entry}

### 4.1.3. Procedure calls\unsection

\begin{entry}{%
\pproto{(\hyper{operator} \hyperi{operand} \dotsfoo)}{\exprtype}}

A procedure call is written by enclosing in parentheses an
expression for the procedure to be called followed by expressions for the arguments to be
passed to it.  The operator and operand expressions are evaluated (in an
unspecified order) and the resulting procedure is passed the resulting
arguments.\mainindex{call}\mainindex{procedure call}
\begin{scheme}%
(+ 3 4)                          \ev  7
((if \schfalse + *) 3 4)         \ev  12%
\end{scheme}

The procedures in this document are available as the values of variables exported by the
standard libraries.  For example, the addition and multiplication
procedures in the above examples are the values of the variables {\cf +}
and {\cf *} in the base library.  New procedures are created by evaluating \lambdaexp{}s
(see section~\ref{lambda}).

Procedure calls can return any number of values (see \ide{values} in
section~\ref{proceduresection}).
Most of the procedures defined in this report return one
value or, for procedures such as {\cf apply}, pass on the values returned
by a call to one of their arguments.
Exceptions are noted in the individual descriptions.


\begin{note} In contrast to other dialects of Lisp, the order of
evaluation is unspecified, and the operator expression and the operand
expressions are always evaluated with the same evaluation rules.
\end{note}

\begin{note}
Although the order of evaluation is otherwise unspecified, the effect of
any concurrent evaluation of the operator and operand expressions is
constrained to be consistent with some sequential order of evaluation.
The order of evaluation may be chosen differently for each procedure call.
\end{note}

\begin{note} In many dialects of Lisp, the empty list, {\tt
()}, is a legitimate expression evaluating to itself.  In Scheme, it is an error.
\end{note}

\end{entry}


### 4.1.4. Procedures\unsection
\label{lamba}

\begin{entry}{%
\proto{lambda}{ \hyper{formals} \hyper{body}}{\exprtype}}

\syntax
\hyper{Formals} is a formal arguments list as described below,
and \hyper{body} is a sequence of zero or more definitions
followed by one or more expressions.

\semantics
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
represents a {\cf letrec*} form --- see section~\ref{letrecstar})
will be evaluated sequentially in the extended environment.
The results of the last expression in the body will be returned as
the results of the procedure call.

\begin{scheme}
(lambda (x) (+ x x))      \ev  {\em{}a procedure}
((lambda (x) (+ x x)) 4)  \ev  8

(define reverse-subtract
  (lambda (x y) (- y x)))
(reverse-subtract 7 10)         \ev  3

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(add4 6)                        \ev  10%
\end{scheme}

\hyper{Formals} have one of the following forms:

\begin{itemize}
\item {\tt(\hyperi{variable} \dotsfoo)}:
The procedure takes a fixed number of arguments; when the procedure is
called, the arguments will be stored in fresh locations
that are bound to the corresponding variables.

\item \hyper{variable}:
The procedure takes any number of arguments; when the procedure is
called, the sequence of actual arguments is converted into a newly
allocated list, and the list is stored in a fresh location
that is bound to
\hyper{variable}.

\item {\tt(\hyperi{variable} \dotsfoo{} \hyper{variable$_{n}$}\ {\bf.}\
\hyper{variable$_{n+1}$})}:
If a space-delimited period precedes the last variable, then
the procedure takes $n$ or more arguments, where $n$ is the
number of formal arguments before the period (it is an error if there is not
at least one).
The value stored in the binding of the last variable will be a
newly allocated
list of the actual arguments left over after all the other actual
arguments have been matched up against the other formal arguments.
\end{itemize}

It is an error for a \hyper{variable} to appear more than once in
\hyper{formals}.

\begin{scheme}
((lambda x x) 3 4 5 6)          \ev  (3 4 5 6)
((lambda (x y . z) z)
 3 4 5 6)                       \ev  (5 6)%
\end{scheme}

\end{entry}

Each procedure created as the result of evaluating a \lambdaexp{} is
(conceptually) tagged
with a storage location, in order to make \ide{eqv?} and
\ide{eq?} work on procedures (see section~\ref{equivalencesection}).


### 4.1.5. Conditionals\unsection

\begin{entry}{%
\proto{if}{ \hyper{test} \hyper{consequent} \hyper{alternate}}{\exprtype}
\rproto{if}{ \hyper{test} \hyper{consequent}}{\exprtype}}  %\/ if hyper = italic

\syntax
\hyper{Test}, \hyper{consequent}, and \hyper{alternate} are
expressions.

\semantics
An {\cf if} expression is evaluated as follows: first,
\hyper{test} is evaluated.  If it yields a true value\index{true} (see
section~\ref{booleansection}), then \hyper{consequent} is evaluated and
its values are returned.  Otherwise \hyper{alternate} is evaluated and its
values are returned.  If \hyper{test} yields a false value and no
\hyper{alternate} is specified, then the result of the expression is
unspecified.

\begin{scheme}
(if (> 3 2) 'yes 'no)           \ev  yes
(if (> 2 3) 'yes 'no)           \ev  no
(if (> 3 2)
    (- 3 2)
    (+ 3 2))                    \ev  1%
\end{scheme}

\end{entry}


### 4.1.6. Assignments\unsection
\label{assignment}

\begin{entry}{%
\proto{set!}{ \hyper{variable} \hyper{expression}}{\exprtype}}

\semantics
\hyper{Expression} is evaluated, and the resulting value is stored in
the location to which \hyper{variable} is bound.  It is an error if \hyper{variable} is not
bound either in some region\index{region} enclosing the {\cf set!}\ expression
or else globally.
The result of the {\cf set!} expression is
unspecified.

\begin{scheme}
(define x 2)
(+ x 1)                 \ev  3
(set! x 4)              \ev  \unspecified
(+ x 1)                 \ev  5%
\end{scheme}

\end{entry}

### 4.1.7. Inclusion\unsection
\label{inclusion}
\begin{entry}{%
\proto{include}{ \hyperi{string} \hyperii{string} \dotsfoo}{\exprtype}
\rproto{include-ci}{ \hyperi{string} \hyperii{string} \dotsfoo}{\exprtype}}

\semantics
Both \ide{include} and
\ide{include-ci} take one or more filenames expressed as string literals,
apply an implementation-specific algorithm to find corresponding files,
read the contents of the files in the specified order as if by repeated applications
of {\cf read},
and effectively replace the {\cf include} or {\cf include-ci} expression
with a {\cf begin} expression containing what was read from the files.
The difference between the two is that \ide{include-ci} reads each file
as if it began with the {\cf{}\#!fold-case} directive, while \ide{include}
does not.

\begin{note}
Implementations are encouraged to search for files in the directory
which contains the including file, and to provide a way for users to
specify other directories to search.
\end{note}

\end{entry}

## 4.2. Derived expression types
\label{derivedexps}

The constructs in this section are hygienic, as discussed in
section~\ref{macrosection}.
For reference purposes, section~\ref{derivedsection} gives syntax definitions
that will convert most of the constructs described in this section
into the primitive constructs described in the previous section.


### 4.2.8. Conditionals\unsection

\begin{entry}{%
\proto{cond}{ \hyperi{clause} \hyperii{clause} \dotsfoo}{\exprtype}
\pproto{else}{\auxiliarytype}
\pproto{=>}{\auxiliarytype}}

\syntax
\hyper{Clauses} take one of two forms, either
\begin{scheme}
(\hyper{test} \hyperi{expression} \dotsfoo)%
\end{scheme}
where \hyper{test} is any expression, or
\begin{scheme}
(\hyper{test} => \hyper{expression})%
\end{scheme}
The last \hyper{clause} can be
an ``else clause,'' which has the form
\begin{scheme}
(else \hyperi{expression} \hyperii{expression} \dotsfoo)\rm.%
\end{scheme}
\mainschindex{else}
\mainschindex{=>}

\semantics
A {\cf cond} expression is evaluated by evaluating the \hyper{test}
expressions of successive \hyper{clause}s in order until one of them
evaluates to a true value\index{true} (see
section~\ref{booleansection}).  When a \hyper{test} evaluates to a true
value, the remaining \hyper{expression}s in its \hyper{clause} are
evaluated in order, and the results of the last \hyper{expression} in the
\hyper{clause} are returned as the results of the entire {\cf cond}
expression.

If the selected \hyper{clause} contains only the
\hyper{test} and no \hyper{expression}s, then the value of the
\hyper{test} is returned as the result.  If the selected \hyper{clause} uses the
\ide{=>} alternate form, then the \hyper{expression} is evaluated.
It is an error if its value is not a procedure that accepts one argument.  This procedure is then
called on the value of the \hyper{test} and the values returned by this
procedure are returned by the {\cf cond} expression.

If all \hyper{test}s evaluate
to \schfalse{}, and there is no else clause, then the result of
the conditional expression is unspecified; if there is an else
clause, then its \hyper{expression}s are evaluated in order, and the values of
the last one are returned.

\begin{scheme}
(cond ((> 3 2) 'greater)
      ((< 3 2) 'less))         \ev  greater%

(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))            \ev  equal%

(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else \schfalse{}))         \ev  2%
\end{scheme}


\end{entry}


\begin{entry}{%
\proto{case}{ \hyper{key} \hyperi{clause} \hyperii{clause} \dotsfoo}{\exprtype}}

\syntax
\hyper{Key} can be any expression.  Each \hyper{clause} has
the form
\begin{scheme}
((\hyperi{datum} \dotsfoo) \hyperi{expression} \hyperii{expression} \dotsfoo)\rm,%
\end{scheme}
where each \hyper{datum} is an external representation of some object.
It is an error if any of the \hyper{datum}s are the same anywhere in the expression.
Alternatively, a \hyper{clause} can be of the form
\begin{scheme}
((\hyperi{datum} \dotsfoo) => \hyper{expression})%
\end{scheme}
The last \hyper{clause} can be an ``else clause,'' which has one of the forms
\begin{scheme}
(else \hyperi{expression} \hyperii{expression} \dotsfoo)
\end{scheme}
or
\begin{scheme}
(else => \hyper{expression})\rm.%
\end{scheme}
\schindex{else}

\semantics
A {\cf case} expression is evaluated as follows.  \hyper{Key} is
evaluated and its result is compared against each \hyper{datum}.  If the
result of evaluating \hyper{key} is the same (in the sense of
{\cf eqv?}; see section~\ref{eqv?}) to a \hyper{datum}, then the
expressions in the corresponding \hyper{clause} are evaluated in order
and the results of the last expression in the \hyper{clause} are
returned as the results of the {\cf case} expression.

If the result of
evaluating \hyper{key} is different from every \hyper{datum}, then if
there is an else clause, its expressions are evaluated and the
results of the last are the results of the {\cf case} expression;
otherwise the result of the {\cf case} expression is unspecified.

If the selected \hyper{clause} or else clause uses the
\ide{=>} alternate form, then the \hyper{expression} is evaluated.
It is an error if its value is not a procedure accepting one argument.
This procedure is then
called on the value of the \hyper{key} and the values returned by this
procedure are returned by the {\cf case} expression.

\begin{scheme}
(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))     \ev  composite
(case (car '(c d))
  ((a) 'a)
  ((b) 'b))                     \ev  \unspecified
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else => (lambda (x) x)))     \ev  c%
\end{scheme}

\end{entry}


\begin{entry}{%
\proto{and}{ \hyperi{test} \dotsfoo}{\exprtype}}

\semantics
The \hyper{test} expressions are evaluated from left to right, and if
any expression evaluates to \schfalse{} (see
section~\ref{booleansection}), then \schfalse{} is returned.  Any remaining
expressions are not evaluated.  If all the expressions evaluate to
true values, the values of the last expression are returned.  If there
are no expressions, then \schtrue{} is returned.

\begin{scheme}
(and (= 2 2) (> 2 1))           \ev  \schtrue
(and (= 2 2) (< 2 1))           \ev  \schfalse
(and 1 2 'c '(f g))             \ev  (f g)
(and)                           \ev  \schtrue%
\end{scheme}

\end{entry}


\begin{entry}{%
\proto{or}{ \hyperi{test} \dotsfoo}{\exprtype}}

\semantics
The \hyper{test} expressions are evaluated from left to right, and the value of the
first expression that evaluates to a true value (see
section~\ref{booleansection}) is returned.  Any remaining expressions
are not evaluated.  If all expressions evaluate to \schfalse{}
or if there are no expressions, then \schfalse{} is returned.

\begin{scheme}
(or (= 2 2) (> 2 1))            \ev  \schtrue
(or (= 2 2) (< 2 1))            \ev  \schtrue
(or \schfalse \schfalse \schfalse) \ev  \schfalse
(or (memq 'b '(a b c))
    (/ 3 0))                    \ev  (b c)%
\end{scheme}

\end{entry}

\begin{entry}{%
\proto{when}{ \hyper{test} \hyperi{expression} \hyperii{expression} \dotsfoo}{\exprtype}}

\syntax
The \hyper{test} is an expression.

\semantics
The test is evaluated, and if it evaluates to a true value,
the expressions are evaluated in order.  The result of the {\cf when}
expression is unspecified.

\begin{scheme}
(when (= 1 1.0)
  (display "1")
  (display "2"))  \ev  \unspecified
 \>{\em and prints}  12%
\end{scheme}
\end{entry}

\begin{entry}{%
\proto{unless}{ \hyper{test} \hyperi{expression} \hyperii{expression} \dotsfoo}{\exprtype}}

\syntax
The \hyper{test} is an expression.

\semantics
The test is evaluated, and if it evaluates to \schfalse{},
the expressions are evaluated in order.  The result of the {\cf unless}
expression is unspecified.

\begin{scheme}
(unless (= 1 1.0)
  (display "1")
  (display "2"))  \ev  \unspecified
 \>{\em and prints nothing}%
\end{scheme}
\end{entry}

\begin{entry}{%
\proto{cond-expand}{ \hyperi{ce-clause} \hyperii{ce-clause} \dotsfoo}{\exprtype}}

\syntax
The \ide{cond-expand} expression type
provides a way to statically
expand different expressions depending on the
implementation.  A
\hyper{ce-clause} takes the following form:

{\tt(\hyper{feature requirement} \hyper{expression} \dotsfoo)}

The last clause can be an ``else clause,'' which has the form

{\tt(else \hyper{expression} \dotsfoo)}

A \hyper{feature requirement} takes one of the following forms:

\begin{itemize}
\item {\tt\hyper{feature identifier}}
\item {\tt(library \hyper{library name})}
\item {\tt(and \hyper{feature requirement} \dotsfoo)}
\item {\tt(or \hyper{feature requirement} \dotsfoo)}
\item {\tt(not \hyper{feature requirement})}
\end{itemize}

\semantics
Each implementation maintains a list of feature identifiers which are
present, as well as a list of libraries which can be imported.  The
value of a \hyper{feature requirement} is determined by replacing
each \hyper{feature identifier} and {\tt(library \hyper{library name})}
on the implementation's lists with \schtrue, and all other feature
identifiers and library names with \schfalse, then evaluating the
resulting expression as a Scheme boolean expression under the normal
interpretation of {\cf and}, {\cf or}, and {\cf not}.

A \ide{cond-expand} is then expanded by evaluating the
\hyper{feature requirement}s of successive \hyper{ce-clause}s
in order until one of them returns \schtrue.  When a true clause is
found, the corresponding \hyper{expression}s are expanded to a
{\cf begin}, and the remaining clauses are ignored.
If none of the \hyper{feature requirement}s evaluate to \schtrue, then
if there is an else clause, its \hyper{expression}s are
included.  Otherwise, the behavior of the \ide{cond-expand} is unspecified.
Unlike {\cf cond}, {\cf cond-expand} does not depend on the value
of any variables.

The exact features provided are implementation-defined, but for
portability a core set of features is given in
appendix~\ref{stdfeatures}.

\end{entry}

### 4.2.9. Binding constructs
\label{bindingsection}

The binding constructs {\cf let}, {\cf let*}, {\cf letrec}, {\cf letrec*},
{\cf let-values}, and {\cf let*-values}
give Scheme a block structure, like Algol 60.  The syntax of the first four
constructs is identical, but they differ in the regions\index{region} they establish
for their variable bindings.  In a {\cf let} expression, the initial
values are computed before any of the variables become bound; in a
{\cf let*} expression, the bindings and evaluations are performed
sequentially; while in {\cf letrec} and {\cf letrec*} expressions,
all the bindings are in
effect while their initial values are being computed, thus allowing
mutually recursive definitions.
The {\cf let-values} and {\cf let*-values} constructs are analogous to {\cf let} and {\cf let*}
respectively, but are designed to handle multiple-valued expressions, binding
different identifiers to the returned values.

\begin{entry}{%
\proto{let}{ \hyper{bindings} \hyper{body}}{\exprtype}}

\syntax
\hyper{Bindings} has the form
\begin{scheme}
((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%
\end{scheme}
where each \hyper{init} is an expression, and \hyper{body} is a
sequence of zero or more definitions followed by a
sequence of one or more expressions as described in section~\ref{lambda}.  It is
an error for a \hyper{variable} to appear more than once in the list of variables
being bound.

\semantics
The \hyper{init}s are evaluated in the current environment (in some
unspecified order), the \hyper{variable}s are bound to fresh locations
holding the results, the \hyper{body} is evaluated in the extended
environment, and the values of the last expression of \hyper{body}
are returned.  Each binding of a \hyper{variable} has \hyper{body} as its
region.\index{region}

\begin{scheme}
(let ((x 2) (y 3))
  (* x y))                      \ev  6

(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))                   \ev  35%
\end{scheme}

See also ``named {\cf let},'' section \ref{namedlet}.

\end{entry}


\begin{entry}{%
\proto{let*}{ \hyper{bindings} \hyper{body}}{\exprtype}}\nobreak

\nobreak
\syntax
\hyper{Bindings} has the form
\begin{scheme}
((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%
\end{scheme}
and \hyper{body} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}.

\semantics
The {\cf let*} binding construct is similar to {\cf let}, but the bindings are performed
sequentially from left to right, and the region\index{region} of a binding indicated
by {\cf(\hyper{variable} \hyper{init})} is that part of the {\cf let*}
expression to the right of the binding.  Thus the second binding is done
in an environment in which the first binding is visible, and so on.
The \hyper{variable}s need not be distinct.

\begin{scheme}
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))             \ev  70%
\end{scheme}

\end{entry}


\begin{entry}{%
\proto{letrec}{ \hyper{bindings} \hyper{body}}{\exprtype}}

\syntax
\hyper{Bindings} has the form
\begin{scheme}
((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%
\end{scheme}
and \hyper{body} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}. It is an error for a \hyper{variable} to appear more
than once in the list of variables being bound.

\semantics
The \hyper{variable}s are bound to fresh locations holding unspecified
values, the \hyper{init}s are evaluated in the resulting environment (in
some unspecified order), each \hyper{variable} is assigned to the result
of the corresponding \hyper{init}, the \hyper{body} is evaluated in the
resulting environment, and the values of the last expression in
\hyper{body} are returned.  Each binding of a \hyper{variable} has the
entire {\cf letrec} expression as its region\index{region}, making it possible to
define mutually recursive procedures.

\begin{scheme}
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
		\ev  \schtrue%
\end{scheme}

One restriction on {\cf letrec} is very important: if it is not possible
to evaluate each \hyper{init} without assigning or referring to the value of any
\hyper{variable}, it is an error.  The
restriction is necessary because
{\cf letrec} is defined in terms of a procedure
call where a {\cf lambda} expression binds the \hyper{variable}s to the values
of the \hyper{init}s.
In the most common uses of {\cf letrec}, all the \hyper{init}s are
\lambdaexp{}s and the restriction is satisfied automatically.

\end{entry}


\begin{entry}{%
\proto{letrec*}{ \hyper{bindings} \hyper{body}}{\exprtype}}
\label{letrecstar}

\syntax
\hyper{Bindings} has the form
\begin{scheme}
((\hyperi{variable} \hyperi{init}) \dotsfoo)\rm,%
\end{scheme}
and \hyper{body}\index{body} is a sequence of
zero or more definitions followed by
one or more expressions as described in section~\ref{lambda}. It is an error for a \hyper{variable} to appear more
than once in the list of variables being bound.

\semantics
The \hyper{variable}s are bound to fresh locations,
each \hyper{variable} is assigned in left-to-right order to the
result of evaluating the corresponding \hyper{init}, the \hyper{body} is
evaluated in the resulting environment, and the values of the last
expression in \hyper{body} are returned.
Despite the left-to-right evaluation and assignment order, each binding of
a \hyper{variable} has the entire {\cf letrec*} expression as its
region\index{region}, making it possible to define mutually recursive
procedures.

If it is not possible to evaluate each \hyper{init} without assigning or
referring to the value of the corresponding \hyper{variable} or the
\hyper{variable} of any of the bindings that follow it in
\hyper{bindings}, it is an error.
Another restriction is that it is an error to invoke the continuation
of an \hyper{init} more than once.

\begin{scheme}
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
                \ev  5%
\end{scheme}

\begin{entry}{%
\proto{let-values}{ \hyper{mv binding spec} \hyper{body}}{\exprtype}}

\syntax
\hyper{Mv binding spec} has the form
\begin{scheme}
((\hyperi{formals} \hyperi{init}) \dotsfoo)\rm,%
\end{scheme}

where each \hyper{init} is an expression, and \hyper{body} is
zero or more definitions followed by a sequence of one or
more expressions as described in section~\ref{lambda}.  It is an error for a variable to appear more than
once in the set of \hyper{formals}.

\semantics
The \hyper{init}s are evaluated in the current environment (in some
unspecified order) as if by invoking {\cf call-with-values}, and the
variables occurring in the \hyper{formals} are bound to fresh locations
holding the values returned by the \hyper{init}s, where the
\hyper{formals} are matched to the return values in the same way that
the \hyper{formals} in a {\cf lambda} expression are matched to the
arguments in a procedure call.  Then, the \hyper{body} is evaluated in
the extended environment, and the values of the last expression of
\hyper{body} are returned.  Each binding of a \hyper{variable} has
\hyper{body} as its region.\index{region}

It is an error if the \hyper{formals} do not match the number of
values returned by the corresponding \hyper{init}.

\begin{scheme}
(let-values (((root rem) (exact-integer-sqrt 32)))
  (* root rem))                \ev  35%
\end{scheme}

\end{entry}


\begin{entry}{%
\proto{let*-values}{ \hyper{mv binding spec} \hyper{body}}{\exprtype}}\nobreak

\nobreak
\syntax
\hyper{Mv binding spec} has the form
\begin{scheme}
((\hyper{formals} \hyper{init}) \dotsfoo)\rm,%
\end{scheme}
and \hyper{body} is a sequence of zero or more
definitions followed by one or more expressions as described in section~\ref{lambda}.  In each \hyper{formals},
it is an error if any variable appears more than once.

\semantics
The {\cf let*-values} construct is similar to {\cf let-values}, but the
\hyper{init}s are evaluated and bindings created sequentially from
left to right, with the region of the bindings of each \hyper{formals}
including the \hyper{init}s to its right as well as \hyper{body}.  Thus the
second \hyper{init} is evaluated in an environment in which the first
set of bindings is visible and initialized, and so on.

\begin{scheme}
(let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y)))     \ev (x y x y)%
\end{scheme}

\end{entry}

\end{entry}


### 4.2.10. Sequencing\unsection
\label{sequencing}

Both of Scheme's sequencing constructs are named {\cf begin}, but the two
have slightly different forms and uses:

\begin{entry}{%
\proto{begin}{ \hyper{expression or definition} \dotsfoo}{\exprtype}}

This form of {\cf begin} can appear as part of a \hyper{body}, or at the
outermost level of a \hyper{program}, or at the REPL, or directly nested
in a {\cf begin} that is itself of this form.
It causes the contained expressions and definitions to be evaluated
exactly as if the enclosing {\cf begin} construct were not present.

\begin{rationale}
This form is commonly used in the output of
macros (see section~\ref{macrosection})
which need to generate multiple definitions and
splice them into the context in which they are expanded.
\end{rationale}

\end{entry}

\begin{entry}{%
\rproto{begin}{ \hyperi{expression} \hyperii{expression} \dotsfoo}{\exprtype}}

This form of {\cf begin} can be used as an ordinary expression.
The \hyper{expression}s are evaluated sequentially from left to right,
and the values of the last \hyper{expression} are returned. This
expression type is used to sequence side effects such as assignments
or input and output.

\begin{scheme}
(define x 0)

(and (= x 0)
     (begin (set! x 5)
            (+ x 1)))              \ev  6

(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))      \ev  \unspecified
 \>{\em and prints}  4 plus 1 equals 5%
\end{scheme}

\end{entry}

Note that there is a third form of {\cf begin} used as a library declaration:
see section~\ref{librarydeclarations}.

### 4.2.11. Iteration%\unsection

\noindent%
\pproto{(do ((\hyperi{variable} \hyperi{init} \hyperi{step})}{\exprtype}
\mainschindex{do}{\tt\obeyspaces%
     \dotsfoo)\\
    (\hyper{test} \hyper{expression} \dotsfoo)\\
  \hyper{command} \dotsfoo)}

\syntax
All of \hyper{init}, \hyper{step}, \hyper{test}, and \hyper{command}
are expressions.

\semantics
A {\cf do} expression is an iteration construct.  It specifies a set of variables to
be bound, how they are to be initialized at the start, and how they are
to be updated on each iteration.  When a termination condition is met,
the loop exits after evaluating the \hyper{expression}s.

A {\cf do} expression is evaluated as follows:
The \hyper{init} expressions are evaluated (in some unspecified order),
the \hyper{variable}s are bound to fresh locations, the results of the
\hyper{init} expressions are stored in the bindings of the
\hyper{variable}s, and then the iteration phase begins.

Each iteration begins by evaluating \hyper{test}; if the result is
false (see section~\ref{booleansection}), then the \hyper{command}
expressions are evaluated in order for effect, the \hyper{step}
expressions are evaluated in some unspecified order, the
\hyper{variable}s are bound to fresh locations, the results of the
\hyper{step}s are stored in the bindings of the
\hyper{variable}s, and the next iteration begins.

If \hyper{test} evaluates to a true value, then the
\hyper{expression}s are evaluated from left to right and the values of
the last \hyper{expression} are returned.  If no \hyper{expression}s
are present, then the value of the {\cf do} expression is unspecified.

The region\index{region} of the binding of a \hyper{variable}
consists of the entire {\cf do} expression except for the \hyper{init}s.
It is an error for a \hyper{variable} to appear more than once in the
list of {\cf do} variables.

A \hyper{step} can be omitted, in which case the effect is the
same as if {\cf(\hyper{variable} \hyper{init} \hyper{variable})} had
been written instead of {\cf(\hyper{variable} \hyper{init})}.

\begin{scheme}
(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i))          \ev  \#(0 1 2 3 4)

(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum)))             \ev  25%
\end{scheme}



\begin{entry}{%
\rproto{let}{ \hyper{variable} \hyper{bindings} \hyper{body}}{\exprtype}}

\label{namedlet}
\semantics
``Named {\cf let}'' is a variant on the syntax of \ide{let} which provides
a more general looping construct than {\cf do} and can also be used to express
recursion.
It has the same syntax and semantics as ordinary {\cf let}
except that \hyper{variable} is bound within \hyper{body} to a procedure
whose formal arguments are the bound variables and whose body is
\hyper{body}.  Thus the execution of \hyper{body} can be repeated by
invoking the procedure named by \hyper{variable}.

%                                              |  <-- right margin
\begin{scheme}
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
               (cons (car numbers) neg))))) %
  \lev  ((6 1 3) (-5 -2))%
\end{scheme}

\end{entry}


### 4.2.12. Delayed evaluation\unsection

\begin{entry}{%
\proto{delay}{ \hyper{expression}}{lazy library syntax}}

\todo{Fix.}

\semantics
The {\cf delay} construct is used together with the procedure \ide{force} to
implement \defining{lazy evaluation} or \defining{call by need}.
{\tt(delay~\hyper{expression})} returns an object called a
\defining{promise} which at some point in the future can be asked (by
the {\cf force} procedure) to evaluate
\hyper{expression}, and deliver the resulting value.
\todo{consider removing unspecified effect}
The effect of \hyper{expression} returning multiple values
is unspecified.

\end{entry}

\begin{entry}{%
\proto{delay-force}{ \hyper{expression}}{lazy library syntax}}

\todo{Fix.}

\semantics
The expression {\cf (delay-force \var{expression})} is conceptually similar to
{\cf (delay (force \var{expression}))},
with the difference that forcing the result
of {\cf delay-force} will in effect result in a tail call to
{\cf (force \var{expression})},
while forcing the result of
{\cf (delay (force \var{expression}))}
might not.  Thus
iterative lazy algorithms that might result in a long series of chains of
{\cf delay} and {\cf force}
can be rewritten using {\cf delay-force} to prevent consuming
unbounded space during evaluation.

\end{entry}

\begin{entry}{%
\proto{force}{ promise}{lazy library procedure}}

The {\cf force} procedure forces the value of a \var{promise} created
by \ide{delay}, \ide{delay-force}, or \ide{make-promise}.\index{promise}
If no value has been computed for the promise, then a value is
computed and returned.  The value of the promise must be cached (or
``memoized'') so that if it is forced a second time, the previously
computed value is returned.
Consequently, a delayed expression is evaluated using the parameter
values and exception handler of the call to {\cf force} which first
requested its value.
If \var{promise} is not a promise, it may be returned unchanged.

\begin{scheme}
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
                               \ev  2%
\end{scheme}

The following example is a mechanical transformation of a lazy
stream-filtering algorithm into Scheme.  Each call to a constructor is
wrapped in {\cf delay}, and each argument passed to a deconstructor is
wrapped in {\cf force}.  The use of {\cf (delay-force ...)} instead of {\cf
(delay (force ...))} around the body of the procedure ensures that an
ever-growing sequence of pending promises does not
exhaust available storage,
because {\cf force} will in effect force such sequences iteratively.

\begin{scheme}
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
                               \ev 5%
\end{scheme}

The following examples are not intended to illustrate good programming
style, as {\cf delay}, {\cf force}, and {\cf delay-force} are mainly intended
for programs written in the functional style.
However, they do illustrate the property that only one value is
computed for a promise, no matter how many times it is forced.

\begin{scheme}
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
       (force p))     \ev  6%
\end{scheme}

Various extensions to this semantics of {\cf delay}, {\cf force} and
{\cf delay-force} are supported in some implementations:

\begin{itemize}
\item Calling {\cf force} on an object that is not a promise may simply
return the object.

\item It may be the case that there is no means by which a promise can be
operationally distinguished from its forced value.  That is, expressions
like the following may evaluate to either \schtrue{} or to \schfalse{},
depending on the implementation:

\begin{scheme}
(eqv? (delay 1) 1)          \ev  \unspecified
(pair? (delay (cons 1 2)))  \ev  \unspecified%
\end{scheme}

\item Implementations may implement ``implicit forcing,'' where
the value of a promise is forced by procedures
that operate only on arguments of a certain type, like {\cf cdr}
and {\cf *}.  However, procedures that operate uniformly on their
arguments, like {\cf list}, must not force them.

\begin{scheme}
(+ (delay (* 3 7)) 13)  \ev  \unspecified
(car
  (list (delay (* 3 7)) 13))    \ev {\it{}a promise}%
\end{scheme}
\end{itemize}
\end{entry}

\begin{entry}{%
\proto{promise?} { \var{obj}}{lazy library procedure}}

The {\cf promise?} procedure returns
\schtrue{} if its argument is a promise, and \schfalse{} otherwise.  Note
that promises are not necessarily disjoint from other Scheme types such
as procedures.

\end{entry}

\begin{entry}{%
\proto{make-promise} { \var{obj}}{lazy library procedure}}

The {\cf make-promise} procedure returns a promise which, when forced, will return
\var{obj}.  It is similar to {\cf delay}, but does not delay
its argument: it is a procedure rather than syntax.
If \var{obj} is already a promise, it is returned.

\end{entry}

### 4.2.13. Dynamic bindings\unsection

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

\begin{entry}{%
\proto{make-parameter}{ init}{procedure}
\rproto{make-parameter}{ init converter}{procedure}}

Returns a newly allocated parameter object,
which is a procedure that accepts zero arguments and
returns the value associated with the parameter object.
Initially, this value is the value of
{\cf (\var{converter} \var{init})}, or of \var{init}
if the conversion procedure \var{converter} is not specified.
The associated value can be temporarily changed
using {\cf parameterize}, which is described below.

The effect of passing arguments to a parameter object is
implementation-dependent.
\end{entry}

\begin{entry}{%
\pproto{(parameterize ((\hyperi{param} \hyperi{value}) \dotsfoo)}{syntax}
{\tt\obeyspaces%
\hspace*{1em}\hyper{body})}}
\mainschindex{parameterize}

\syntax
Both \hyperi{param} and \hyperi{value} are expressions.

\domain{It is an error if the value of any \hyper{param} expression is not a parameter object.}
\semantics
A {\cf parameterize} expression is used to change the values returned by
specified parameter objects during the evaluation of the body.

The \hyper{param} and \hyper{value} expressions
are evaluated in an unspecified order.  The \hyper{body} is
evaluated in a dynamic environment in which calls to the
parameters return the results of passing the corresponding values
to the conversion procedure specified when the parameters were created.
Then the previous values of the parameters are restored without passing
them to the conversion procedure.
The results of the last
expression in the \hyper{body} are returned as the results of the entire
{\cf parameterize} expression.

\begin{note}
If the conversion procedure is not idempotent, the results of
{\cf (parameterize ((x (x))) ...)},
which appears to bind the parameter \var{x} to its current value,
might not be what the user expects.
\end{note}

If an implementation supports multiple threads of execution, then
{\cf parameterize} must not change the associated values of any parameters
in any thread other than the current thread and threads created
inside \hyper{body}.

Parameter objects can be used to specify configurable settings for a
computation without the need to pass the value to every
procedure in the call chain explicitly.

\begin{scheme}
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
  (f 12))                                    \ev \scherror%
\end{scheme}
\end{entry}


### 4.2.14. Exception handling\unsection

\begin{entry}{%
\pproto{(guard (\hyper{variable}}{\exprtype}
{\tt\obeyspaces%
\hspace*{4em}\hyperi{cond clause} \hyperii{cond clause} \dotsfoo)\\
\hspace*{2em}\hyper{body})}\\
}
\mainschindex{guard}

\syntax
Each \hyper{cond clause} is as in the specification of {\cf cond}.

\semantics
The \hyper{body} is evaluated with an exception
handler that binds the raised object (see \ide{raise} in section~\ref{exceptionsection})
to \hyper{variable} and, within the scope of
that binding, evaluates the clauses as if they were the clauses of a
{\cf cond} expression. That implicit {\cf cond} expression is evaluated with the
continuation and dynamic environment of the {\cf guard} expression. If every
\hyper{cond clause}'s \hyper{test} evaluates to \schfalse{} and there
is no else clause, then
{\cf raise-continuable} is invoked on the raised object within the dynamic
environment of the original call to {\cf raise}
or {\cf raise-continuable}, except that the current
exception handler is that of the {\cf guard} expression.


See section~\ref{exceptionsection} for a more complete discussion of
exceptions.

\begin{scheme}
(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'a 42))))
\ev 42

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition)))
  (raise (list (cons 'b 23))))
\ev (b . 23)%
\end{scheme}
\end{entry}


### 4.2.15. Quasiquotation\unsection
\label{quasiquotesection}

\begin{entry}{%
\proto{quasiquote}{ \hyper{qq template}}{\exprtype} \nopagebreak
\pproto{\backquote\hyper{qq template}}{\exprtype}
\pproto{unquote}{\auxiliarytype}
\pproto{\comma}{\auxiliarytype}
\pproto{unquote-splicing}{\auxiliarytype}
\pproto{\commaatsign}{\auxiliarytype}}

``Quasiquote''\index{backquote} expressions are useful
for constructing a list or vector structure when some but not all of the
desired structure is known in advance.  If no
commas\index{comma} appear within the \hyper{qq template}, the result of
evaluating
\backquote\hyper{qq template} is equivalent to the result of evaluating
\singlequote\hyper{qq template}.  If a comma\mainschindex{,} appears within the
\hyper{qq template}, however, the expression following the comma is
evaluated (``unquoted'') and its result is inserted into the structure
instead of the comma and the expression.  If a comma appears followed
without intervening whitespace by a commercial at-sign (\atsign),\mainschindex{,@} then it is an error if the following
expression does not evaluate to a list; the opening and closing parentheses
of the list are then ``stripped away'' and the elements of the list are
inserted in place of the comma at-sign expression sequence.  A comma
at-sign normally appears only within a list or vector \hyper{qq template}.

\begin{note}
In order to unquote an identifier beginning with {\cf @}, it is necessary
to use either an explicit {\cf unquote} or to put whitespace after the comma,
to avoid colliding with the comma at-sign sequence.
\end{note}

\begin{scheme}
`(list ,(+ 1 2) 4)  \ev  (list 3 4)
(let ((name 'a)) `(list ,name ',name)) %
          \lev  (list a (quote a))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) %
          \lev  (a 3 4 5 6 b)
`(({\cf foo} ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) %
          \lev  ((foo 7) . cons)
`\#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) %
          \lev  \#(10 5 2 4 3 8)
(let ((foo '(foo bar)) (@baz 'baz))
  `(list ,@foo , @baz))%
          \lev  (list foo bar baz)%
\end{scheme}

Quasiquote expressions can be nested.  Substitutions are made only for
unquoted components appearing at the same nesting level
as the outermost quasiquote.  The nesting level increases by one inside
each successive quasiquotation, and decreases by one inside each
unquotation.

\begin{scheme}
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) %
          \lev  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e)) %
          \lev  (a `(b ,x ,'y d) e)%
\end{scheme}

A quasiquote expression may return either newly allocated, mutable objects or
literal structure for any structure that is constructed at run time
during the evaluation of the expression. Portions that do not need to
be rebuilt are always literal. Thus,

\begin{scheme}
(let ((a 3)) `((1 2) ,a ,4 ,'five 6))%
\end{scheme}

may be treated as equivalent to either of the following expressions:

\begin{scheme}
`((1 2) 3 4 five 6)

(let ((a 3))
  (cons '(1 2)
        (cons a (cons 4 (cons 'five '(6))))))%
\end{scheme}

However, it is not equivalent to this expression:

\begin{scheme}
(let ((a 3)) (list (list 1 2) a 4 'five 6))%
\end{scheme}

The two notations
 \backquote\hyper{qq template} and {\tt (quasiquote \hyper{qq template})}
 are identical in all respects.
 {\cf,\hyper{expression}} is identical to {\cf (unquote \hyper{expression})},
 and
 {\cf,@\hyper{expression}} is identical to {\cf (unquote-splicing \hyper{expression})}.
The \ide{write} procedure may output either format.
\mainschindex{`}

\begin{scheme}
(quasiquote (list (unquote (+ 1 2)) 4)) %
          \lev  (list 3 4)
'(quasiquote (list (unquote (+ 1 2)) 4)) %
          \lev  `(list ,(+ 1 2) 4)
     {\em{}i.e.,} (quasiquote (list (unquote (+ 1 2)) 4))%
\end{scheme}


It is an error if any of the identifiers {\cf quasiquote}, {\cf unquote},
or {\cf unquote-splicing} appear in positions within a \hyper{qq template}
otherwise than as described above.

\end{entry}

### 4.2.16. Case-lambda\unsection
\label{caselambdasection}
\begin{entry}{%
\proto{case-lambda}{ \hyper{clause} \dotsfoo}{case-lambda library syntax}}

\syntax
Each \hyper{clause} is of the form
(\hyper{formals} \hyper{body}),
where \hyper{formals} and \hyper{body} have the same syntax
as in a \lambdaexp.

\semantics
A {\cf case-lambda} expression evaluates to a procedure that accepts
a variable number of arguments and is lexically scoped in the same
manner as a procedure resulting from a \lambdaexp. When the procedure
is called, the first \hyper{clause} for which the arguments agree
with \hyper{formals} is selected, where agreement is specified as for
the \hyper{formals} of a \lambdaexp. The variables of \hyper{formals} are
bound to fresh locations, the values of the arguments are stored in those
locations, the \hyper{body} is evaluated in the extended environment,
and the results of \hyper{body} are returned as the results of the
procedure call.

It is an error for the arguments not to agree with
the \hyper{formals} of any \hyper{clause}.

\begin{scheme}
(define range
  (case-lambda
   ((e) (range 0 e))
   ((b e) (do ((r '() (cons e r))
               (e (- e 1) (- e 1)))
              ((< e b) r)))))

(range 3)    \ev (0 1 2)
(range 3 5)  \ev (3 4)%
\end{scheme}

\end{entry}

## 4.3. Macros
\label{macrosection}

Scheme programs can define and use new derived expression types,
 called {\em macros}.\mainindex{macro}
Program-defined expression types have the syntax
\begin{scheme}
(\hyper{keyword} {\hyper{datum}} ...)%
\end{scheme}%
where \hyper{keyword} is an identifier that uniquely determines the
expression type.  This identifier is called the {\em syntactic
keyword}\index{syntactic keyword}, or simply {\em
keyword}\index{keyword}, of the macro\index{macro keyword}.  The
number of the \hyper{datum}s, and their syntax, depends on the
expression type.

Each instance of a macro is called a {\em use}\index{macro use}
of the macro.
The set of rules that specifies
how a use of a macro is transcribed into a more primitive expression
is called the {\em transformer}\index{macro transformer}
of the macro.

The macro definition facility consists of two parts:

\begin{itemize}
\item A set of expressions used to establish that certain identifiers
are macro keywords, associate them with macro transformers, and control
the scope within which a macro is defined, and

\item a pattern language for specifying macro transformers.
\end{itemize}

The syntactic keyword of a macro can shadow variable bindings, and local
variable bindings can shadow syntactic bindings.  \index{keyword}
Two mechanisms are provided to prevent unintended conflicts:

\begin{itemize}

\item If a macro transformer inserts a binding for an identifier
(variable or keyword), the identifier will in effect be renamed
throughout its scope to avoid conflicts with other identifiers.
Note that a global variable definition may or may not introduce a binding;
see section~\ref{defines}.

\item If a macro transformer inserts a free reference to an
identifier, the reference refers to the binding that was visible
where the transformer was specified, regardless of any local
bindings that surround the use of the macro.

\end{itemize}

In consequence, all macros
defined using the pattern language  are ``hygienic'' and ``referentially
transparent'' and thus preserve Scheme's lexical scoping.~\cite{Kohlbecker86,
hygienic,Bawden88,macrosthatwork,syntacticabstraction}
\mainindex{hygienic}\mainindex{referentially transparent}

Implementations may provide macro facilities of other types.

### 4.3.17. Binding constructs for syntactic keywords
\label{bindsyntax}

The {\cf let-syntax} and {\cf letrec-syntax} binding constructs are
analogous to {\cf let} and {\cf letrec}, but they bind
syntactic keywords to macro transformers instead of binding variables
to locations that contain values.  Syntactic keywords can also be
bound globally or locally with {\cf define-syntax};
see section~\ref{define-syntax}.

\begin{entry}{%
\proto{let-syntax}{ \hyper{bindings} \hyper{body}}{\exprtype}}

\syntax
\hyper{Bindings} has the form
\begin{scheme}
((\hyper{keyword} \hyper{transformer spec}) \dotsfoo)%
\end{scheme}
Each \hyper{keyword} is an identifier,
each \hyper{transformer spec} is an instance of {\cf syntax-rules}, and
\hyper{body} is a sequence of one or more definitions followed
by one or more expressions.  It is an error
for a \hyper{keyword} to appear more than once in the list of keywords
being bound.

\semantics
The \hyper{body} is expanded in the syntactic environment
obtained by extending the syntactic environment of the
{\cf let-syntax} expression with macros whose keywords are
the \hyper{keyword}s, bound to the specified transformers.
Each binding of a \hyper{keyword} has \hyper{body} as its region.

\begin{scheme}
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
      (m))))                       \ev  outer%
\end{scheme}

\end{entry}

\begin{entry}{%
\proto{letrec-syntax}{ \hyper{bindings} \hyper{body}}{\exprtype}}

\syntax
Same as for {\cf let-syntax}.

\semantics
 The \hyper{body} is expanded in the syntactic environment obtained by
extending the syntactic environment of the {\cf letrec-syntax}
expression with macros whose keywords are the
\hyper{keyword}s, bound to the specified transformers.
Each binding of a \hyper{keyword} has the \hyper{transformer spec}s
as well as the \hyper{body} within its region,
so the transformers can
transcribe expressions into uses of the macros
introduced by the {\cf letrec-syntax} expression.

\begin{scheme}
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
           y)))        \ev  7%
\end{scheme}

\end{entry}

### 4.3.18. Pattern language
\label{patternlanguage}

A \hyper{transformer spec} has one of the following forms:

\begin{entry}{%
\pproto{(syntax-rules (\hyper{literal} \dotsfoo)}{\exprtype}
{\tt\obeyspaces%
\hspace*{1em}\hyper{syntax rule} \dotsfoo)\\
}
\pproto{(syntax-rules \hyper{ellipsis} (\hyper{literal} \dotsfoo)}{\exprtype}
{\tt\obeyspaces%
\hspace*{1em}\hyper{syntax rule} \dotsfoo)}\\
\pproto{\_}{\auxiliarytype}
\pproto{\dotsfoo}{\auxiliarytype}}
\mainschindex{_}

\syntax
It is an error if any of the \hyper{literal}s, or the \hyper{ellipsis} in the second form,
is not an identifier.
It is also an error if
\hyper{syntax rule} is not of the form
\begin{scheme}
(\hyper{pattern} \hyper{template})%
\end{scheme}
The \hyper{pattern} in a \hyper{syntax rule} is a list \hyper{pattern}
whose first element is an identifier.

A \hyper{pattern} is either an identifier, a constant, or one of the
following
\begin{scheme}
(\hyper{pattern} \ldots)
(\hyper{pattern} \hyper{pattern} \ldots . \hyper{pattern})
(\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots)
(\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots
  . \hyper{pattern})
\#(\hyper{pattern} \ldots)
\#(\hyper{pattern} \ldots \hyper{pattern} \hyper{ellipsis} \hyper{pattern} \ldots)%
\end{scheme}
and a \hyper{template} is either an identifier, a constant, or one of the following
\begin{scheme}
(\hyper{element} \ldots)
(\hyper{element} \hyper{element} \ldots . \hyper{template})
(\hyper{ellipsis} \hyper{template})
\#(\hyper{element} \ldots)%
\end{scheme}
where an \hyper{element} is a \hyper{template} optionally
followed by an \hyper{ellipsis}.
An \hyper{ellipsis} is the identifier specified in the second form
of {\cf syntax-rules}, or the default identifier {\cf ...}
(three consecutive periods) otherwise.\schindex{...}

\semantics An instance of {\cf syntax-rules} produces a new macro
transformer by specifying a sequence of hygienic rewrite rules.  A use
of a macro whose keyword is associated with a transformer specified by
{\cf syntax-rules} is matched against the patterns contained in the
\hyper{syntax rule}s, beginning with the leftmost \hyper{syntax rule}.
When a match is found, the macro use is transcribed hygienically
according to the template.

An identifier appearing within a \hyper{pattern} can be an underscore
({\cf \_}), a literal identifier listed in the list of \hyper{literal}s,
or the \hyper{ellipsis}.
All other identifiers appearing within a \hyper{pattern} are
{\em pattern variables}.

The keyword at the beginning of the pattern in a
\hyper{syntax rule} is not involved in the matching and
is considered neither a pattern variable nor a literal identifier.

Pattern variables match arbitrary input elements and
are used to refer to elements of the input in the template.
It is an error for the same pattern variable to appear more than once in a
\hyper{pattern}.

Underscores also match arbitrary input elements but are not pattern variables
and so cannot be used to refer to those elements.  If an underscore appears
in the \hyper{literal}s list, then that takes precedence and
underscores in the \hyper{pattern} match as literals.
Multiple underscores can appear in a \hyper{pattern}.

Identifiers that appear in \texttt{(\hyper{literal} \dotsfoo)} are
interpreted as literal
identifiers to be matched against corresponding elements of the input.
An element in the input matches a literal identifier if and only if it is an
identifier and either both its occurrence in the macro expression and its
occurrence in the macro definition have the same lexical binding, or
the two identifiers are the same and both have no lexical binding.

A subpattern followed by \hyper{ellipsis} can match zero or more elements of
the input, unless \hyper{ellipsis} appears in the \hyper{literal}s, in which
case it is matched as a literal.

More formally, an input expression $E$ matches a pattern $P$ if and only if:

\begin{itemize}
\item $P$ is an underscore ({\cf \_}).

\item $P$ is a non-literal identifier; or

\item $P$ is a literal identifier and $E$ is an identifier with the same
      binding; or

\item $P$ is a list {\cf ($P_1$ $\dots$ $P_n$)} and $E$ is a
      list of $n$
      elements that match $P_1$ through $P_n$, respectively; or

\item $P$ is an improper list
      {\cf ($P_1$ $P_2$ $\dots$ $P_n$ . $P_{n+1}$)}
      and $E$ is a list or
      improper list of $n$ or more elements that match $P_1$ through $P_n$,
      respectively, and whose $n$th tail matches $P_{n+1}$; or

\item $P$ is of the form
      {\cf ($P_1$ $\dots$ $P_k$ $P_e$ \meta{ellipsis} $P_{m+1}$ \dotsfoo{} $P_n$)}
      where $E$ is
      a proper list of $n$ elements, the first $k$ of which match
      $P_1$ through $P_k$, respectively,
      whose next $m-k$ elements each match $P_e$,
      whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$; or

\item $P$ is of the form
      {\cf ($P_1$ $\dots$ $P_k$ $P_{e}$ \meta{ellipsis} $P_{m+1}$ \dotsfoo{} $P_n$ . $P_x$)}
      where $E$ is
      a list or improper list of $n$ elements, the first $k$ of which match
      $P_1$ through $P_k$,
      whose next $m-k$ elements each match $P_e$,
      whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$,
      and whose $n$th and final cdr matches $P_x$; or

\item $P$ is a vector of the form {\cf \#($P_1$ $\dots$ $P_n$)}
      and $E$ is a vector
      of $n$ elements that match $P_1$ through $P_n$; or

\item $P$ is of the form
      {\cf \#($P_1$ $\dots$ $P_k$ $P_{e}$ \meta{ellipsis} $P_{m+1}$ \dotsfoo $P_n$)}
      where $E$ is a vector of $n$
      elements the first $k$ of which match $P_1$ through $P_k$,
      whose next $m-k$ elements each match $P_e$,
      and whose remaining $n-m$ elements match $P_{m+1}$ through $P_n$; or

\item $P$ is a constant and $E$ is equal to $P$ in the sense of
      the {\cf equal?} procedure.
\end{itemize}

It is an error to use a macro keyword, within the scope of its
binding, in an expression that does not match any of the patterns.

When a macro use is transcribed according to the template of the
matching \hyper{syntax rule}, pattern variables that occur in the
template are replaced by the elements they match in the input.
Pattern variables that occur in subpatterns followed by one or more
instances of the identifier
\hyper{ellipsis} are allowed only in subtemplates that are
followed by as many instances of \hyper{ellipsis}.
They are replaced in the
output by all of the elements they match in the input, distributed as
indicated.  It is an error if the output cannot be built up as
specified.

%%% This description of output construction is very vague.  It should
%%% probably be formalized, but that is not easy...

Identifiers that appear in the template but are not pattern variables
or the identifier
\hyper{ellipsis} are inserted into the output as literal identifiers.  If a
literal identifier is inserted as a free identifier then it refers to the
binding of that identifier within whose scope the instance of
{\cf syntax-rules} appears.
If a literal identifier is inserted as a bound identifier then it is
in effect renamed to prevent inadvertent captures of free identifiers.

A template of the form
{\cf (\hyper{ellipsis} \hyper{template})} is identical to \hyper{template},
except that
ellipses within the template have no special meaning.
That is, any ellipses contained within \hyper{template} are
treated as ordinary identifiers.
In particular, the template {\cf (\hyper{ellipsis} \hyper{ellipsis})} produces
a single \hyper{ellipsis}.
This allows syntactic abstractions to expand into code containing
ellipses.

\begin{scheme}
(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))

(be-like-begin sequence)
(sequence 1 2 3 4) \ev 4%
\end{scheme}

As an example, if \ide{let} and \ide{cond} are defined as in
section~\ref{derivedsection} then they are hygienic (as required) and
the following is not an error.

\begin{scheme}
(let ((=> \schfalse))
  (cond (\schtrue => 'ok)))           \ev ok%
\end{scheme}

The macro transformer for {\cf cond} recognizes {\cf =>}
as a local variable, and hence an expression, and not as the
base identifier {\cf =>}, which the macro transformer treats
as a syntactic keyword.  Thus the example expands into

\begin{scheme}
(let ((=> \schfalse))
  (if \schtrue (begin => 'ok)))%
\end{scheme}

instead of

\begin{scheme}
(let ((=> \schfalse))
  (let ((temp \schtrue))
    (if temp ('ok temp))))%
\end{scheme}

which would result in an invalid procedure call.

\end{entry}

### 4.3.19. Signaling errors in macro transformers


\begin{entry}{%
\pproto{(syntax-error \hyper{message} \hyper{args} \dotsfoo)}{\exprtype}}
\mainschindex{syntax-error}

{\cf syntax-error} behaves similarly to {\cf error} (\ref{exceptionsection}) except that implementations
with an expansion pass separate from evaluation should signal an error
as soon as {\cf syntax-error} is expanded.  This can be used as
a {\cf syntax-rules} \hyper{template} for a \hyper{pattern} that is
an invalid use of the macro, which can provide more descriptive error
messages.  \hyper{message} is a string literal, and \hyper{args}
arbitrary expressions providing additional information.
Applications cannot count on being able to catch syntax errors with
exception handlers or guards.

\todo{Shinn: This doesn't check all non-identifier cases, think of a better example.}

\begin{scheme}
(define-syntax simple-let
  (syntax-rules ()
    ((\_ (head ... ((x . y) val) . tail)
        body1 body2 ...)
     (syntax-error
      "expected an identifier but got"
      (x . y)))
    ((\_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
       val ...))))%
\end{scheme}

\end{entry}