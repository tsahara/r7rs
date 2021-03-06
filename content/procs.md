+++
weight = 30
title = "6 Standard procedures"
menu = "main"
+++

# 6. Standard procedures
{{< label "initialenv" >}}
{{< label "builtinchapter" >}}





This chapter describes Scheme's built-in procedures.

The procedures ``force``, ``promise?``, and ``make-promise`` are intimately associated
with the expression types ``delay`` and ``delay-force``, and are described
with them in section~\ref{force}.  In the same way, the procedure
``make-parameter`` is intimately associated with the expression type
``parameterize``, and is described with it in section~\ref{make-parameter}.


A program can use a global variable definition to bind any variable.  It may
subsequently alter any such binding by an assignment (see
section~\ref{assignment}).  These operations do not modify the behavior of
any procedure defined in this report or imported from a library
(see section~\ref{libraries}).  Altering any global binding that has
not been introduced by a definition has an unspecified effect on the
behavior of the procedures defined in this chapter.

When a procedure is said to return a \defining{newly allocated} object,
it means that the locations in the object are fresh.

## 6.1. Equivalence predicates
{{< label "equivalencesection" >}}

A \defining{predicate} is a procedure that always returns a boolean
value ({{< tt "#t" >}} or \schfalse).  An \defining{equivalence predicate} is
the computational analogue of a mathematical equivalence relation; it is
symmetric, reflexive, and transitive.  Of the equivalence predicates
described in this section, ``eq?`` is the finest or most
discriminating, ``equal?`` is the coarsest, and ``eqv?`` is
slightly less discriminating than ``eq?``.


\begin{entry}{
\proto{eqv?}{ \vari{obj} \varii{obj}}{procedure}}

The ``eqv?`` procedure defines a useful equivalence relation on objects.
Briefly, it returns {{< tt "#t" >}} if \vari{obj} and \varii{obj} are
normally regarded as the same object.  This relation is left slightly
open to interpretation, but the following partial specification of
``eqv?`` holds for all implementations of Scheme.

The ``eqv?`` procedure returns {{< tt "#t" >}} if:


- \vari{obj} and \varii{obj} are both {{< tt "#t" >}} or both \schfalse.

- \vari{obj} and \varii{obj} are both symbols and are the same
symbol according to the ``symbol=?`` procedure
(section~\ref{symbolsection}).

- \vari{obj} and \varii{obj} are both exact numbers and
are numerically equal (in the sense of ``=``).

- \vari{obj} and \varii{obj} are both inexact numbers such that
they are numerically equal (in the sense of ``=``)
and they yield the same results (in the sense of ``eqv?``)
when passed as arguments to any other procedure
that can be defined as a finite composition of Scheme's standard
arithmetic procedures, provided it does not result in a NaN value.

- \vari{obj} and \varii{obj} are both characters and are the same
character according to the ``char=?`` procedure
(section~\ref{charactersection}).

- \vari{obj} and \varii{obj} are both the empty list.

- \vari{obj} and \varii{obj} are pairs, vectors, bytevectors, records,
or strings that denote the same location in the store
(section~\ref{storagemodel}).

- \vari{obj} and \varii{obj} are procedures whose location tags are
equal (section~\ref{lambda}).


The ``eqv?`` procedure returns {{< tt "#f" >}} if:


- \vari{obj} and \varii{obj} are of different types
(section~\ref{disjointness}).

- one of \vari{obj} and \varii{obj} is {{< tt "#t" >}} but the other is
{{< tt "#f" >}}.

- \vari{obj} and \varii{obj} are symbols but are not the same
symbol according to the ``symbol=?`` procedure
(section~\ref{symbolsection}).

- one of \vari{obj} and \varii{obj} is an exact number but the other
is an inexact number.

- \vari{obj} and \varii{obj} are both exact numbers and
are numerically unequal (in the sense of ``=``).

- \vari{obj} and \varii{obj} are both inexact numbers such that either
they are numerically unequal (in the sense of ``=``),
or they do not yield the same results (in the sense of ``eqv?``)
when passed as arguments to any other procedure
that can be defined as a finite composition of Scheme's standard
arithmetic procedures, provided it does not result in a NaN value.
As an exception, the behavior of ``eqv?`` is unspecified
when both \vari{obj} and \varii{obj} are NaN.

- \vari{obj} and \varii{obj} are characters for which the ``char=?``
procedure returns {{< tt "#f" >}}.

- one of \vari{obj} and \varii{obj} is the empty list but the other
is not.

- \vari{obj} and \varii{obj} are pairs, vectors, bytevectors, records,
or strings that denote distinct locations.

- \vari{obj} and \varii{obj} are procedures that would behave differently
(return different values or have different side effects) for some arguments.



```
(eqv? 'a 'a)                     \ev  \schtrue
(eqv? 'a 'b)                     \ev  \schfalse
(eqv? 2 2)                       \ev  \schtrue
(eqv? 2 2.0)                     \ev  \schfalse
(eqv? '() '())                   \ev  \schtrue
(eqv? 100000000 100000000)       \ev  \schtrue
(eqv? 0.0 +nan.0)                \ev  \schfalse
(eqv? (cons 1 2) (cons 1 2))     \ev  \schfalse
(eqv? (lambda () 1)
      (lambda () 2))             \ev  \schfalse
(let ((p (lambda (x) x)))
  (eqv? p p))                    \ev  \schtrue
(eqv? #f 'nil)                   \ev  \schfalse
```

The following examples illustrate cases in which the above rules do
not fully specify the behavior of ``eqv?``.  All that can be said
about such cases is that the value returned by ``eqv?`` must be a
boolean.

```
(eqv? "" "")             \ev  \unspecified
(eqv? '#() '#())         \ev  \unspecified
(eqv? (lambda (x) x)
      (lambda (x) x))    \ev  \unspecified
(eqv? (lambda (x) x)
      (lambda (y) y))    \ev  \unspecified
(eqv? 1.0e0 1.0f0)       \ev  \unspecified
(eqv? +nan.0 +nan.0)     \ev  \unspecified
```

Note that ``(eqv? 0.0 -0.0)`` will return {{< tt "#f" >}} if negative zero
is distinguished, and {{< tt "#t" >}} if negative zero is not distinguished.

The next set of examples shows the use of ``eqv?`` with procedures
that have local state.  The ``gen-counter`` procedure must return a distinct
procedure every time, since each procedure has its own internal counter.
The ``gen-loser`` procedure, however, returns operationally equivalent procedures each time, since
the local state does not affect the value or side effects of the
procedures.  However, ``eqv?`` may or may not detect this equivalence.

```
(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter)))
  (eqv? g g))           \ev  \schtrue
(eqv? (gen-counter) (gen-counter))
                        \ev  \schfalse
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(let ((g (gen-loser)))
  (eqv? g g))           \ev  \schtrue
(eqv? (gen-loser) (gen-loser))
                        \ev  \unspecified

(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
         (g (lambda () (if (eqv? f g) 'both 'g))))
  (eqv? f g))
                        \ev  \unspecified

(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
  (eqv? f g))
                        \ev  \schfalse
```

Since it is an error to modify constant objects (those returned by
literal expressions), implementations may
share structure between constants where appropriate.  Thus
the value of ``eqv?`` on constants is sometimes
implementation-dependent.

```
(eqv? '(a) '(a))                 \ev  \unspecified
(eqv? "a" "a")                   \ev  \unspecified
(eqv? '(b) (cdr '(a b)))	     \ev  \unspecified
(let ((x '(a)))
  (eqv? x x))                    \ev  \schtrue
```

The above definition of ``eqv?`` allows implementations latitude in
their treatment of procedures and literals:  implementations may
either detect or fail to detect that two procedures or two literals
are equivalent to each other, and can decide whether or not to
merge representations of equivalent objects by using the same pointer or
bit pattern to represent both.

*Note:&nbsp;*
If inexact numbers are represented as IEEE binary floating-point numbers,
then an implementation of ``eqv?`` that simply compares equal-sized
inexact numbers for bitwise equality is correct by the above definition.


\end{entry}


\begin{entry}{
\proto{eq?}{ \vari{obj} \varii{obj}}{procedure}}

The ``eq?`` procedure is similar to ``eqv?`` except that in some cases it is
capable of discerning distinctions finer than those detectable by
``eqv?``.  It must always return {{< tt "#f" >}} when ``eqv?`` also
would, but may return {{< tt "#f" >}} in some cases where ``eqv?`` would return {{< tt "#t" >}}.

On symbols, booleans, the empty list, pairs, and records,
and also on non-empty
strings, vectors, and bytevectors, ``eq?`` and ``eqv?`` are guaranteed to have the same
behavior.  On procedures, ``eq?`` must return true if the arguments' location
tags are equal.  On numbers and characters, ``eq?``'s behavior is
implementation-dependent, but it will always return either true or
false.  On empty strings, empty vectors, and empty bytevectors, ``eq?`` may also behave
differently from ``eqv?``.

```
(eq? 'a 'a)                     \ev  \schtrue
(eq? '(a) '(a))                 \ev  \unspecified
(eq? (list 'a) (list 'a))       \ev  \schfalse
(eq? "a" "a")                   \ev  \unspecified
(eq? "" "")                     \ev  \unspecified
(eq? '() '())                   \ev  \schtrue
(eq? 2 2)                       \ev  \unspecified
(eq? #\A #\A)                   \ev  \unspecified
(eq? car car)                   \ev  \schtrue
(let ((n (+ 2 3)))
  (eq? n n))      \ev  \unspecified
(let ((x '(a)))
  (eq? x x))      \ev  \schtrue
(let ((x '#()))
  (eq? x x))      \ev  \schtrue
(let ((p (lambda (x) x)))
  (eq? p p))      \ev  \schtrue
```


\begin{rationale} It will usually be possible to implement ``eq?`` much
more efficiently than ``eqv?``, for example, as a simple pointer
comparison instead of as some more complicated operation.  One reason is
that it is not always possible to compute ``eqv?`` of two numbers in
constant time, whereas ``eq?`` implemented as pointer comparison will
always finish in constant time.
\end{rationale}

\end{entry}


\begin{entry}{
\proto{equal?}{ \vari{obj} \varii{obj}}{procedure}}

The ``equal?`` procedure, when applied to pairs, vectors, strings and
bytevectors, recursively compares them, returning {{< tt "#t" >}} when the
unfoldings of its arguments into (possibly infinite) trees are equal
(in the sense of ``equal?``)
as ordered trees, and {{< tt "#f" >}} otherwise.  It returns the same as
``eqv?`` when applied to booleans, symbols, numbers, characters,
ports, procedures, and the empty list.  If two objects are ``eqv?``,
they must be ``equal?`` as well.  In all other cases, ``equal?``
may return either {{< tt "#t" >}} or {{< tt "#f" >}}.

Even if its arguments are
circular data structures, ``equal?`` must always terminate.

```
(equal? 'a 'a)                  \ev  \schtrue
(equal? '(a) '(a))              \ev  \schtrue
(equal? '(a (b) c)
        '(a (b) c))             \ev  \schtrue
(equal? "abc" "abc")            \ev  \schtrue
(equal? 2 2)                    \ev  \schtrue
(equal? (make-vector 5 'a)
        (make-vector 5 'a))     \ev  \schtrue
(equal? '#1=(a b . #1#)
        '#2=(a b a b . #2#))    \ev  \schtrue
(equal? (lambda (x) x)
        (lambda (y) y))  \ev  \unspecified
```

*Note:&nbsp;*
A rule of thumb is that objects are generally ``equal?`` if they print
the same.




\end{entry}


## 6.2. Numbers
{{< label "numbersection" >}}


\newcommand{\type}[1]{{\it#1}}
\newcommand{\tupe}[1]{{#1}}

It is important to distinguish between mathematical numbers, the
Scheme numbers that attempt to model them, the machine representations
used to implement the Scheme numbers, and notations used to write numbers.
This report uses the types \type{number}, \type{complex}, \type{real},
\type{rational}, and \type{integer} to refer to both mathematical numbers
and Scheme numbers.

### 6.2.1. Numerical types
{{< label "numericaltypes" >}}


Mathematically, numbers are arranged into a tower of subtypes
in which each level is a subset of the level above it:
\begin{tabbing}
         \=\tupe{number} \\
\> \tupe{complex number} \\
\> \tupe{real number} \\
\> \tupe{rational number} \\
\> \tupe{integer}
\end{tabbing}

For example, 3 is an integer.  Therefore 3 is also a rational,
a real, and a complex number.  The same is true of the Scheme numbers
that model 3.  For Scheme numbers, these types are defined by the
predicates \ide{number?}, \ide{complex?}, \ide{real?}, \ide{rational?},
and \ide{integer?}.

There is no simple relationship between a number's type and its
representation inside a computer.  Although most implementations of
Scheme will offer at least two different representations of 3, these
different representations denote the same integer.

Scheme's numerical operations treat numbers as abstract data, as
independent of their representation as possible.  Although an implementation
of Scheme may use multiple internal representations of
numbers, this ought not to be apparent to a casual programmer writing
simple programs.

### 6.2.2. Exactness

 {{< label "exactly" >}}

It is useful to distinguish between numbers that are
represented exactly and those that might not be.  For example, indexes
into data structures must be known exactly, as must some polynomial
coefficients in a symbolic algebra system.  On the other hand, the
results of measurements are inherently inexact, and irrational numbers
may be approximated by rational and therefore inexact approximations.
In order to catch uses of inexact numbers where exact numbers are
required, Scheme explicitly distinguishes exact from inexact numbers.
This distinction is orthogonal to the dimension of type.

A Scheme number is
\type{exact} if it was written as an exact constant or was derived from
\tupe{exact} numbers using only \tupe{exact} operations.  A number is
\type{inexact} if it was written as an inexact constant,
if it was
derived using \tupe{inexact} ingredients, or if it was derived using
\tupe{inexact} operations. Thus \tupe{inexact}ness is a contagious
property of a number.
In particular, an \defining{exact complex number} has an exact real part
and an exact imaginary part; all other complex numbers are \defining{inexact
complex numbers}.

If two implementations produce \tupe{exact} results for a
computation that did not involve \tupe{inexact} intermediate results,
the two ultimate results will be mathematically equal.  This is
generally not true of computations involving \tupe{inexact} numbers
since approximate methods such as floating-point arithmetic may be used,
but it is the duty of each implementation to make the result as close as
practical to the mathematically ideal result.

Rational operations such as ``+`` should always produce
\tupe{exact} results when given \tupe{exact} arguments.
If the operation is unable to produce an \tupe{exact} result,
then it may either report the violation of an implementation restriction
or it may silently coerce its
result to an \tupe{inexact} value.
However, ``(/~3~4)`` must not return the mathematically incorrect value ``0``.
See section~\ref{restrictions}.

Except for \ide{exact}, the operations described in
this section must generally return inexact results when given any inexact
arguments.  An operation may, however, return an \tupe{exact} result if it can
prove that the value of the result is unaffected by the inexactness of its
arguments.  For example, multiplication of any number by an \tupe{exact} zero
may produce an \tupe{exact} zero result, even if the other argument is
\tupe{inexact}.

Specifically, the expression ``(* 0 +inf.0)`` may return ``0``,
or ``+nan.0``, or report that inexact numbers are not supported,
or report that non-rational real numbers are not supported, or fail
silently or noisily in other implementation-specific ways.

### 6.2.3. Implementation restrictions

{{< label "restrictions" >}}

Implementations of Scheme are not required to implement the whole
tower of subtypes given in section~\ref{numericaltypes},
but they must implement a coherent subset consistent with both the
purposes of the implementation and the spirit of the Scheme language.
For example, implementations in which all numbers are \tupe{real},
or in which non-\tupe{real} numbers are always \tupe{inexact},
or in which \tupe{exact} numbers are always \tupe{integer},
are still quite useful.

Implementations may also support only a limited range of numbers of
any type, subject to the requirements of this section.  The supported
range for \tupe{exact} numbers of any type may be different from the
supported range for \tupe{inexact} numbers of that type.  For example,
an implementation that uses IEEE binary double-precision floating-point numbers to represent all its
\tupe{inexact} \tupe{real} numbers may also
support a practically unbounded range of \tupe{exact} \tupe{integer}s
and \tupe{rational}s
while limiting the range of \tupe{inexact} \tupe{real}s (and therefore
the range of \tupe{inexact} \tupe{integer}s and \tupe{rational}s)
to the dynamic range of the IEEE binary double format.
Furthermore,
the gaps between the representable \tupe{inexact} \tupe{integer}s and
\tupe{rational}s are
likely to be very large in such an implementation as the limits of this
range are approached.

An implementation of Scheme must support exact integers
throughout the range of numbers permitted as indexes of
lists, vectors, bytevectors, and strings or that result from computing the length of
one of these.  The \ide{length}, \ide{vector-length},
\ide{bytevector-length}, and \ide{string-length} procedures must return an exact
integer, and it is an error to use anything but an exact integer as an
index.  Furthermore, any integer constant within the index range, if
expressed by an exact integer syntax, must be read as an exact
integer, regardless of any implementation restrictions that apply
outside this range.  Finally, the procedures listed below will always
return exact integer results provided all their arguments are exact integers
and the mathematically expected results are representable as exact integers
within the implementation:

```
-                     *
+                     abs
ceiling               denominator
exact-integer-sqrt    expt
floor                 floor/
floor-quotient        floor-remainder
gcd                   lcm
max                   min
modulo                numerator
quotient              rationalize
remainder             round
square                truncate
truncate/             truncate-quotient
truncate-remainder
```

It is recommended, but not required, that implementations support
\tupe{exact} \tupe{integer}s and \tupe{exact} \tupe{rational}s of
practically unlimited size and precision, and to implement the
above procedures and the ``/`` procedure in
such a way that they always return \tupe{exact} results when given \tupe{exact}
arguments.  If one of these procedures is unable to deliver an \tupe{exact}
result when given \tupe{exact} arguments, then it may either report a
violation of an
implementation restriction or it may silently coerce its result to an
\tupe{inexact} number; such a coercion can cause an error later.
Nevertheless, implementations that do not provide \tupe{exact} rational
numbers should return \tupe{inexact} rational numbers rather than
reporting an implementation restriction.

An implementation may use floating-point and other approximate
representation strategies for \tupe{inexact} numbers.
This report recommends, but does not require, that
implementations that use
floating-point representations
follow the IEEE 754 standard,
and that implementations using
other representations should match or exceed the precision achievable
using these floating-point standards {{< cite "IEEE" >}}.
In particular, the description of transcendental functions in IEEE 754-2008
should be followed by such implementations, particularly with respect
to infinities and NaNs.

Although Scheme allows a variety of written
notations for
numbers, any particular implementation may support only some of them.
For example, an implementation in which all numbers are \tupe{real}
need not support the rectangular and polar notations for complex
numbers.  If an implementation encounters an \tupe{exact} numerical constant that
it cannot represent as an \tupe{exact} number, then it may either report a
violation of an implementation restriction or it may silently represent the
constant by an \tupe{inexact} number.

### 6.2.4. Implementation extensions


Implementations may provide more than one representation of
floating-point numbers with differing precisions.  In an implementation
which does so, an inexact result must be represented with at least
as much precision as is used to express any of the inexact arguments
to that operation.  Although it is desirable for potentially inexact
operations such as ``sqrt`` to produce \tupe{exact} answers when
applied to \tupe{exact} arguments, if an \tupe{exact} number is operated
upon so as to produce an \tupe{inexact} result, then the most precise
representation available must be used.  For example, the value of {\cf
(sqrt 4)} should be ``2``, but in an implementation that provides both
single and double precision floating point numbers it may be the latter
but must not be the former.

It is the programmer's responsibility to avoid using inexact
number objects with magnitude or significand too large to be
represented in the implementation.

In addition, implementations may
distinguish special numbers called \tupe{positive infinity},
\tupe{negative infinity}, \tupe{NaN}, and \tupe{negative zero}.

Positive infinity is regarded as an inexact real (but not rational)
number that represents an indeterminate value greater than the
numbers represented by all rational numbers. Negative infinity
is regarded as an inexact real (but not rational) number that
represents an indeterminate value less than the numbers represented
by all rational numbers.

Adding or multiplying an infinite value by any finite real value results
in an appropriately signed infinity; however, the sum of positive and
negative infinities is a NaN.  Positive infinity is the reciprocal
of zero, and negative infinity is the reciprocal of negative zero.
The behavior of the transcendental functions is sensitive to infinity
in accordance with IEEE 754.

A NaN is regarded as an inexact real (but not rational) number
so indeterminate that it might represent any real value, including
positive or negative infinity, and might even be greater than positive
infinity or less than negative infinity.
An implementation that does not support non-real numbers may use NaN
to represent non-real values like ``(sqrt -1.0)`` and ``(asin 2.0)``.

A NaN always compares false to any number, including a NaN.
An arithmetic operation where one operand is NaN returns NaN, unless the
implementation can prove that the result would be the same if the NaN
were replaced by any rational number.  Dividing zero by zero results in
NaN unless both zeros are exact.


Negative zero is an inexact real value written ``-0.0`` and is distinct
(in the sense of ``eqv?``) from ``0.0``.  A Scheme implementation
is not required to distinguish negative zero.  If it does, however, the
behavior of the transcendental functions is sensitive to the distinction
in accordance with IEEE 754.
Specifically, in a Scheme implementing both complex numbers and negative zero,
the branch cut of the complex logarithm function is such that
``(imag-part (log -1.0-0.0i))`` is $-\pi$ rather than $\pi$.

Furthermore, the negation of negative zero is ordinary zero and vice
versa.  This implies that the sum of two or more negative zeros is negative,
and the result of subtracting (positive) zero from a negative zero is
likewise negative.  However, numerical comparisons treat negative zero
as equal to zero.

Note that both the real and the imaginary parts of a complex number
can be infinities, NaNs, or negative zero.

### 6.2.5. Syntax of numerical constants
{{< label "numbernotations" >}}

The syntax of the written representations for numbers is described formally in
section~\ref{numbersyntax}.  Note that case is not significant in numerical
constants.

A number can be written in binary, octal, decimal, or
hexa\-decimal by the use of a radix prefix.  The radix prefixes are {\cf
#b}\sharpindex{b} (binary), ``#o``\sharpindex{o} (octal), {\cf
#d}\sharpindex{d} (decimal), and ``#x``\sharpindex{x} (hexa\-decimal).  With
no radix prefix, a number is assumed to be expressed in decimal.

A
numerical constant can be specified to be either \tupe{exact} or
\tupe{inexact} by a prefix.  The prefixes are ``#e``\sharpindex{e}
for \tupe{exact}, and ``#i``\sharpindex{i} for \tupe{inexact}.  An exactness
prefix can appear before or after any radix prefix that is used.  If
the written representation of a number has no exactness prefix, the
constant is
\tupe{inexact} if it contains a decimal point or an
exponent.
Otherwise, it is \tupe{exact}.

In systems with \tupe{inexact} numbers
of varying precisions it can be useful to specify
the precision of a constant.  For this purpose,
implementations may accept numerical constants
written with an exponent marker that indicates the
desired precision of the \tupe{inexact}
representation.  If so, the letter ``s``, ``f``,
``d``, or ``l``, meaning _short_, _single_,
_double_, or _long_ precision, respectively,
can be used in place of ``e``.
The default precision has at least as much precision
as _double_, but
implementations may allow this default to be set by the user.

```
3.14159265358979F0
       {\rm Round to single ---} 3.141593
0.6L0
       {\rm Extend to long ---} .600000000000000
```

The numbers positive infinity, negative infinity, and NaN are written
``+inf.0``, ``-inf.0`` and ``+nan.0`` respectively.
NaN may also be written ``-nan.0``.
The use of signs in the written representation does not necessarily
reflect the underlying sign of the NaN value, if any.
Implementations are not required to support these numbers, but if they do,
they must do so in general conformance with IEEE 754.  However, implementations
are not required to support signaling NaNs, nor to provide a way to distinguish
between different NaNs.

There are two notations provided for non-real complex numbers:
the \defining{rectangular notation}
_a_``+``_b_``i``,
where _a_ is the real part and _b_ is the imaginary part;
and the \defining{polar notation}
_r_``@``$\theta$,
where _r_ is the magnitude and $\theta$ is the phase (angle) in radians.
These are related by the equation
$a+b\mathrm{i} = r \cos\theta + (r \sin\theta) \mathrm{i}$.
All of _a_, _b_, _r_, and $\theta$ are real numbers.


### 6.2.6. Numerical operations

The reader is referred to section~\ref{typeconventions} for a summary
of the naming conventions used to specify restrictions on the types of
arguments to numerical routines.
The examples used in this section assume that any numerical constant written
using an \tupe{exact} notation is indeed represented as an \tupe{exact}
number.  Some examples also assume that certain numerical constants written
using an \tupe{inexact} notation can be represented without loss of
accuracy; the \tupe{inexact} constants were chosen so that this is
likely to be true in implementations that use IEEE binary doubles to represent
inexact numbers.



\begin{entry}{
\proto{number?}{ obj}{procedure}
\proto{complex?}{ obj}{procedure}
\proto{real?}{ obj}{procedure}
\proto{rational?}{ obj}{procedure}
\proto{integer?}{ obj}{procedure}}

These numerical type predicates can be applied to any kind of
argument, including non-numbers.  They return {{< tt "#t" >}} if the object is
of the named type, and otherwise they return {{< tt "#f" >}}.
In general, if a type predicate is true of a number then all higher
type predicates are also true of that number.  Consequently, if a type
predicate is false of a number, then all lower type predicates are
also false of that number.

If \vr{z} is a complex number, then ``(real? \vr{z``)} is true if
and only if ``(zero? (imag-part \vr{z``))} is true.
If \vr{x} is an inexact real number, then {\cf
(integer? \vr{x})} is true if and only if ``(= \vr{x`` (round \vr{x}))}.

The numbers ``+inf.0``, ``-inf.0``, and ``+nan.0`` are real but
not rational.

```
(complex? 3+4i)         \ev  \schtrue
(complex? 3)            \ev  \schtrue
(real? 3)               \ev  \schtrue
(real? -2.5+0i)         \ev  \schtrue
(real? -2.5+0.0i)       \ev  \schfalse
(real? #e1e10)          \ev  \schtrue
(real? +inf.0)          \ev  \schtrue
(real? +nan.0)          \ev  \schtrue
(rational? -inf.0)      \ev  \schfalse
(rational? 3.5)         \ev  \schtrue
(rational? 6/10)        \ev  \schtrue
(rational? 6/3)         \ev  \schtrue
(integer? 3+0i)         \ev  \schtrue
(integer? 3.0)          \ev  \schtrue
(integer? 8/4)          \ev  \schtrue
```

*Note:&nbsp;*
The behavior of these type predicates on \tupe{inexact} numbers
is unreliable, since any inaccuracy might affect the result.


*Note:&nbsp;*
In many implementations the \ide{complex?} procedure will be the same as
\ide{number?}, but unusual implementations may represent
some irrational numbers exactly or may extend the number system to
support some kind of non-complex numbers.


\end{entry}

\begin{entry}{
\proto{exact?}{ \vr{z}}{procedure}
\proto{inexact?}{ \vr{z}}{procedure}}

These numerical predicates provide tests for the exactness of a
quantity.  For any Scheme number, precisely one of these predicates
is true.

```
(exact? 3.0)           \ev  \schfalse
(exact? #e3.0)         \ev  \schtrue
(inexact? 3.)          \ev  \schtrue
```

\end{entry}


\begin{entry}{
\proto{exact-integer?}{ \vr{z}}{procedure}}

Returns {{< tt "#t" >}} if \vr{z} is both \tupe{exact} and an \tupe{integer};
otherwise returns {{< tt "#f" >}}.

```
(exact-integer? 32) \ev {{< tt "#t" >}}
(exact-integer? 32.0) \ev {{< tt "#f" >}}
(exact-integer? 32/5) \ev {{< tt "#f" >}}
```
\end{entry}


\begin{entry}{
\proto{finite?}{ \vr{z}}{inexact library procedure}}

The ``finite?`` procedure returns {{< tt "#t" >}} on all real numbers except
``+inf.0``, ``-inf.0``, and ``+nan.0``, and on complex
numbers if their real and imaginary parts are both finite.
Otherwise it returns {{< tt "#f" >}}.

```
(finite? 3)            \ev  \schtrue
(finite? +inf.0)       \ev  \schfalse
(finite? 3.0+inf.0i)   \ev  \schfalse
```
\end{entry}

\begin{entry}{
\proto{infinite?}{ \vr{z}}{inexact library procedure}}

The ``infinite?`` procedure returns {{< tt "#t" >}} on the real numbers
``+inf.0`` and ``-inf.0``, and on complex
numbers if their real or imaginary parts or both are infinite.
Otherwise it returns {{< tt "#f" >}}.

```
(infinite? 3)            \ev  \schfalse
(infinite? +inf.0)       \ev  \schtrue
(infinite? +nan.0)       \ev  \schfalse
(infinite? 3.0+inf.0i)   \ev  \schtrue
```
\end{entry}

\begin{entry}{
\proto{nan?}{ \vr{z}}{inexact library procedure}}

The ``nan?`` procedure returns {{< tt "#t" >}} on ``+nan.0``, and on complex
numbers if their real or imaginary parts or both are ``+nan.0``.
Otherwise it returns {{< tt "#f" >}}.

```
(nan? +nan.0)          \ev  \schtrue
(nan? 32)              \ev  \schfalse
(nan? +nan.0+5.0i)     \ev  \schtrue
(nan? 1+2i)            \ev  \schfalse
```
\end{entry}


\begin{entry}{
\proto{=}{ \vri{z} \vrii{z} \vriii{z} ...}{procedure}
\proto{<}{ \vri{x} \vrii{x} \vriii{x} ...}{procedure}
\proto{>}{ \vri{x} \vrii{x} \vriii{x} ...}{procedure}
\proto{<=}{ \vri{x} \vrii{x} \vriii{x} ...}{procedure}
\proto{>=}{ \vri{x} \vrii{x} \vriii{x} ...}{procedure}}

These procedures return {{< tt "#t" >}} if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing,
monotonically non-decreasing, or monotonically non-increasing,
and {{< tt "#f" >}} otherwise.
If any of the arguments are ``+nan.0``, all the predicates return {{< tt "#f" >}}.
They do not distinguish between inexact zero and inexact negative zero.

These predicates are required to be transitive.

*Note:&nbsp;*
The implementation approach
of converting all arguments to inexact numbers
if any argument is inexact is not transitive.  For example, let
``big`` be ``(expt 2 1000)``, and assume that ``big`` is exact and that
inexact numbers are represented by 64-bit IEEE binary floating point numbers.
Then ``(= (- big 1) (inexact big))`` and
``(= (inexact big) (+ big 1))`` would both be true with this approach,
because of the limitations of IEEE
representations of large integers, whereas ``(= (- big 1) (+ big 1))``
is false.  Converting inexact values to exact numbers that are the same (in the sense of ``=``) to them will avoid
this problem, though special care must be taken with infinities.


*Note:&nbsp;*
While it is not an error to compare \tupe{inexact} numbers using these
predicates, the results are unreliable because a small inaccuracy
can affect the result; this is especially true of \ide{=} and \ide{zero?}.
When in doubt, consult a numerical analyst.


\end{entry}

\begin{entry}{
\proto{zero?}{ \vr{z}}{procedure}
\proto{positive?}{ \vr{x}}{procedure}
\proto{negative?}{ \vr{x}}{procedure}
\proto{odd?}{ \vr{n}}{procedure}
\proto{even?}{ \vr{n}}{procedure}}

These numerical predicates test a number for a particular property,
returning {{< tt "#t" >}} or \schfalse.  See note above.

\end{entry}

\begin{entry}{
\proto{max}{ \vri{x} \vrii{x} ...}{procedure}
\proto{min}{ \vri{x} \vrii{x} ...}{procedure}}

These procedures return the maximum or minimum of their arguments.

```
(max 3 4)              \ev  4    ; exact
(max 3.9 4)            \ev  4.0  ; inexact
```

*Note:&nbsp;*
If any argument is inexact, then the result will also be inexact (unless
the procedure can prove that the inaccuracy is not large enough to affect the
result, which is possible only in unusual implementations).  If ``min`` or
``max`` is used to compare numbers of mixed exactness, and the numerical
value of the result cannot be represented as an inexact number without loss of
accuracy, then the procedure may report a violation of an implementation
restriction.


\end{entry}


\begin{entry}{
\proto{+}{ \vri{z} ...}{procedure}
\proto{*}{ \vri{z} ...}{procedure}}

These procedures return the sum or product of their arguments.

```
(+ 3 4)                 \ev  7
(+ 3)                   \ev  3
(+)                     \ev  0
(* 4)                   \ev  4
(*)                     \ev  1
```

\end{entry}


\begin{entry}{
\proto{-}{ \vr{z}}{procedure}
\rproto{-}{ \vri{z} \vrii{z} ...}{procedure}
\proto{/}{ \vr{z}}{procedure}
\rproto{/}{ \vri{z} \vrii{z} ...}{procedure}}

With two or more arguments, these procedures return the difference or
quotient of their arguments, associating to the left.  With one argument,
however, they return the additive or multiplicative inverse of their argument.

It is an error if any argument of ``/`` other than the first is an exact zero.
If the first argument is an exact zero, an implementation may return an
exact zero unless one of the other arguments is a NaN.

```
(- 3 4)                 \ev  -1
(- 3 4 5)               \ev  -6
(- 3)                   \ev  -3
(/ 3 4 5)               \ev  3/20
(/ 3)                   \ev  1/3
```

\end{entry}


\begin{entry}{
\proto{abs}{ x}{procedure}}

The ``abs`` procedure returns the absolute value of its argument.
```
(abs -7)                \ev  7
```
\end{entry}


\begin{entry}{
\proto{floor/}{ \vri{n} \vrii{n}}{procedure}
\proto{floor-quotient}{ \vri{n} \vrii{n}}{procedure}
\proto{floor-remainder}{ \vri{n} \vrii{n}}{procedure}
\proto{truncate/}{ \vri{n} \vrii{n}}{procedure}
\proto{truncate-quotient}{ \vri{n} \vrii{n}}{procedure}
\proto{truncate-remainder}{ \vri{n} \vrii{n}}{procedure}}

These procedures implement
number-theoretic (integer) division.  It is an error if \vrii{n} is zero.
The procedures ending in ``/`` return two integers; the other
procedures return an integer.  All the procedures compute a
quotient \vr{n_q} and remainder \vr{n_r} such that
$\vri{n} = \vrii{n} \vr{n_q} + \vr{n_r}$.  For each of the
division operators, there are three procedures defined as follows:

```
({{< hyper "operator" >}}/ \vri{n} \vrii{n})             \ev \vr{n_q} \vr{n_r}
({{< hyper "operator" >}}-quotient \vri{n} \vrii{n})     \ev \vr{n_q}
({{< hyper "operator" >}}-remainder \vri{n} \vrii{n})    \ev \vr{n_r}
```

The remainder \vr{n_r} is determined by the choice of integer
\vr{n_q}: $\vr{n_r} = \vri{n} - \vrii{n} \vr{n_q}$.  Each set of
operators uses a different choice of \vr{n_q}:

\begin{tabular}{l l}
\texttt{floor}     & $\vr{n_q} = \lfloor\vri{n} / \vrii{n}\rfloor$ \\
\texttt{truncate}  & $\vr{n_q} = \text{truncate}(\vri{n} / \vrii{n})$ \\
\end{tabular}

For any of the operators, and for integers \vri{n} and \vrii{n}
with \vrii{n} not equal to 0,
```
     (= \vri{n} (+ (* \vrii{n} ({{< hyper "operator" >}}-quotient \vri{n} \vrii{n}))
           ({{< hyper "operator" >}}-remainder \vri{n} \vrii{n})))
                                 \ev  \schtrue
```
provided all numbers involved in that computation are exact.

Examples:

```
(floor/ 5 2)         \ev 2 1
(floor/ -5 2)        \ev -3 1
(floor/ 5 -2)        \ev -3 -1
(floor/ -5 -2)       \ev 2 -1
(truncate/ 5 2)      \ev 2 1
(truncate/ -5 2)     \ev -2 -1
(truncate/ 5 -2)     \ev -2 1
(truncate/ -5 -2)    \ev 2 -1
(truncate/ -5.0 -2)  \ev 2.0 -1.0
```

\end{entry}


\begin{entry}{
\proto{quotient}{ \vri{n} \vrii{n}}{procedure}
\proto{remainder}{ \vri{n} \vrii{n}}{procedure}
\proto{modulo}{ \vri{n} \vrii{n}}{procedure}}

The ``quotient`` and ``remainder`` procedures are equivalent to {\cf
truncate-quotient} and ``truncate-remainder``, respectively, and {\cf
modulo} is equivalent to ``floor-remainder``.

*Note:&nbsp;*
These procedures are provided for backward compatibility with earlier
versions of this report.

\end{entry}

\begin{entry}{
\proto{gcd}{ \vri{n} ...}{procedure}
\proto{lcm}{ \vri{n} ...}{procedure}}

These procedures return the greatest common divisor or least common
multiple of their arguments.  The result is always non-negative.

```
(gcd 32 -36)            \ev  4
(gcd)                   \ev  0
(lcm 32 -36)            \ev  288
(lcm 32.0 -36)          \ev  288.0  ; inexact
(lcm)                   \ev  1
```

\end{entry}


\begin{entry}{
\proto{numerator}{ \vr{q}}{procedure}
\proto{denominator}{ \vr{q}}{procedure}}

These procedures return the numerator or denominator of their
argument; the result is computed as if the argument was represented as
a fraction in lowest terms.  The denominator is always positive.  The
denominator of 0 is defined to be 1.

```
(numerator (/ 6 4))  \ev  3
(denominator (/ 6 4))  \ev  2
(denominator
  (inexact (/ 6 4))) \ev 2.0
```

\end{entry}


\begin{entry}{
\proto{floor}{ x}{procedure}
\proto{ceiling}{ x}{procedure}
\proto{truncate}{ x}{procedure}
\proto{round}{ x}{procedure}
}

These procedures return integers.
The ``floor`` procedure returns the largest integer not larger than \vr{x}.
The ``ceiling`` procedure returns the smallest integer not smaller than~\vr{x},
``truncate`` returns the integer closest to \vr{x} whose absolute
value is not larger than the absolute value of \vr{x}, and ``round`` returns the
closest integer to \vr{x}, rounding to even when \vr{x} is halfway between two
integers.

\begin{rationale}
The ``round`` procedure rounds to even for consistency with the default rounding
mode specified by the IEEE 754 IEEE floating-point standard.
\end{rationale}

*Note:&nbsp;*
If the argument to one of these procedures is inexact, then the result
will also be inexact.  If an exact value is needed, the
result can be passed to the ``exact`` procedure.
If the argument is infinite or a NaN, then it is returned.


```
(floor -4.3)          \ev  -5.0
(ceiling -4.3)        \ev  -4.0
(truncate -4.3)       \ev  -4.0
(round -4.3)          \ev  -4.0

(floor 3.5)           \ev  3.0
(ceiling 3.5)         \ev  4.0
(truncate 3.5)        \ev  3.0
(round 3.5)           \ev  4.0  ; inexact

(round 7/2)           \ev  4    ; exact
(round 7)             \ev  7
```

\end{entry}

\begin{entry}{
\proto{rationalize}{ x y}{procedure}
}

The ``rationalize`` procedure returns the _simplest_ rational number
differing from \vr{x} by no more than \vr{y}.  A rational number $r_1$ is
_simpler_  than another rational number
$r_2$ if $r_1 = p_1/q_1$ and $r_2 = p_2/q_2$ (in lowest terms) and $|p_1|
\leq |p_2|$ and $|q_1| \leq |q_2|$.  Thus $3/5$ is simpler than $4/7$.
Although not all rationals are comparable in this ordering (consider $2/7$
and $3/5$), any interval contains a rational number that is simpler than
every other rational number in that interval (the simpler $2/5$ lies
between $2/7$ and $3/5$).  Note that $0 = 0/1$ is the simplest rational of
all.

```
(rationalize
  (exact .3) 1/10)  \ev 1/3    ; exact
(rationalize .3 1/10)        \ev #i1/3  ; inexact
```

\end{entry}

\begin{entry}{
\proto{exp}{ \vr{z}}{inexact library procedure}
\proto{log}{ \vr{z}}{inexact library procedure}
\rproto{log}{ \vri{z} \vrii{z}}{inexact library procedure}
\proto{sin}{ \vr{z}}{inexact library procedure}
\proto{cos}{ \vr{z}}{inexact library procedure}
\proto{tan}{ \vr{z}}{inexact library procedure}
\proto{asin}{ \vr{z}}{inexact library procedure}
\proto{acos}{ \vr{z}}{inexact library procedure}
\proto{atan}{ \vr{z}}{inexact library procedure}
\rproto{atan}{ \vr{y} \vr{x}}{inexact library procedure}}

These procedures
compute the usual transcendental functions.  The ``log`` procedure
computes the natural logarithm of \vr{z} (not the base ten logarithm)
if a single argument is given, or the base-\vrii{z} logarithm of \vri{z}
if two arguments are given.
The ``asin``, ``acos``, and ``atan`` procedures compute arcsine ($\sin^{-1}$),
arc-cosine ($\cos^{-1}$), and arctangent ($\tan^{-1}$), respectively.
The two-argument variant of ``atan`` computes {\tt (angle
(make-rectangular \vr{x} \vr{y}))} (see below), even in implementations
that don't support complex numbers.

In general, the mathematical functions log, arcsine, arc-cosine, and
arctangent are multiply defined.
The value of $\log z$ is defined to be the one whose imaginary part
lies in the range from $-\pi$ (inclusive if ``-0.0`` is distinguished,
exclusive otherwise) to $\pi$ (inclusive).
The value of $\log 0$ is mathematically undefined.
With $\log$ defined this way, the values of $\sin^{-1} z$, $\cos^{-1} z$,
and $\tan^{-1} z$ are according to the following formul\ae:
$$\sin^{-1} z = -i \log (i z + \sqrt{1 - z^2})$$
$$\cos^{-1} z = \pi / 2 - \sin^{-1} z$$
$$\tan^{-1} z = (\log (1 + i z) - \log (1 - i z)) / (2 i)$$

However, ``(log 0.0)`` returns ``-inf.0``
(and ``(log -0.0)`` returns ``-inf.0+$\pi$i``) if the
implementation supports infinities (and ``-0.0``).

The range of \texttt{(``atan`` _y_ _x_)} is as in the
following table. The asterisk (*) indicates that the entry applies to
implementations that distinguish minus zero.

\begin{center}
\begin{tabular}{clll}
& $y$ condition & $x$ condition & range of result $r$\\\hline
& $y = 0.0$ & $x > 0.0$ & $0.0$\\
$\ast$ & $y = +0.0$  & $x > 0.0$ & $+0.0$\\
$\ast$ & $y = -0.0$ & $x > 0.0$ & $-0.0$\\
& $y > 0.0$ & $x > 0.0$ & $0.0 < r < \frac{\pi}{2}$\\
& $y > 0.0$ & $x = 0.0$ & $\frac{\pi}{2}$\\
& $y > 0.0$ & $x < 0.0$ & $\frac{\pi}{2} < r < \pi$\\
& $y = 0.0$ & $x < 0$ & $\pi$\\
$\ast$ & $y = +0.0$ & $x < 0.0$ & $\pi$\\
$\ast$ & $y = -0.0$ & $x < 0.0$ & $-\pi$\\
&$y < 0.0$ & $x < 0.0$ & $-\pi< r< -\frac{\pi}{2}$\\
&$y < 0.0$ & $x = 0.0$ & $-\frac{\pi}{2}$\\
&$y < 0.0$ & $x > 0.0$ & $-\frac{\pi}{2} < r< 0.0$\\
&$y = 0.0$ & $x = 0.0$ & undefined\\
$\ast$& $y = +0.0$ & $x = +0.0$ & $+0.0$\\
$\ast$& $y = -0.0$ & $x = +0.0$& $-0.0$\\
$\ast$& $y = +0.0$ & $x = -0.0$ & $\pi$\\
$\ast$& $y = -0.0$ & $x = -0.0$ & $-\pi$\\
$\ast$& $y = +0.0$ & $x = 0$ & $\frac{\pi}{2}$\\
$\ast$& $y = -0.0$ & $x = 0$    & $-\frac{\pi}{2}$
\end{tabular}
\end{center}

The above specification follows {{< cite "CLtL" >}}, which in turn
cites {{< cite "Penfield81" >}}; refer to these sources for more detailed
discussion of branch cuts, boundary conditions, and implementation of
these functions.  When it is possible, these procedures produce a real
result from a real argument.


\end{entry}

\begin{entry}{
\proto{square}{ \vr{z}}{procedure}}

Returns the square of \vr{z}.
This is equivalent to \texttt{(``*`` _z_ _z_)}.
```
(square 42)      \ev 1764
(square 2.0)     \ev 4.0
```


\end{entry}

\begin{entry}{
\proto{sqrt}{ \vr{z}}{inexact library procedure}}

Returns the principal square root of \vr{z}.  The result will have
either a positive real part, or a zero real part and a non-negative imaginary
part.

```
(sqrt 9)  \ev 3
(sqrt -1) \ev +i
```
\end{entry}


\begin{entry}{
\proto{exact-integer-sqrt}{ k}{procedure}}

Returns two non-negative exact integers $s$ and $r$ where
$_k_ = s^2 + r$ and $_k_ < (s+1)^2$.

```
(exact-integer-sqrt 4) \ev 2 0
(exact-integer-sqrt 5) \ev 2 1
```
\end{entry}


\begin{entry}{
\proto{expt}{ \vri{z} \vrii{z}}{procedure}}

Returns \vri{z} raised to the power \vrii{z}.  For nonzero \vri{z}, this is
$${z_1}^{z_2} = e^{z_2 \log {z_1}}$$
The value of $0^z$ is $1$ if ``(zero? z)``, $0$ if ``(real-part z)``
is positive, and an error otherwise.  Similarly for $0.0^z$,
with inexact results.
\end{entry}




\begin{entry}{
\proto{make-rectangular}{ \vri{x} \vrii{x}}{complex library procedure}
\proto{make-polar}{ \vriii{x} \vriv{x}}{complex library procedure}
\proto{real-part}{ \vr{z}}{complex library procedure}
\proto{imag-part}{ \vr{z}}{complex library procedure}
\proto{magnitude}{ \vr{z}}{complex library procedure}
\proto{angle}{ \vr{z}}{complex library procedure}}

Let \vri{x}, \vrii{x}, \vriii{x}, and \vriv{x} be
real numbers and \vr{z} be a complex number such that
 $$ \vr{z} = \vri{x} + \vrii{x}\hbox{$i$}
 = \vriii{x} \cdot e^{i x_4}$$
Then all of

```
(make-rectangular \vri{x} \vrii{x}) \ev \vr{z}
(make-polar \vriii{x} \vriv{x})     \ev \vr{z}
(real-part \vr{z})                  \ev \vri{x}
(imag-part \vr{z})                  \ev \vrii{x}
(magnitude \vr{z})                  \ev $|\vriii{x}|$
(angle \vr{z})                      \ev $x_{angle}$
```

are true, where $-\pi \le x_{angle} \le \pi$ with $x_{angle} = \vriv{x} + 2\pi n$
for some integer $n$.

The ``make-polar`` procedure may return an inexact complex number even if its
arguments are exact.
The ``real-part`` and ``imag-part`` procedures may return exact real
numbers when applied to an inexact complex number if the corresponding
argument passed to ``make-rectangular`` was exact.


\begin{rationale}
The ``magnitude`` procedure is the same as \ide{abs} for a real argument,
but ``abs`` is in the base library, whereas
``magnitude`` is in the optional complex library.
\end{rationale}

\end{entry}


\begin{entry}{
\proto{inexact}{ \vr{z}}{procedure}
\proto{exact}{ \vr{z}}{procedure}}

The procedure ``inexact`` returns an \tupe{inexact} representation of \vr{z}.
The value returned is the
\tupe{inexact} number that is numerically closest to the argument.
For inexact arguments, the result is the same as the argument. For exact
complex numbers, the result is a complex number whose real and imaginary
parts are the result of applying ``inexact`` to the real
and imaginary parts of the argument, respectively.
If an \tupe{exact} argument has no reasonably close \tupe{inexact} equivalent
(in the sense of ``=``),
then a violation of an implementation restriction may be reported.

The procedure ``exact`` returns an \tupe{exact} representation of
\vr{z}.  The value returned is the \tupe{exact} number that is numerically
closest to the argument.
For exact arguments, the result is the same as the argument. For inexact
non-integral real arguments, the implementation may return a rational
approximation, or may report an implementation violation. For inexact
complex arguments, the result is a complex number whose real and
imaginary parts are the result of applying ``exact`` to the
real and imaginary parts of the argument, respectively.
If an \tupe{inexact} argument has no reasonably close \tupe{exact} equivalent,
(in the sense of ``=``),
then a violation of an implementation restriction may be reported.

These procedures implement the natural one-to-one correspondence between
\tupe{exact} and \tupe{inexact} integers throughout an
implementation-dependent range.  See section~\ref{restrictions}.

*Note:&nbsp;*
These procedures were known in {{< rnrs 5 >}} as ``exact->inexact`` and
``inexact->exact``, respectively, but they have always accepted
arguments of any exactness.  The new names are clearer and shorter,
as well as being compatible with {{< rnrs 6 >}}.


\end{entry}

{{< medskip >}}

### 6.2.7. Numerical input and output

\begin{entry}{
\proto{number->string}{ z}{procedure}
\rproto{number->string}{ z radix}{procedure}}

\domain{It is an error if \vr{radix} is not one of 2, 8, 10, or 16.}
The procedure ``number\coerce{``string} takes a
number and a radix and returns as a string an external representation of
the given number in the given radix such that
```
(let ((number \vr{number})
      (radix \vr{radix}))
  (eqv? number
        (string->number (number->string number
                                        radix)
                        radix)))
```
is true.  It is an error if no possible result makes this expression true.
If omitted, \vr{radix} defaults to 10.

If \vr{z} is inexact, the radix is 10, and the above expression
can be satisfied by a result that contains a decimal point,
then the result contains a decimal point and is expressed using the
minimum number of digits (exclusive of exponent and trailing
zeroes) needed to make the above expression
true {{< cite "howtoprint,howtoread" >}};
otherwise the format of the result is unspecified.

The result returned by ``number\coerce{``string}
never contains an explicit radix prefix.

*Note:&nbsp;*
The error case can occur only when \vr{z} is not a complex number
or is a complex number with a non-rational real or imaginary part.


\begin{rationale}
If \vr{z} is an inexact number and
the radix is 10, then the above expression is normally satisfied by
a result containing a decimal point.  The unspecified case
allows for infinities, NaNs, and unusual representations.
\end{rationale}

\end{entry}


\begin{entry}{
\proto{string->number}{ string}{procedure}
\rproto{string->number}{ string radix}{procedure}}


Returns a number of the maximally precise representation expressed by the
given \vr{string}.
\domain{It is an error if \vr{radix} is not 2, 8, 10, or 16.}
If supplied, \vr{radix} is a default radix that will be overridden
if an explicit radix prefix is present in \vr{string} (e.g. {\tt "#o177"}).  If \vr{radix}
is not supplied, then the default radix is 10.  If \vr{string} is not
a syntactically valid notation for a number, or would result in a
number that the implementation cannot represent, then ``string->number``
returns {{< tt "#f" >}}.
An error is never signaled due to the content of \vr{string}.

```
(string->number "100")        \ev  100
(string->number "100" 16)     \ev  256
(string->number "1e2")        \ev  100.0
```

*Note:&nbsp;*
The domain of ``string->number`` may be restricted by implementations
in the following ways.
If all numbers supported by an implementation are real, then
``string->number`` is permitted to return {{< tt "#f" >}} whenever
\vr{string} uses the polar or rectangular notations for complex
numbers.  If all numbers are integers, then
``string->number`` may return {{< tt "#f" >}} whenever
the fractional notation is used.  If all numbers are exact, then
``string->number`` may return {{< tt "#f" >}} whenever
an exponent marker or explicit exactness prefix is used.
If all inexact
numbers are integers, then
``string->number`` may return {{< tt "#f" >}} whenever
a decimal point is used.

The rules used by a particular implementation for ``string->number`` must
also be applied to ``read`` and to the routine that reads programs, in
order to maintain consistency between internal numeric processing, I/O,
and the processing of programs.
As a consequence, the {{< rnrs 5 >}} permission to return {{< tt "#f" >}} when
_string_ has an explicit radix prefix has been withdrawn.


\end{entry}

## 6.3. Booleans
{{< label "booleansection" >}}

The standard boolean objects for true and false are written as
{{< tt "#t" >}} and \schfalse.\sharpindex{t}\sharpindex{f}
Alternatively, they can be written \sharptrue~and \sharpfalse,
respectively.  What really
matters, though, are the objects that the Scheme conditional expressions
(``if``, ``cond``, ``and``, ``or``, ``when``, ``unless``, ``do``) treat as
true or false.  The phrase "a true value"
(or sometimes just "true") means any object treated as true by the
conditional expressions, and the phrase "a false value" (or
"false") means any object treated as false by the conditional expressions.

Of all the Scheme values, only {{< tt "#f" >}}
counts as false in conditional expressions.
All other Scheme values, including \schtrue,
count as true.

*Note:&nbsp;*
Unlike some other dialects of Lisp,
Scheme distinguishes {{< tt "#f" >}} and the empty list
from each other and from the symbol \ide{nil}.


Boolean constants evaluate to themselves, so they do not need to be quoted
in programs.

```
\schtrue         \ev  \schtrue
\schfalse        \ev  \schfalse
'\schfalse       \ev  \schfalse
```


\begin{entry}{
\proto{not}{ obj}{procedure}}

The ``not`` procedure returns {{< tt "#t" >}} if _obj_ is false, and returns
{{< tt "#f" >}} otherwise.

```
(not \schtrue)   \ev  \schfalse
(not 3)          \ev  \schfalse
(not (list 3))   \ev  \schfalse
(not \schfalse)  \ev  \schtrue
(not '())        \ev  \schfalse
(not (list))     \ev  \schfalse
(not 'nil)       \ev  \schfalse
```

\end{entry}


\begin{entry}{
\proto{boolean?}{ obj}{procedure}}

The ``boolean?`` predicate returns {{< tt "#t" >}} if _obj_ is either {{< tt "#t" >}} or
{{< tt "#f" >}} and returns {{< tt "#f" >}} otherwise.

```
(boolean? \schfalse)  \ev  \schtrue
(boolean? 0)          \ev  \schfalse
(boolean? '())        \ev  \schfalse
```

\end{entry}

\begin{entry}{
\proto{boolean=?}{ \vari{boolean} \varii{boolean} \variii{boolean} ...}{procedure}}

Returns {{< tt "#t" >}} if all the arguments are booleans and all
are {{< tt "#t" >}} or all are {{< tt "#f" >}}.

\end{entry}

## 6.4. Pairs and lists
{{< label "listsection" >}}

A \defining{pair} (sometimes called a \defining{dotted pair}) is a
record structure with two fields called the car and cdr fields (for
historical reasons).  Pairs are created by the procedure ``cons``.
The car and cdr fields are accessed by the procedures ``car`` and
``cdr``.  The car and cdr fields are assigned by the procedures
``set-car!`` and ``set-cdr!``.

Pairs are used primarily to represent lists.  A \defining{list} can
be defined recursively as either the empty list or a pair whose
cdr is a list.  More precisely, the set of lists is defined as the smallest
set _X_ such that


- The empty list is in _X_.
- If _list_ is in _X_, then any pair whose cdr field contains
      _list_ is also in _X_.


The objects in the car fields of successive pairs of a list are the
elements of the list.  For example, a two-element list is a pair whose car
is the first element and whose cdr is a pair whose car is the second element
and whose cdr is the empty list.  The length of a list is the number of
elements, which is the same as the number of pairs.

The empty list is a special object of its own type.
It is not a pair, it has no elements, and its length is zero.

*Note:&nbsp;*
The above definitions imply that all lists have finite length and are
terminated by the empty list.


The most general notation (external representation) for Scheme pairs is
the "dotted" notation \hbox``(\vari{c`` . \varii{c})} where
\vari{c} is the value of the car field and \varii{c} is the value of the
cdr field.  For example ``(4 . 5)`` is a pair whose car is 4 and whose
cdr is 5.  Note that ``(4 . 5)`` is the external representation of a
pair, not an expression that evaluates to a pair.

A more streamlined notation can be used for lists: the elements of the
list are simply enclosed in parentheses and separated by spaces.  The
empty list is written {\tt()}.  For example,

```
(a b c d e)
```

and

```
(a . (b . (c . (d . (e . ())))))
```

are equivalent notations for a list of symbols.

A chain of pairs not ending in the empty list is called an
\defining{improper list}.  Note that an improper list is not a list.
The list and dotted notations can be combined to represent
improper lists:

```
(a b c . d)
```

is equivalent to

```
(a . (b . (c . d)))
```

Whether a given pair is a list depends upon what is stored in the cdr
field.  When the \ide{set-cdr!} procedure is used, an object can be a
list one moment and not the next:

```
(define x (list 'a 'b 'c))
(define y x)
y                       \ev  (a b c)
(list? y)               \ev  \schtrue
(set-cdr! x 4)          \ev  \unspecified
x                       \ev  (a . 4)
(eqv? x y)              \ev  \schtrue
y                       \ev  (a . 4)
(list? y)               \ev  \schfalse
(set-cdr! x x)          \ev  \unspecified
(list? x)               \ev  \schfalse
```

Within literal expressions and representations of objects read by the
\ide{read} procedure, the forms \singlequote{{< hyper "datum" >}}\schindex{'},
\backquote{{< hyper "datum" >}}, {\tt,}{{< hyper "datum" >}}\schindex{,}, and
{\tt,@}{{< hyper "datum" >}} denote two-ele\-ment lists whose first elements are
the symbols \ide{quote}, \ide{quasiquote}, \hbox{\ide{unquote}}, and
\ide{unquote-splicing}, respectively.  The second element in each case
is {{< hyper "datum" >}}.  This convention is supported so that arbitrary Scheme
programs can be represented as lists.
That is, according to Scheme's grammar, every
\meta{expression} is also a \meta{datum} (see section~\ref{datum}).
Among other things, this permits the use of the ``read`` procedure to
parse Scheme programs.  See section~\ref{externalreps}.


\begin{entry}{
\proto{pair?}{ obj}{procedure}}

The ``pair?`` predicate returns {{< tt "#t" >}} if _obj_ is a pair, and otherwise
returns \schfalse.

```
(pair? '(a . b))        \ev  \schtrue
(pair? '(a b c))        \ev  \schtrue
(pair? '())             \ev  \schfalse
(pair? '#(a b))         \ev  \schfalse
```
\end{entry}


\begin{entry}{
\proto{cons}{ \vari{obj} \varii{obj}}{procedure}}

Returns a newly allocated pair whose car is \vari{obj} and whose cdr is
\varii{obj}.  The pair is guaranteed to be different (in the sense of
``eqv?``) from every existing object.

```
(cons 'a '())           \ev  (a)
(cons '(a) '(b c d))    \ev  ((a) b c d)
(cons "a" '(b c))       \ev  ("a" b c)
(cons 'a 3)             \ev  (a . 3)
(cons '(a b) 'c)        \ev  ((a b) . c)
```
\end{entry}


\begin{entry}{
\proto{car}{ pair}{procedure}}

Returns the contents of the car field of _pair_.  Note that it is an
error to take the car of the empty list.

```
(car '(a b c))          \ev  a
(car '((a) b c d))      \ev  (a)
(car '(1 . 2))          \ev  1
(car '())               \ev  \scherror
```

\end{entry}


\begin{entry}{
\proto{cdr}{ pair}{procedure}}

Returns the contents of the cdr field of _pair_.
Note that it is an error to take the cdr of the empty list.

```
(cdr '((a) b c d))      \ev  (b c d)
(cdr '(1 . 2))          \ev  2
(cdr '())               \ev  \scherror
```

\end{entry}


\begin{entry}{
\proto{set-car!}{ pair obj}{procedure}}

Stores _obj_ in the car field of _pair_.
```
(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(set-car! (f) 3)             \ev  \unspecified
(set-car! (g) 3)             \ev  \scherror
```

\end{entry}


\begin{entry}{
\proto{set-cdr!}{ pair obj}{procedure}}

Stores _obj_ in the cdr field of _pair_.
\end{entry}

\setbox0\hbox{\tt(cadr _pair_)}
\setbox1\hbox{procedure}


\begin{entry}{
\proto{caar}{ pair}{procedure}
\proto{cadr}{ pair}{procedure}
\proto{cdar}{ pair}{procedure}
\proto{cddr}{ pair}{procedure}}

These procedures are compositions of ``car`` and ``cdr`` as follows:

```
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
```

\end{entry}

\begin{entry}{
\proto{caaar}{ pair}{cxr library procedure}
\proto{caadr}{ pair}{cxr library procedure}
\pproto{\hbox to 1\wd0 {\hfil$\vdots$\hfil}}{\hbox to 1\wd1 {\hfil$\vdots$\hfil}}
\mainschindex{cadar}\mainschindex{caddr}
\mainschindex{cdaar}\mainschindex{cdadr}\mainschindex{cddar}\mainschindex{cdddr}
\mainschindex{caaaar}\mainschindex{caaadr}\mainschindex{caadar}\mainschindex{caaddr}
\mainschindex{cadaar}\mainschindex{cadadr}\mainschindex{caddar}\mainschindex{cadddr}
\mainschindex{cdaaar}\mainschindex{cdaadr}\mainschindex{cdadar}\mainschindex{cdaddr}
\mainschindex{cddaar}\mainschindex{cddadr}
\proto{cdddar}{ pair}{cxr library procedure}
\proto{cddddr}{ pair}{cxr library procedure}}

These twenty-four procedures are further compositions of ``car`` and ``cdr``
on the same principles.
For example, ``caddr`` could be defined by

```
(define caddr (lambda (x) (car (cdr (cdr x))))){\rm.}
```

Arbitrary compositions up to four deep are provided.

\end{entry}


\begin{entry}{
\proto{null?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is the empty list,
otherwise returns \schfalse.

\end{entry}

\begin{entry}{
\proto{list?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a list.  Otherwise, it returns {{< tt "#f" >}}.
By definition, all lists have finite length and are terminated by
the empty list.

```
        (list? '(a b c))     \ev  \schtrue
        (list? '())          \ev  \schtrue
        (list? '(a . b))     \ev  \schfalse
        (let ((x (list 'a)))
          (set-cdr! x x)
          (list? x))         \ev  \schfalse
```


\end{entry}

\begin{entry}{
\proto{make-list}{ k}{procedure}
\rproto{make-list}{ k fill}{procedure}}

Returns a newly allocated list of _k_ elements.  If a second
argument is given, then each element is initialized to _fill_.
Otherwise the initial contents of each element is unspecified.

```
(make-list 2 3)   \ev   (3 3)
```

\end{entry}



\begin{entry}{
\proto{list}{ _obj_ ...}{procedure}}

Returns a newly allocated list of its arguments.

```
(list 'a (+ 3 4) 'c)            \ev  (a 7 c)
(list)                          \ev  ()
```
\end{entry}


\begin{entry}{
\proto{length}{ list}{procedure}}

Returns the length of _list_.

```
(length '(a b c))               \ev  3
(length '(a (b) (c d e)))       \ev  3
(length '())                    \ev  0
```


\end{entry}


\begin{entry}{
\proto{append}{ list ...}{procedure}}

\domain{The last argument, if there is one, can be of any type.}
Returns a list consisting of the elements of the first _list_
followed by the elements of the other _list_s.
If there are no arguments, the empty list is returned.
If there is exactly one argument, it is returned.
Otherwise the resulting list is always newly allocated, except that it shares
structure with the last argument.
An improper list results if the last argument is not a
proper list.

```
(append '(x) '(y))              \ev  (x y)
(append '(a) '(b c d))          \ev  (a b c d)
(append '(a (b)) '((c)))        \ev  (a (b) (c))
```


```
(append '(a b) '(c . d))        \ev  (a b c . d)
(append '() 'a)                 \ev  a
```
\end{entry}


\begin{entry}{
\proto{reverse}{ list}{procedure}}

Returns a newly allocated list consisting of the elements of _list_
in reverse order.

```
(reverse '(a b c))              \ev  (c b a)
(reverse '(a (b c) d (e (f))))  \lev  ((e (f)) d (b c) a)
```
\end{entry}


\begin{entry}{
\proto{list-tail}{ list \vr{k}}{procedure}}

\domain{It is an error if _list_ has fewer than \vr{k} elements.}
Returns the sublist of _list_ obtained by omitting the first \vr{k}
elements.
The ``list-tail`` procedure could be defined by

```
(define list-tail
  (lambda (x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1)))))
```
\end{entry}


\begin{entry}{
\proto{list-ref}{ list \vr{k}}{procedure}}

\domain{The _list_ argument can be circular, but
it is an error if _list_ has fewer than \vr{k} elements.}
Returns the \vr{k}th element of _list_.  (This is the same
as the car of {\tt(list-tail _list_ \vr{k})}.)

```
(list-ref '(a b c d) 2)                 \ev  c
(list-ref '(a b c d)
          (exact (round 1.8))) \lev  c
```
\end{entry}

\begin{entry}{
\proto{list-set!}{ list k obj}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _list_.}
The ``list-set!`` procedure stores _obj_ in element \vr{k} of _list_.
```
(let ((ls (list 'one 'two 'five!)))
  (list-set! ls 2 'three)
  ls)      \lev  (one two three)

(list-set! '(0 1 2) 1 "oops")  \lev  \scherror  ; constant list
```
\end{entry}




\begin{entry}{
\proto{memq}{ obj list}{procedure}
\proto{memv}{ obj list}{procedure}
\proto{member}{ obj list}{procedure}
\rproto{member}{ obj list compare}{procedure}}

These procedures return the first sublist of _list_ whose car is
_obj_, where the sublists of _list_ are the non-empty lists
returned by {\tt (list-tail _list_ _k_)} for _k_ less
than the length of _list_.  If
_obj_ does not occur in _list_, then {{< tt "#f" >}} (not the empty list) is
returned.  The ``memq`` procedure uses ``eq?`` to compare _obj_ with the elements of
_list_, while ``memv`` uses ``eqv?`` and
``member`` uses _compare_, if given, and ``equal?`` otherwise.

```
(memq 'a '(a b c))              \ev  (a b c)
(memq 'b '(a b c))              \ev  (b c)
(memq 'a '(b c d))              \ev  \schfalse
(memq (list 'a) '(b (a) c))     \ev  \schfalse
(member (list 'a)
        '(b (a) c))             \ev  ((a) c)
(member "B"
        '("a" "b" "c")
        string-ci=?)            \ev  ("b" "c")
(memq 101 '(100 101 102))       \ev  \unspecified
(memv 101 '(100 101 102))       \ev  (101 102)
```

\end{entry}


\begin{entry}{
\proto{assq}{ obj alist}{procedure}
\proto{assv}{ obj alist}{procedure}
\proto{assoc}{ obj alist}{procedure}
\rproto{assoc}{ obj alist compare}{procedure}}

\domain{It is an error if _alist_ (for "association list") is not a list of
pairs.}
These procedures find the first pair in _alist_ whose car field is _obj_,
and returns that pair.  If no pair in _alist_ has _obj_ as its
car, then {{< tt "#f" >}} (not the empty list) is returned.  The ``assq`` procedure uses
``eq?`` to compare _obj_ with the car fields of the pairs in _alist_,
while ``assv`` uses ``eqv?`` and ``assoc`` uses _compare_ if given
and ``equal?`` otherwise.

```
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)     \ev  (a 1)
(assq 'b e)     \ev  (b 2)
(assq 'd e)     \ev  \schfalse
(assq (list 'a) '(((a)) ((b)) ((c))))
                \ev  \schfalse
(assoc (list 'a) '(((a)) ((b)) ((c))))
                           \ev  ((a))
(assoc 2.0 '((1 1) (2 4) (3 9)) =)
                           \ev (2 4)
(assq 5 '((2 3) (5 7) (11 13)))
                           \ev  \unspecified
(assv 5 '((2 3) (5 7) (11 13)))
                           \ev  (5 7)
```


\begin{rationale}
Although they are often used as predicates,
``memq``, ``memv``, ``member``, ``assq``, ``assv``, and ``assoc`` do not
have question marks in their names because they return
potentially useful values rather than just {{< tt "#t" >}} or {{< tt "#f" >}}.
\end{rationale}
\end{entry}

\begin{entry}{
\proto{list-copy}{ obj}{procedure}}

Returns a newly allocated copy of the given _obj_ if it is a list.
Only the pairs themselves are copied; the cars of the result are
the same (in the sense of ``eqv?``) as the cars of _list_.
If _obj_ is an improper list, so is the result, and the final
cdrs are the same in the sense of ``eqv?``.
An _obj_ which is not a list is returned unchanged.
It is an error if _obj_ is a circular list.

```
(define a '(1 8 2 8)) ; a may be immutable
(define b (list-copy a))
(set-car! b 3)        ; b is mutable
b \ev (3 8 2 8)
a \ev (1 8 2 8)
```

\end{entry}


## 6.5. Symbols
{{< label "symbolsection" >}}

Symbols are objects whose usefulness rests on the fact that two
symbols are identical (in the sense of ``eqv?``) if and only if their
names are spelled the same way.  For instance, they can be used
the way enumerated values are used in other languages.

The rules for writing a symbol are exactly the same as the rules for
writing an identifier; see sections~\ref{syntaxsection}
and~\ref{identifiersyntax}.

It is guaranteed that any symbol that has been returned as part of
a literal expression, or read using the ``read`` procedure, and
subsequently written out using the ``write`` procedure, will read back
in as the identical symbol (in the sense of ``eqv?``).

*Note:&nbsp;*
Some implementations have values known as "uninterned symbols,"
which defeat write/read invariance, and also violate the rule that two
symbols are the same if and only if their names are spelled the same.
This report does not specify the behavior of
implementation-dependent extensions.



\begin{entry}{
\proto{symbol?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a symbol, otherwise returns \schfalse.

```
(symbol? 'foo)          \ev  \schtrue
(symbol? (car '(a b)))  \ev  \schtrue
(symbol? "bar")         \ev  \schfalse
(symbol? 'nil)          \ev  \schtrue
(symbol? '())           \ev  \schfalse
(symbol? \schfalse)     \ev  \schfalse
```
\end{entry}

\begin{entry}{
\proto{symbol=?}{ \vari{symbol} \varii{symbol} \variii{symbol} ...}{procedure}}

Returns {{< tt "#t" >}} if all the arguments are symbols and all have the same
names in the sense of ``string=?``.

*Note:&nbsp;*
The definition above assumes that none of the arguments
are uninterned symbols.


\end{entry}

\begin{entry}{
\proto{symbol->string}{ symbol}{procedure}}

Returns the name of _symbol_ as a string, but without adding escapes.
It is an error
to apply mutation procedures like \ide{string-set!} to strings returned
by this procedure.

```
(symbol->string 'flying-fish)
                                  \ev  "flying-fish"
(symbol->string 'Martin)          \ev  "Martin"
(symbol->string
   (string->symbol "Malvina"))
                                  \ev  "Malvina"
```
\end{entry}


\begin{entry}{
\proto{string->symbol}{ string}{procedure}}

Returns the symbol whose name is _string_.  This procedure can
create symbols with names containing special characters that would
require escaping when written, but does not interpret escapes in its input.

```
(string->symbol "mISSISSIppi")  \lev
  mISSISSIppi
(eqv? 'bitBlt (string->symbol "bitBlt"))     \lev  \schtrue
(eqv? 'LollyPop
     (string->symbol
       (symbol->string 'LollyPop)))  \lev  \schtrue
(string=? "K. Harper, M.D."
          (symbol->string
            (string->symbol "K. Harper, M.D.")))  \lev  \schtrue
```

\end{entry}


## 6.6. Characters
{{< label "charactersection" >}}

Characters are objects that represent printed characters such as
letters and digits.
All Scheme implementations must support at least the ASCII character
repertoire: that is, Unicode characters U+0000 through U+007F.
Implementations may support any other Unicode characters they see fit,
and may also support non-Unicode characters as well.
Except as otherwise specified, the result of applying any of the
following procedures to a non-Unicode character is implementation-dependent.

Characters are written using the notation \sharpsign\backwhack{{< hyper "character" >}}
or \sharpsign\backwhack{{< hyper "character name" >}} or
\sharpsign\backwhack{}x\meta{hex scalar value}.

The following character names must be supported
by all implementations with the given values.
Implementations may add other names
provided they cannot be interpreted as hex scalar values preceded by ``x``.

$$
\begin{tabular}{ll}
{\tt #\backwhack{}alarm}&; \textrm{U+0007}\\
{\tt #\backwhack{}backspace}&; \textrm{U+0008}\\
{\tt #\backwhack{}delete}&; \textrm{U+007F}\\
{\tt #\backwhack{}escape}&; \textrm{U+001B}\\
{\tt #\backwhack{}newline}&; the linefeed character, \textrm{U+000A}\\
{\tt #\backwhack{}null}&; the null character, \textrm{U+0000}\\
{\tt #\backwhack{}return}&; the return character, \textrm{U+000D}\\
{\tt #\backwhack{}space}&; the preferred way to write a space\\
{\tt #\backwhack{}tab}&; the tab character, \textrm{U+0009}\\
\end{tabular}
$$

Here are some additional examples:

$$
\begin{tabular}{ll}
{\tt #\backwhack{}a}&; lower case letter\\
{\tt #\backwhack{}A}&; upper case letter\\
{\tt #\backwhack{}(}&; left parenthesis\\
{\tt #\backwhack{} }&; the space character\\
{\tt #\backwhack{}x03BB}&; $\lambda$ (if character is supported)\\
{\tt #\backwhack{}iota}&; $\iota$ (if character and name are supported)\\
\end{tabular}
$$

Case is significant in \sharpsign\backwhack{{< hyper "character" >}}, and in
\sharpsign\backwhack{\rm$\langle$character name$\rangle$},
but not in {\cf\sharpsign\backwhack{}x}\meta{hex scalar value}.
If {{< hyper "character" >}} in
\sharpsign\backwhack{{< hyper "character" >}} is alphabetic, then any character
immediately following {{< hyper "character" >}} cannot be one that can appear in an identifier.
This rule resolves the ambiguous case where, for
example, the sequence of characters "{\tt\sharpsign\backwhack space}"
could be taken to be either a representation of the space character or a
representation of the character "{\tt\sharpsign\backwhack s}" followed
by a representation of the symbol "{\tt pace}."


Characters written in the \sharpsign\backwhack{} notation are self-evaluating.
That is, they do not have to be quoted in programs.

Some of the procedures that operate on characters ignore the
difference between upper case and lower case.  The procedures that
ignore case have \hbox{"{\tt -ci}"} (for "case
insensitive") embedded in their names.


\begin{entry}{
\proto{char?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a character, otherwise returns \schfalse.

\end{entry}


\begin{entry}{
\proto{char=?}{ \vri{char} \vrii{char} \vriii{char} ...}{procedure}
\proto{char<?}{ \vri{char} \vrii{char} \vriii{char} ...}{procedure}
\proto{char>?}{ \vri{char} \vrii{char} \vriii{char} ...}{procedure}
\proto{char<=?}{ \vri{char} \vrii{char} \vriii{char} ...}{procedure}
\proto{char>=?}{ \vri{char} \vrii{char} \vriii{char} ...}{procedure}}

{{< label "characterequality" >}}

These procedures return {{< tt "#t" >}} if
the results of passing their arguments to ``char\coerce{``integer}
are respectively
equal, monotonically increasing, monotonically decreasing,
monotonically non-decreasing, or monotonically non-increasing.

These predicates are required to be transitive.

\end{entry}


\begin{entry}{
\proto{char-ci=?}{ \vri{char} \vrii{char} \vriii{char} ...}{char library procedure}
\proto{char-ci<?}{ \vri{char} \vrii{char} \vriii{char} ...}{char library procedure}
\proto{char-ci>?}{ \vri{char} \vrii{char} \vriii{char} ...}{char library procedure}
\proto{char-ci<=?}{ \vri{char} \vrii{char} \vriii{char} ...}{char library procedure}
\proto{char-ci>=?}{ \vri{char} \vrii{char} \vriii{char} ...}{char library procedure}}

These procedures are similar to ``char=?`` et cetera, but they treat
upper case and lower case letters as the same.  For example, {\cf
(char-ci=? #\backwhack{}A #\backwhack{}a)} returns \schtrue.

Specifically, these procedures behave as if ``char-foldcase`` were
applied to their arguments before they were compared.

\end{entry}


\begin{entry}{
\proto{char-alphabetic?}{ char}{char library procedure}
\proto{char-numeric?}{ char}{char library procedure}
\proto{char-whitespace?}{ char}{char library procedure}
\proto{char-upper-case?}{ letter}{char library procedure}
\proto{char-lower-case?}{ letter}{char library procedure}}

These procedures return {{< tt "#t" >}} if their arguments are alphabetic,
numeric, whitespace, upper case, or lower case characters, respectively,
otherwise they return \schfalse.

Specifically, they must return {{< tt "#t" >}} when applied to characters with
the Unicode properties Alphabetic, Numeric_Digit, White_Space, Uppercase, and
Lowercase respectively, and {{< tt "#f" >}} when applied to any other Unicode
characters.  Note that many Unicode characters are alphabetic but neither
upper nor lower case.

\end{entry}


\begin{entry}{
\proto{digit-value}{ char}{char library procedure}}

This procedure returns the numeric value (0 to 9) of its argument
if it is a numeric digit (that is, if ``char-numeric?`` returns {{< tt "#t" >}}),
or {{< tt "#f" >}} on any other character.

```
(digit-value #\3) \ev 3
(digit-value #\x0664) \ev 4
(digit-value #\x0AE6) \ev 0
(digit-value #\x0EA6) \ev \schfalse
```
\end{entry}


\begin{entry}{
\proto{char->integer}{ char}{procedure}
\proto{integer->char}{ \vr{n}}{procedure}}

Given a Unicode character,
``char\coerce{``integer} returns an exact integer
between 0 and {\tt #xD7FF} or
between {\tt #xE000} and {\tt #x10FFFF}
which is equal to the Unicode scalar value of that character.
Given a non-Unicode character,
it returns an exact integer greater than {\tt #x10FFFF}.
This is true independent of whether the implementation uses
the Unicode representation internally.

Given an exact integer that is the value returned by
a character when ``char\coerce{``integer} is applied to it, ``integer\coerce{``char}
returns that character.
\end{entry}


\begin{entry}{
\proto{char-upcase}{ char}{char library procedure}
\proto{char-downcase}{ char}{char library procedure}
\proto{char-foldcase}{ char}{char library procedure}}


The ``char-upcase`` procedure, given an argument that is the
lowercase part of a Unicode casing pair, returns the uppercase member
of the pair, provided that both characters are supported by the Scheme
implementation.  Note that language-sensitive casing pairs are not used.  If the
argument is not the lowercase member of such a pair, it is returned.

The ``char-downcase`` procedure, given an argument that is the
uppercase part of a Unicode casing pair, returns the lowercase member
of the pair, provided that both characters are supported by the Scheme
implementation.  Note that language-sensitive casing pairs are not used.  If the
argument is not the uppercase member of such a pair, it is returned.

The ``char-foldcase`` procedure applies the Unicode simple
case-folding algorithm to its argument and returns the result.  Note that
language-sensitive folding is not used.  If the argument is an uppercase
letter, the result will be either a lowercase letter
or the same as the argument if the lowercase letter does not exist or
is not supported by the implementation.
See UAX #29 {{< cite "uax29" >}} (part of the Unicode Standard) for details.

Note that many Unicode lowercase characters do not have uppercase
equivalents.

\end{entry}


## 6.7. Strings
{{< label "stringsection" >}}

Strings are sequences of characters.
Strings are written as sequences of characters enclosed within quotation marks
(``"``).  Within a string literal, various escape
sequences represent characters other than
themselves.  Escape sequences always start with a backslash (\backwhack{}):


\item{\cf\backwhack{}a} : alarm, U+0007
\item{\cf\backwhack{}b} : backspace, U+0008
\item{\cf\backwhack{}t} : character tabulation, U+0009
\item{\cf\backwhack{}n} : linefeed, U+000A
\item{\cf\backwhack{}r} : return, U+000D
\item{\cf\backwhack{}}\verb|"| : double quote, U+0022
\item{\cf\backwhack{}\backwhack{}} : backslash, U+005C
\item{\cf\backwhack{}|} : vertical line, U+007C
\item{\cf\backwhack{}\arbno{{{< hyper "intraline whitespace" >}}}{{< hyper "line ending" >}}
      \arbno{{{< hyper "intraline whitespace" >}}}} : nothing
\item{\cf\backwhack{}x\meta{hex scalar value};} : specified character (note the
  terminating semi-colon).


The result is unspecified if any other character in a string occurs
after a backslash.

Except for a line ending, any character outside of an escape
sequence stands for itself in the string literal.  A line ending which
is preceded by {\cf\backwhack{}{{< hyper "intraline whitespace" >}}} expands
to nothing (along with any trailing intraline whitespace), and can be
used to indent strings for improved legibility. Any other line ending
has the same effect as inserting a {\cf\backwhack{}n} character into
the string.

Examples:

```
"The word \"recursion\" has many meanings."
"Another example:\ntwo lines of text"
"Here's text \
   containing just one line"
"\x03B1; is named GREEK SMALL LETTER ALPHA."
```

The _length_ of a string is the number of characters that it
contains.  This number is an exact, non-negative integer that is fixed when the
string is created.  The \defining{valid indexes} of a string are the
exact non-negative integers less than the length of the string.  The first
character of a string has index 0, the second has index 1, and so on.


Some of the procedures that operate on strings ignore the
difference between upper and lower case.  The names of the versions that ignore case
end with \hbox{"``-ci``"} (for "case insensitive").

Implementations may forbid certain characters from appearing in strings.
However, with the exception of {\tt #\backwhack{}null}, ASCII characters must
not be forbidden.
For example, an implementation might support the entire Unicode repertoire,
but only allow characters U+0001 to U+00FF (the Latin-1 repertoire
without {\tt #\backwhack{}null}) in strings.

It is an error to pass such a forbidden character to
``make-string``, ``string``, ``string-set!``, or ``string-fill!``,
as part of the list passed to ``list\coerce{``string},
or as part of the vector passed to ``vector\coerce{``string}
(see section~\ref{vectortostring}),
or in UTF-8 encoded form within a bytevector passed to
``utf8\coerce{``string} (see section~\ref{utf8tostring}).
It is also an error for a procedure passed to ``string-map``
(see section~\ref{stringmap}) to return a forbidden character,
or for ``read-string`` (see section~\ref{readstring})
to attempt to read one.

\begin{entry}{
\proto{string?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a string, otherwise returns \schfalse.
\end{entry}


\begin{entry}{
\proto{make-string}{ \vr{k}}{procedure}
\rproto{make-string}{ \vr{k} char}{procedure}}

The ``make-string`` procedure returns a newly allocated string of
length \vr{k}.  If _char_ is given, then all the characters of the string
are initialized to _char_, otherwise the contents of the
string are unspecified.

\end{entry}

\begin{entry}{
\proto{string}{ char ...}{procedure}}

Returns a newly allocated string composed of the arguments.
It is analogous to ``list``.

\end{entry}

\begin{entry}{
\proto{string-length}{ string}{procedure}}

Returns the number of characters in the given _string_.
\end{entry}


\begin{entry}{
\proto{string-ref}{ string \vr{k}}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _string_.}
The ``string-ref`` procedure returns character \vr{k} of _string_ using zero-origin indexing.
\end{entry}
There is no requirement for this procedure to execute in constant time.


\begin{entry}{
\proto{string-set!}{ string k char}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _string_.}
The ``string-set!`` procedure stores _char_ in element \vr{k} of _string_.
There is no requirement for this procedure to execute in constant time.

```
(define (f) (make-string 3 #\*))
(define (g) "***")
(string-set! (f) 0 #\?)  \ev  \unspecified
(string-set! (g) 0 #\?)  \ev  \scherror
(string-set! (symbol->string 'immutable)
             0
             #\?)  \ev  \scherror
```

\end{entry}


\begin{entry}{
\proto{string=?}{ \vri{string} \vrii{string} \vriii{string} ...}{procedure}}

Returns {{< tt "#t" >}} if all the strings are the same length and contain
exactly the same characters in the same positions, otherwise returns
\schfalse.

\end{entry}

\begin{entry}{
\proto{string-ci=?}{ \vri{string} \vrii{string} \vriii{string} ...}{char library procedure}}

Returns {{< tt "#t" >}} if, after case-folding, all the strings are the same
length and contain the same characters in the same positions, otherwise
returns \schfalse.  Specifically, these procedures behave as if
``string-foldcase`` were applied to their arguments before comparing them.

\end{entry}


\begin{entry}{
\proto{string<?}{ \vri{string} \vrii{string} \vriii{string} ...}{procedure}
\proto{string-ci<?}{ \vri{string} \vrii{string} \vriii{string} ...}{char library procedure}
\proto{string>?}{ \vri{string} \vrii{string} \vriii{string} ...}{procedure}
\proto{string-ci>?}{ \vri{string} \vrii{string} \vriii{string} ...}{char library procedure}
\proto{string<=?}{ \vri{string} \vrii{string} \vriii{string} ...}{procedure}
\proto{string-ci<=?}{ \vri{string} \vrii{string} \vriii{string} ...}{char library procedure}
\proto{string>=?}{ \vri{string} \vrii{string} \vriii{string} ...}{procedure}
\proto{string-ci>=?}{ \vri{string} \vrii{string} \vriii{string} ...}{char library procedure}}

These procedures return {{< tt "#t" >}} if their arguments are (respectively):
monotonically increasing, monotonically decreasing,
monotonically non-decreasing, or monotonically non-increasing.

These predicates are required to be transitive.

These procedures compare strings in an implementation-defined way.
One approach is to make them the lexicographic extensions to strings of
the corresponding orderings on characters.  In that case, ``string<?``\
would be the lexicographic ordering on strings induced by the ordering
``char<?`` on characters, and if the two strings differ in length but
are the same up to the length of the shorter string, the shorter string
would be considered to be lexicographically less than the longer string.
However, it is also permitted to use the natural ordering imposed by the
implementation's internal representation of strings, or a more complex locale-specific
ordering.

In all cases, a pair of strings must satisfy exactly one of
``string<?``, ``string=?``, and ``string>?``, and must satisfy
``string<=?`` if and only if they do not satisfy ``string>?`` and
``string>=?`` if and only if they do not satisfy ``string<?``.

The \hbox{"{\tt -ci}"} procedures behave as if they applied
``string-foldcase`` to their arguments before invoking the corresponding
procedures without  \hbox{"{\tt -ci}"}.


\end{entry}

\begin{entry}{
\proto{string-upcase}{ string}{char library procedure}
\proto{string-downcase}{ string}{char library procedure}
\proto{string-foldcase}{ string}{char library procedure}}


These procedures apply the Unicode full string uppercasing, lowercasing,
and case-folding algorithms to their arguments and return the result.
In certain cases, the result differs in length from the argument.
If the result is equal to the argument in the sense of ``string=?``, the argument may be returned.
Note that language-sensitive mappings and foldings are not used.

The Unicode Standard prescribes special treatment of the Greek letter
$\Sigma$, whose normal lower-case form is $\sigma$ but which becomes
$\varsigma$ at the end of a word.  See UAX #29 {{< cite "uax29" >}} (part of
the Unicode Standard) for details.  However, implementations of {\cf
string-downcase} are not required to provide this behavior, and may
choose to change $\Sigma$ to $\sigma$ in all cases.

\end{entry}


\begin{entry}{
\proto{substring}{ string start end}{procedure}}

The ``substring`` procedure returns a newly allocated string formed from the characters of
_string_ beginning with index _start_ and ending with index
_end_.
This is equivalent to calling ``string-copy`` with the same arguments,
but is provided for backward compatibility and
stylistic flexibility.
\end{entry}


\begin{entry}{
\proto{string-append}{ _string_ ...}{procedure}}

Returns a newly allocated string whose characters are the concatenation of the
characters in the given strings.

\end{entry}


\begin{entry}{
\proto{string->list}{ string}{procedure}
\rproto{string->list}{ string start}{procedure}
\rproto{string->list}{ string start end}{procedure}
\proto{list->string}{ list}{procedure}}

\domain{It is an error if any element of _list_ is not a character.}
The ``string\coerce{``list} procedure returns a newly allocated list of the
characters of _string_ between _start_ and _end_.
``list\coerce{``string}
returns a newly allocated string formed from the elements in the list
_list_.
In both procedures, order is preserved.
``string\coerce{``list}
and ``list\coerce{``string} are
inverses so far as ``equal?`` is concerned.

\end{entry}


\begin{entry}{
\proto{string-copy}{ string}{procedure}
\rproto{string-copy}{ string start}{procedure}
\rproto{string-copy}{ string start end}{procedure}}

Returns a newly allocated copy of the part of the given _string_
between _start_ and _end_.

\end{entry}


\begin{entry}{
\proto{string-copy!}{ to at from}{procedure}
\rproto{string-copy!}{ to at from start}{procedure}
\rproto{string-copy!}{ to at from start end}{procedure}}

\domain{It is an error if _at_ is less than zero or greater than the length of _to_.
It is also an error if ``(- (string-length _to``) _at_)_
is less than ``(- _end`` _start_)_.}
Copies the characters of string _from_ between _start_ and _end_
to string _to_, starting at _at_.  The order in which characters are
copied is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
string and then into the destination.  This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

```
(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 1 a 0 2)
b \ev "a12de"
```

\end{entry}


\begin{entry}{
\proto{string-fill!}{ string fill}{procedure}
\rproto{string-fill!}{ string fill start}{procedure}
\rproto{string-fill!}{ string fill start end}{procedure}}

\domain{It is an error if _fill_ is not a character.}

The ``string-fill!`` procedure stores _fill_
in the elements of _string_
between _start_ and _end_.

\end{entry}


## 6.8. Vectors
{{< label "vectorsection" >}}

Vectors are heterogeneous structures whose elements are indexed
by integers.  A vector typically occupies less space than a list
of the same length, and the average time needed to access a randomly
chosen element is typically less for the vector than for the list.

The _length_ of a vector is the number of elements that it
contains.  This number is a non-negative integer that is fixed when the
vector is created.  The _valid indexes_ of a
vector are the exact non-negative integers less than the length of the
vector.  The first element in a vector is indexed by zero, and the last
element is indexed by one less than the length of the vector.

Vectors are written using the notation {\tt#(_obj_ ...)}.
For example, a vector of length 3 containing the number zero in element
0, the list {\cf(2 2 2 2)} in element 1, and the string ``"Anna"`` in
element 2 can be written as follows:

```
#(0 (2 2 2 2) "Anna")
```

Vector constants are self-evaluating, so they do not need to be quoted in programs.

\begin{entry}{
\proto{vector?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a vector; otherwise returns \schfalse.
\end{entry}


\begin{entry}{
\proto{make-vector}{ k}{procedure}
\rproto{make-vector}{ k fill}{procedure}}

Returns a newly allocated vector of _k_ elements.  If a second
argument is given, then each element is initialized to _fill_.
Otherwise the initial contents of each element is unspecified.

\end{entry}


\begin{entry}{
\proto{vector}{ obj ...}{procedure}}

Returns a newly allocated vector whose elements contain the given
arguments.  It is analogous to ``list``.

```
(vector 'a 'b 'c)               \ev  #(a b c)
```
\end{entry}


\begin{entry}{
\proto{vector-length}{ vector}{procedure}}

Returns the number of elements in _vector_ as an exact integer.
\end{entry}


\begin{entry}{
\proto{vector-ref}{ vector k}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _vector_.}
The ``vector-ref`` procedure returns the contents of element \vr{k} of
_vector_.

```
(vector-ref '#(1 1 2 3 5 8 13 21)
            5)  \lev  8
(vector-ref '#(1 1 2 3 5 8 13 21)
            (exact
             (round (* 2 (acos -1))))) \lev 13
```
\end{entry}


\begin{entry}{
\proto{vector-set!}{ vector k obj}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _vector_.}
The ``vector-set!`` procedure stores _obj_ in element \vr{k} of _vector_.

```
(let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec)      \lev  #(0 ("Sue" "Sue") "Anna")

(vector-set! '#(0 1 2) 1 "doe")  \lev  \scherror  ; constant vector
```
\end{entry}


\begin{entry}{
\proto{vector->list}{ vector}{procedure}
\rproto{vector->list}{ vector start}{procedure}
\rproto{vector->list}{ vector start end}{procedure}
\proto{list->vector}{ list}{procedure}}

The ``vector->list`` procedure returns a newly allocated list of the objects contained
in the elements of _vector_ between _start_ and _end_.
The ``list->vector`` procedure returns a newly
created vector initialized to the elements of the list _list_.

In both procedures, order is preserved.

```
(vector->list '#(dah dah didah))  \lev  (dah dah didah)
(vector->list '#(dah dah didah) 1 2) \lev (dah)
(list->vector '(dididit dah))   \lev  #(dididit dah)
```
\end{entry}

\begin{entry}{
\proto{vector->string}{ vector}{procedure}
\rproto{vector->string}{ vector start}{procedure}
\rproto{vector->string}{ vector start end}{procedure}
\proto{string->vector}{ string}{procedure}
\rproto{string->vector}{ string start}{procedure}
\rproto{string->vector}{ string start end}{procedure}}
{{< label "vectortostring" >}}

\domain{It is an error if any element of _vector_ between _start_
and _end_ is not a character.}
The ``vector->string`` procedure returns a newly allocated string of the objects contained
in the elements of _vector_
between _start_ and _end_.
The ``string->vector`` procedure returns a newly
created vector initialized to the elements of the string _string_
between _start_ and _end_.

In both procedures, order is preserved.


```
(string->vector "ABC")  \ev   #(#\A #\B #\C)
(vector->string
  #(#\1 #\2 #\3) \ev "123"
```
\end{entry}

\begin{entry}{
\proto{vector-copy}{ vector}{procedure}
\rproto{vector-copy}{ vector start}{procedure}
\rproto{vector-copy}{ vector start end}{procedure}}

Returns a newly allocated copy of the elements of the given _vector_
between _start_ and _end_.
The elements of the new vector are the same (in the sense of
``eqv?``) as the elements of the old.


```
(define a #(1 8 2 8)) ; a may be immutable
(define b (vector-copy a))
(vector-set! b 0 3)   ; b is mutable
b \ev #(3 8 2 8)
(define c (vector-copy b 1 3))
c \ev #(8 2)
```

\end{entry}

\begin{entry}{
\proto{vector-copy!}{ to at from}{procedure}
\rproto{vector-copy!}{ to at from start}{procedure}
\rproto{vector-copy!}{ to at from start end}{procedure}}

\domain{It is an error if _at_ is less than zero or greater than the length of _to_.
It is also an error if ``(- (vector-length _to``) _at_)_
is less than ``(- _end`` _start_)_.}
Copies the elements of vector _from_ between _start_ and _end_
to vector _to_, starting at _at_.  The order in which elements are
copied is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
vector and then into the destination.  This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

```
(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
b \ev #(10 1 2 40 50)
```

\end{entry}

\begin{entry}{
\proto{vector-append}{ _vector_ ...}{procedure}}

Returns a newly allocated vector whose elements are the concatenation
of the elements of the given vectors.

```
(vector-append #(a b c) #(d e f)) \lev #(a b c d e f)
```

\end{entry}

\begin{entry}{
\proto{vector-fill!}{ vector fill}{procedure}
\rproto{vector-fill!}{ vector fill start}{procedure}
\rproto{vector-fill!}{ vector fill start end}{procedure}}

The ``vector-fill!`` procedure stores _fill_
in the elements of _vector_
between _start_ and _end_.

```
(define a (vector 1 2 3 4 5))
(vector-fill! a 'smash 2 4)
a \lev #(1 2 smash smash 5)
```

\end{entry}


## 6.9. Bytevectors
{{< label "bytevectorsection" >}}

\defining{Bytevectors} represent blocks of binary data.
They are fixed-length sequences of bytes, where
a \defining{byte} is an exact integer in the range from 0 to 255 inclusive.
A bytevector is typically more space-efficient than a vector
containing the same values.

The _length_ of a bytevector is the number of elements that it
contains.  This number is a non-negative integer that is fixed when
the bytevector is created.  The _valid indexes_ of
a bytevector are the exact non-negative integers less than the length of the
bytevector, starting at index zero as with vectors.

Bytevectors are written using the notation {\tt#u8(_byte_ ...)}.
For example, a bytevector of length 3 containing the byte 0 in element
0, the byte 10 in element 1, and the byte 5 in
element 2 can be written as follows:

```
#u8(0 10 5)
```

Bytevector constants are self-evaluating, so they do not need to be quoted in programs.


\begin{entry}{
\proto{bytevector?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a bytevector.
Otherwise, {{< tt "#f" >}} is returned.
\end{entry}

\begin{entry}{
\proto{make-bytevector}{ k}{procedure}
\rproto{make-bytevector}{ k byte}{procedure}}

The ``make-bytevector`` procedure returns a newly allocated bytevector of
length \vr{k}.  If _byte_ is given, then all elements of the bytevector
are initialized to _byte_, otherwise the contents of each
element are unspecified.

```
(make-bytevector 2 12) \ev #u8(12 12)
```

\end{entry}

\begin{entry}{
\proto{bytevector}{ _byte_ ...}{procedure}}

Returns a newly allocated bytevector containing its arguments.

```
(bytevector 1 3 5 1 3 5)        \ev  #u8(1 3 5 1 3 5)
(bytevector)                          \ev  #u8()
```
\end{entry}

\begin{entry}{
\proto{bytevector-length}{ bytevector}{procedure}}

Returns the length of _bytevector_ in bytes as an exact integer.
\end{entry}

\begin{entry}{
\proto{bytevector-u8-ref}{ bytevector k}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _bytevector_.}
Returns the _k_th byte of _bytevector_.

```
(bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21)
            5)  \lev  8
```
\end{entry}

\begin{entry}{
\proto{bytevector-u8-set!}{ bytevector k byte}{procedure}}

\domain{It is an error if \vr{k} is not a valid index of _bytevector_.}
Stores _byte_ as the _k_th byte of _bytevector_.

```
(let ((bv (bytevector 1 2 3 4)))
  (bytevector-u8-set! bv 1 3)
  bv) \lev #u8(1 3 3 4)
```
\end{entry}

\begin{entry}{
\proto{bytevector-copy}{ bytevector}{procedure}
\rproto{bytevector-copy}{ bytevector start}{procedure}
\rproto{bytevector-copy}{ bytevector start end}{procedure}}

Returns a newly allocated bytevector containing the bytes in _bytevector_
between _start_ and _end_.

```
(define a #u8(1 2 3 4 5))
(bytevector-copy a 2 4)) \ev #u8(3 4)
```

\end{entry}

\begin{entry}{
\proto{bytevector-copy!}{ to at from}{procedure}
\rproto{bytevector-copy!}{ to at from start}{procedure}
\rproto{bytevector-copy!}{ to at from start end}{procedure}}

\domain{It is an error if _at_ is less than zero or greater than the length of _to_.
It is also an error if ``(- (bytevector-length _to``) _at_)_
is less than ``(- _end`` _start_)_.}
Copies the bytes of bytevector _from_ between _start_ and _end_
to bytevector _to_, starting at _at_.  The order in which bytes are
copied is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
bytevector and then into the destination.  This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

```
(define a (bytevector 1 2 3 4 5))
(define b (bytevector 10 20 30 40 50))
(bytevector-copy! b 1 a 0 2)
b \ev #u8(10 1 2 40 50)
```

*Note:&nbsp;*
This procedure appears in {{< rnrs 6 >}}, but places the source before the destination,
contrary to other such procedures in Scheme.


\end{entry}

\begin{entry}{
\proto{bytevector-append}{ _bytevector_ ...}{procedure}}

Returns a newly allocated bytevector whose elements are the concatenation
of the elements in the given bytevectors.

```
(bytevector-append #u8(0 1 2) #u8(3 4 5)) \lev #u8(0 1 2 3 4 5)
```

\end{entry}

{{< label "utf8tostring" >}}
\begin{entry}{
\proto{utf8->string}{ bytevector} {procedure}
\rproto{utf8->string}{ bytevector start} {procedure}
\rproto{utf8->string}{ bytevector start end} {procedure}
\proto{string->utf8}{ string} {procedure}
\rproto{string->utf8}{ string start} {procedure}
\rproto{string->utf8}{ string start end} {procedure}}

\domain{It is an error for _bytevector_ to contain invalid UTF-8 byte sequences.}
These procedures translate between strings and bytevectors
that encode those strings using the UTF-8 encoding.
The ``utf8\coerce{``string} procedure decodes the bytes of
a bytevector between _start_ and _end_
and returns the corresponding string;
the ``string\coerce{``utf8} procedure encodes the characters of a
string between _start_ and _end_
and returns the corresponding bytevector.

```
(utf8->string #u8(#x41)) \ev "A"
(string->utf8 "$\lambda$") \ev #u8(#xCE #xBB)
```

\end{entry}

## 6.10. Control features
{{< label "proceduresection" >}}

This section describes various primitive procedures which control the
flow of program execution in special ways.
Procedures in this section that invoke procedure arguments
always do so in the same dynamic environment as the call of the
original procedure.
The ``procedure?`` predicate is also described here.

\begin{entry}{
\proto{procedure?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is a procedure, otherwise returns \schfalse.

```
(procedure? car)            \ev  \schtrue
(procedure? 'car)           \ev  \schfalse
(procedure? (lambda (x) (* x x)))
                            \ev  \schtrue
(procedure? '(lambda (x) (* x x)))
                            \ev  \schfalse
(call-with-current-continuation procedure?)
                            \ev  \schtrue
```

\end{entry}


\begin{entry}{
\proto{apply}{ proc \vari{arg} $\ldots$ args}{procedure}}

The ``apply`` procedure calls _proc_ with the elements of the list
{\cf(append (list \vari{arg} ...) _args_)} as the actual
arguments.

```
(apply + (list 3 4))              \ev  7

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

((compose sqrt *) 12 75)              \ev  30
```
\end{entry}


\begin{entry}{
\proto{map}{ proc \vari{list} \varii{list} ...}{procedure}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it list}s
and return a single value.}
The ``map`` procedure applies _proc_ element-wise to the elements of the
_list_s and returns a list of the results, in order.
If more than one _list_ is given and not all lists have the same length,
``map`` terminates when the shortest list runs out.
The _list_s can be circular, but it is an error if all of them are circular.
It is an error for _proc_ to mutate any of the lists.
The dynamic order in which _proc_ is applied to the elements of the
_list_s is unspecified.  If multiple returns occur from ``map``,
the values returned by earlier returns are not mutated.

```
(map cadr '((a b) (d e) (g h)))   \lev  (b e h)

(map (lambda (n) (expt n n))
     '(1 2 3 4 5))                \lev  (1 4 27 256 3125)

(map + '(1 2 3) '(4 5 6 7))         \ev  (5 7 9)

(let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1))
         count)
       '(a b)))                 \ev  (1 2) _or_ (2 1)
```

\end{entry}

\begin{entry}{
\proto{string-map}{ proc \vari{string} \varii{string} ...}{procedure}}
{{< label "stringmap" >}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it string}s
and return a single character.}
The ``string-map`` procedure applies _proc_ element-wise to the elements of the
_string_s and returns a string of the results, in order.
If more than one _string_ is given and not all strings have the same length,
``string-map`` terminates when the shortest string runs out.
The dynamic order in which _proc_ is applied to the elements of the
_string_s is unspecified.
If multiple returns occur from ``string-map``,
the values returned by earlier returns are not mutated.

```
(string-map char-foldcase "AbdEgH") \lev  "abdegh"

(string-map
 (lambda (c)
   (integer->char (+ 1 (char->integer c))))
 "HAL")                \lev  "IBM"

(string-map
 (lambda (c k)
   ((if (eqv? k \sharpsign\backwhack{}u) char-upcase char-downcase)
    c))
 "studlycaps xxx"
 "ululululul")   \lev   "StUdLyCaPs"
```

\end{entry}

\begin{entry}{
\proto{vector-map}{ proc \vari{vector} \varii{vector} ...}{procedure}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it vector}s
and return a single value.}
The ``vector-map`` procedure applies _proc_ element-wise to the elements of the
_vector_s and returns a vector of the results, in order.
If more than one _vector_ is given and not all vectors have the same length,
``vector-map`` terminates when the shortest vector runs out.
The dynamic order in which _proc_ is applied to the elements of the
_vector_s is unspecified.
If multiple returns occur from ``vector-map``,
the values returned by earlier returns are not mutated.

```
(vector-map cadr '#((a b) (d e) (g h)))   \lev  #(b e h)

(vector-map (lambda (n) (expt n n))
            '#(1 2 3 4 5))                \lev  #(1 4 27 256 3125)

(vector-map + '#(1 2 3) '#(4 5 6 7))       \lev  #(5 7 9)

(let ((count 0))
  (vector-map
   (lambda (ignored)
     (set! count (+ count 1))
     count)
   '#(a b)))                     \ev  #(1 2) _or_ #(2 1)
```

\end{entry}


\begin{entry}{
\proto{for-each}{ proc \vari{list} \varii{list} ...}{procedure}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it list}s.}
The arguments to ``for-each`` are like the arguments to ``map``, but
``for-each`` calls _proc_ for its side effects rather than for its
values.  Unlike ``map``, ``for-each`` is guaranteed to call _proc_ on
the elements of the _list_s in order from the first element(s) to the
last, and the value returned by ``for-each`` is unspecified.
If more than one _list_ is given and not all lists have the same length,
``for-each`` terminates when the shortest list runs out.
The _list_s can be circular, but it is an error if all of them are circular.

It is an error for _proc_ to mutate any of the lists.

```
(let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v)                                \ev  #(0 1 4 9 16)
```

\end{entry}

\begin{entry}{
\proto{string-for-each}{ proc \vari{string} \varii{string} ...}{procedure}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it string}s.}
The arguments to ``string-for-each`` are like the arguments to {\cf
string-map}, but ``string-for-each`` calls _proc_ for its side
effects rather than for its values.  Unlike ``string-map``, {\cf
string-for-each} is guaranteed to call _proc_ on the elements of
the _list_s in order from the first element(s) to the last, and the
value returned by ``string-for-each`` is unspecified.
If more than one _string_ is given and not all strings have the same length,
``string-for-each`` terminates when the shortest string runs out.
It is an error for _proc_ to mutate any of the strings.

```
(let ((v '()))
  (string-for-each
   (lambda (c) (set! v (cons (char->integer c) v)))
   "abcde")
  v)                         \ev  (101 100 99 98 97)
```

\end{entry}

\begin{entry}{
\proto{vector-for-each}{ proc \vari{vector} \varii{vector} ...}{procedure}}

\domain{It is an error if _proc_ does not
accept as many arguments as there are {\it vector}s.}
The arguments to ``vector-for-each`` are like the arguments to {\cf
vector-map}, but ``vector-for-each`` calls _proc_ for its side
effects rather than for its values.  Unlike ``vector-map``, {\cf
vector-for-each} is guaranteed to call _proc_ on the elements of
the _vector_s in order from the first element(s) to the last, and
the value returned by ``vector-for-each`` is unspecified.
If more than one _vector_ is given and not all vectors have the same length,
``vector-for-each`` terminates when the shortest vector runs out.
It is an error for _proc_ to mutate any of the vectors.

```
(let ((v (make-list 5)))
  (vector-for-each
   (lambda (i) (list-set! v i (* i i)))
   '#(0 1 2 3 4))
  v)                                \ev  (0 1 4 9 16)
```

\end{entry}


\begin{entry}{
\proto{call-with-current-continuation}{ proc}{procedure}
\proto{call/cc}{ proc}{procedure}}

{{< label "continuations" >}} \domain{It is an error if _proc_ does not accept one
argument.}
The procedure ``call-with-current-continuation`` (or its
equivalent abbreviation ``call/cc``) packages
the current continuation (see the rationale below) as an "escape
procedure" and passes it as an argument to
_proc_.
The escape procedure is a Scheme procedure that, if it is
later called, will abandon whatever continuation is in effect at that later
time and will instead use the continuation that was in effect
when the escape procedure was created.  Calling the escape procedure
will cause the invocation of _before_ and _after_ thunks installed using
\ide{dynamic-wind}.

The escape procedure accepts the same number of arguments as the continuation to
the original call to \callcc.
Most continuations take only one value.
Continuations created by the ``call-with-values``
procedure (including the initialization expressions of
``define-values``, ``let-values``, and ``let*-values`` expressions),
take the number of values that the consumer expects.
The continuations of all non-final expressions within a sequence
of expressions, such as in ``lambda``, ``case-lambda``, ``begin``,
``let``, ``let*``, ``letrec``, ``letrec*``, ``let-values``,
``let*-values``, ``let-syntax``, ``letrec-syntax``, ``parameterize``,
``guard``, ``case``, ``cond``, ``when``, and ``unless`` expressions,
take an arbitrary number of values because they discard the values passed
to them in any event.
The effect of passing no values or more than one value to continuations
that were not created in one of these ways is unspecified.


The escape procedure that is passed to _proc_ has
unlimited extent just like any other procedure in Scheme.  It can be stored
in variables or data structures and can be called as many times as desired.
However, like the ``raise`` and ``error`` procedures, it never
returns to its caller.

The following examples show only the simplest ways in which
``call-with-current-continuation`` is used.  If all real uses were as
simple as these examples, there would be no need for a procedure with
the power of ``call-with-current-continuation``.

```
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    \schtrue))                        \ev  -3

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return \schfalse))))))
          (r obj))))))

(list-length '(1 2 3 4))            \ev  4

(list-length '(a b . c))            \ev  \schfalse
```

\begin{rationale}

A common use of ``call-with-current-continuation`` is for
structured, non-local exits from loops or procedure bodies, but in fact
``call-with-current-continuation`` is useful for implementing a
wide variety of advanced control structures.
In fact, ``raise`` and ``guard`` provide a more structured mechanism
for non-local exits.

Whenever a Scheme expression is evaluated there is a
\defining{continuation} wanting the result of the expression.  The continuation
represents an entire (default) future for the computation.  If the expression is
evaluated at the REPL, for example, then the continuation might take the
result, print it on the screen, prompt for the next input, evaluate it, and
so on forever.  Most of the time the continuation includes actions
specified by user code, as in a continuation that will take the result,
multiply it by the value stored in a local variable, add seven, and give
the answer to the REPL's continuation to be printed.  Normally these
ubiquitous continuations are hidden behind the scenes and programmers do not
think much about them.  On rare occasions, however, a programmer
needs to deal with continuations explicitly.
The ``call-with-current-continuation`` procedure allows Scheme programmers to do
that by creating a procedure that acts just like the current
continuation.


\end{rationale}





\end{entry}

\begin{entry}{
\proto{values}{ obj $\ldots$}{procedure}}

Delivers all of its arguments to its continuation.
The {\tt values} procedure might be defined as follows:
```
(define (values . things)
  (call-with-current-continuation
    (lambda (cont) (apply cont things))))
```

\end{entry}

\begin{entry}{
\proto{call-with-values}{ producer consumer}{procedure}}

Calls its _producer_ argument with no arguments and
a continuation that, when passed some values, calls the
_consumer_ procedure with those values as arguments.
The continuation for the call to _consumer_ is the
continuation of the call to {\tt call-with-values}.

```
(call-with-values (lambda () (values 4 5))
                  (lambda (a b) b))
                                                   \ev  5

(call-with-values * -)                             \ev  -1
```

\end{entry}

\begin{entry}{
\proto{dynamic-wind}{ before thunk after}{procedure}}

Calls _thunk_ without arguments, returning the result(s) of this call.
_Before_ and _after_ are called, also without arguments, as required
by the following rules.  Note that, in the absence of calls to continuations
captured using \ide{call-with-current-continuation}, the three arguments are
called once each, in order.  _Before_ is called whenever execution
enters the dynamic extent of the call to _thunk_ and _after_ is called
whenever it exits that dynamic extent.  The dynamic extent of a procedure
call is the period between when the call is initiated and when it
returns.
The _before_ and _after_ thunks are called in the same dynamic
environment as the call to ``dynamic-wind``.
In Scheme, because of ``call-with-current-continuation``, the
dynamic extent of a call is not always a single, connected time period.
It is defined as follows:

- The dynamic extent is entered when execution of the body of the
called procedure begins.

- The dynamic extent is also entered when execution is not within
the dynamic extent and a continuation is invoked that was captured
(using ``call-with-current-continuation``) during the dynamic extent.

- It is exited when the called procedure returns.

- It is also exited when execution is within the dynamic extent and
a continuation is invoked that was captured while not within the
dynamic extent.


If a second call to ``dynamic-wind`` occurs within the dynamic extent of the
call to _thunk_ and then a continuation is invoked in such a way that the
_after_s from these two invocations of ``dynamic-wind`` are both to be
called, then the _after_ associated with the second (inner) call to
``dynamic-wind`` is called first.

If a second call to ``dynamic-wind`` occurs within the dynamic extent of the
call to _thunk_ and then a continuation is invoked in such a way that the
_before_s from these two invocations of ``dynamic-wind`` are both to be
called, then the _before_ associated with the first (outer) call to
``dynamic-wind`` is called first.

If invoking a continuation requires calling the _before_ from one call
to ``dynamic-wind`` and the _after_ from another, then the _after_
is called first.


The effect of using a captured continuation to enter or exit the dynamic
extent of a call to _before_ or _after_ is unspecified.

```
(let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda ()
        (add (call-with-current-continuation
               (lambda (c0)
                 (set! c c0)
                 'talk1))))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))
    \lev (connect talk1 disconnect
               connect talk2 disconnect)
```
\end{entry}

## 6.11. Exceptions
{{< label "exceptionsection" >}}

This section describes Scheme's exception-handling and
exception-raising procedures.
For the concept of Scheme exceptions, see section~\ref{errorsituations}.
See also \ref{guard} for the ``guard`` syntax.

\defining{Exception handler}s are one-argument procedures that determine the
action the program takes when an exceptional situation is signaled.
The system implicitly maintains a current exception handler
in the dynamic environment.

The program raises an exception by
invoking the current exception handler, passing it an object
encapsulating information about the exception.  Any procedure
accepting one argument can serve as an exception handler and any
object can be used to represent an exception.

\begin{entry}{
\proto{with-exception-handler}{ _handler_ _thunk_}{procedure}}

\domain{It is an error if _handler_ does not accept one argument.
It is also an error if _thunk_ does not accept zero arguments.}
The ``with-exception-handler`` procedure returns the results of invoking
_thunk_.  _Handler_ is installed as the current
exception handler
in the dynamic environment used for the invocation of _thunk_.

```
(call-with-current-continuation
 (lambda (k)
  (with-exception-handler
   (lambda (x)
    (display "condition: ")
    (write x)
    (newline)
    (k 'exception))
   (lambda ()
    (+ 1 (raise 'an-error))))))
        \ev exception
 \>_and prints_  condition: an-error

(with-exception-handler
 (lambda (x)
  (display "something went wrong\backwhack{}n"))
 (lambda ()
  (+ 1 (raise 'an-error))))
 \>_prints_  something went wrong
```

After printing, the second example then raises another exception.
\end{entry}

\begin{entry}{
\proto{raise}{ _obj_}{procedure}}

Raises an exception by invoking the current exception
handler on _obj_. The handler is called with the same
dynamic environment as that of the call to ``raise``, except that
the current exception handler is the one that was in place when the
handler being called was installed.  If the handler returns, a secondary
exception is raised in the same dynamic environment as the handler.
The relationship between _obj_ and the object raised by
the secondary exception is unspecified.
\end{entry}

\begin{entry}{
\proto{raise-continuable}{ _obj_}{procedure}}

Raises an exception by invoking the current
exception handler on _obj_. The handler is called with
the same dynamic environment as the call to
``raise-continuable``, except that: (1) the current
exception handler is the one that was in place when the handler being
called was installed, and (2) if the handler being called returns,
then it will again become the current exception handler.  If the
handler returns, the values it returns become the values returned by
the call to ``raise-continuable``.
\end{entry}

```
(with-exception-handler
  (lambda (con)
    (cond
      ((string? con)
       (display con))
      (else
       (display "a warning has been issued")))
    42)
  (lambda ()
    (+ (raise-continuable "should be a number")
       23)))
   {\it prints:} should be a number
   \ev 65
```

\begin{entry}{
\proto{error}{ _message_ _obj_ $\ldots$}{procedure}}

\domain{_Message_ should be a string.}
Raises an exception as if by calling
``raise`` on a newly allocated implementation-defined object which encapsulates
the information provided by _message_,
as well as any _obj_s, known as the \defining{irritants}.
The procedure ``error-object?`` must return {{< tt "#t" >}} on such objects.

```
(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else
          (error
            "null-list?: argument out of domain"
            l))))
```

\end{entry}

\begin{entry}{
\proto{error-object?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is an object created by ``error``
or one of an implementation-defined set of objects.  Otherwise, it returns
\schfalse.
The objects used to signal errors, including those which satisfy the
predicates ``file-error?`` and ``read-error?``, may or may not
satisfy ``error-object?``.

\end{entry}

\begin{entry}{
\proto{error-object-message}{ error-object}{procedure}}

Returns the message encapsulated by _error-object_.

\end{entry}

\begin{entry}{
\proto{error-object-irritants}{ error-object}{procedure}}

Returns a list of the irritants encapsulated by _error-object_.

\end{entry}

\begin{entry}{
\proto{read-error?}{ obj}{procedure}
\proto{file-error?}{ obj}{procedure}}

Error type predicates.  Returns {{< tt "#t" >}} if _obj_ is an
object raised by the ``read`` procedure or by the inability to open
an input or output port on a file, respectively.  Otherwise, it
returns \schfalse.

\end{entry}

## 6.12. Environments and evaluation

\begin{entry}{
\proto{environment}{ \vri{list} ...}{eval library procedure}}
{{< label "environments" >}}

This procedure returns a specifier for the environment that results by
starting with an empty environment and then importing each _list_,
considered as an import set, into it.  (See section~\ref{libraries} for
a description of import sets.)  The bindings of the environment
represented by the specifier are immutable, as is the environment itself.

\end{entry}

\begin{entry}{
\proto{scheme-report-environment}{ version}{r5rs library procedure}}

If _version_ is equal to ``5``,
corresponding to {{< rnrs 5 >}},
``scheme-report-environment`` returns a specifier for an
environment that contains only the bindings
defined in the {{< rnrs 5 >}} library.
Implementations must support this value of _version_.

Implementations may also support other values of _version_, in which
case they return a specifier for an environment containing bindings corresponding to the specified version of the report.
If _version_
is neither ``5`` nor another value supported by
the implementation, an error is signaled.


The effect of defining or assigning (through the use of ``eval``)
an identifier bound in a ``scheme-report-environment`` (for example
``car``) is unspecified.  Thus both the environment and the bindings
it contains may be immutable.

\end{entry}

\begin{entry}{
\proto{null-environment}{ version}{r5rs library procedure}}

If _version_ is equal to ``5``,
corresponding to {{< rnrs 5 >}},
the ``null-environment`` procedure returns
a specifier for an environment that contains only the
bindings for all syntactic keywords
defined in the {{< rnrs 5 >}} library.
Implementations must support this value of _version_.

Implementations may also support other values of _version_, in which
case they return a specifier for an environment containing appropriate bindings corresponding to the specified version of the report.
If _version_
is neither ``5`` nor another value supported by
the implementation, an error is signaled.

The effect of defining or assigning (through the use of ``eval``)
an identifier bound in a ``scheme-report-environment`` (for example
``car``) is unspecified.  Thus both the environment and the bindings
it contains may be immutable.

\end{entry}

\begin{entry}{
\proto{interaction-environment}{}{repl library procedure}}

This procedure returns a specifier for a mutable environment that contains an
imple\-men\-ta\-tion-defined set of bindings, typically a superset of
those exported by {\cf(scheme base)}.  The intent is that this procedure
will return the environment in which the implementation would evaluate
expressions entered by the user into a REPL.

\end{entry}

\begin{entry}{
\proto{eval}{ expr-or-def environment-specifier}{eval library procedure}}

If _expr-or-def_ is an expression, it is evaluated in the
specified environment and its values are returned.
If it is a definition, the specified identifier(s) are defined in the specified
environment, provided the environment is not immutable.
Implementations may extend ``eval`` to allow other objects.

```
(eval '(* 7 3) (environment '(scheme base)))
                                                   \ev  21

(let ((f (eval '(lambda (f x) (f x x))
               (null-environment 5))))
  (f + 10))
                                                   \ev  20
(eval '(define foo 32)
      (environment '(scheme base)))
                                                   \ev {\it{} error is signaled}
```

\end{entry}

## 6.13. Input and output

### 6.13.1. Ports
{{< label "portsection" >}}

Ports represent input and output devices.  To Scheme, an input port is
a Scheme object that can deliver data upon command, while an output
port is a Scheme object that can accept data.
Whether the input and output port types are disjoint is
implementation-dependent.

Different _port types_ operate on different data.  Scheme
imple\-men\-ta\-tions are required to support _textual ports_
and _binary ports_, but may also provide other port types.

A textual port supports reading or writing of individual characters
from or to a backing store containing characters
using ``read-char`` and ``write-char`` below, and it supports operations
defined in terms of characters, such as ``read`` and ``write``.

A binary port supports reading or writing of individual bytes from
or to a backing store containing bytes using ``read-u8`` and {\cf
write-u8} below, as well as operations defined in terms of bytes.
Whether the textual and binary port types are disjoint is
implementation-dependent.

Ports can be used to access files, devices, and similar things on the host
system on which the Scheme program is running.

\begin{entry}{
\proto{call-with-port}{ port proc}{procedure}}

\domain{It is an error if _proc_ does not accept one argument.}
The ``call-with-port``
procedure calls _proc_ with _port_ as an argument.
If _proc_ returns,
then the port is closed automatically and the values yielded by the
_proc_ are returned.  If _proc_ does not return, then
the port must not be closed automatically unless it is possible to
prove that the port will never again be used for a read or write
operation.

\begin{rationale}
Because Scheme's escape procedures have unlimited extent, it  is
possible to escape from the current continuation but later to resume it.
If implementations were permitted to close the port on any escape from the
current continuation, then it would be impossible to write portable code using
both ``call-with-current-continuation`` and ``call-with-port``.
\end{rationale}

\end{entry}

\begin{entry}{
\proto{call-with-input-file}{ string proc}{file library procedure}
\proto{call-with-output-file}{ string proc}{file library procedure}}

\domain{It is an error if _proc_ does not accept one argument.}
These procedures obtain a
textual port obtained by opening the named file for input or output
as if by ``open-input-file`` or ``open-output-file``.
The port and _proc_ are then passed to a procedure equivalent
to ``call-with-port``.
\end{entry}

\begin{entry}{
\proto{input-port?}{ obj}{procedure}
\proto{output-port?}{ obj}{procedure}
\proto{textual-port?}{ obj}{procedure}
\proto{binary-port?}{ obj}{procedure}
\proto{port?}{ obj}{procedure}}

These procedures return {{< tt "#t" >}} if _obj_ is an input port, output port,
textual port, binary port, or any
kind of port, respectively.  Otherwise they return \schfalse.

\end{entry}


\begin{entry}{
\proto{input-port-open?}{ port}{procedure}
\proto{output-port-open?}{ port}{procedure}}

Returns {{< tt "#t" >}} if _port_ is still open and capable of
performing input or output, respectively, and {{< tt "#f" >}} otherwise.


\end{entry}


\begin{entry}{
\proto{current-input-port}{}{procedure}
\proto{current-output-port}{}{procedure}
\proto{current-error-port}{}{procedure}}

Returns the current default input port, output port, or error port (an
output port), respectively.  These procedures are parameter objects, which can be
overridden with ``parameterize`` (see
section~\ref{make-parameter}).  The initial bindings for these
are implementation-defined textual ports.

\end{entry}


\begin{entry}{
\proto{with-input-from-file}{ string thunk}{file library procedure}
\proto{with-output-to-file}{ string thunk}{file library procedure}}

The file is opened for input or output
as if by ``open-input-file`` or ``open-output-file``,
and the new port is made to be the value returned by
``current-input-port`` or ``current-output-port``
(as used by {\tt (read)}, {\tt (write _obj_)}, and so forth).
The _thunk_ is then called with no arguments.  When the _thunk_ returns,
the port is closed and the previous default is restored.
It is an error if _thunk_ does not accept zero arguments.
Both procedures return the values yielded by _thunk_.
If an escape procedure
is used to escape from the continuation of these procedures, they
behave exactly as if the current input or output port had been bound
dynamically with ``parameterize``.


\end{entry}


\begin{entry}{
\proto{open-input-file}{ string}{file library procedure}
\proto{open-binary-input-file}{ string}{file library procedure}}

Takes a _string_ for an existing file and returns a textual
input port or binary input port that is capable of delivering data from the
file.  If the file does not exist or cannot be opened, an error that satisfies ``file-error?`` is signaled.

\end{entry}


\begin{entry}{
\proto{open-output-file}{ string}{file library procedure}
\proto{open-binary-output-file}{ string}{file library procedure}}

Takes a _string_ naming an output file to be created and returns a
textual output port or binary output port that is capable of writing
data to a new file by that name.

If a file with the given name already exists,
the effect is unspecified.
If the file cannot be opened,
an error that satisfies ``file-error?`` is signaled.

\end{entry}


\begin{entry}{
\proto{close-port}{ port}{procedure}
\proto{close-input-port}{ port}{procedure}
\proto{close-output-port}{ port}{procedure}}

Closes the resource associated with _port_, rendering the _port_
incapable of delivering or accepting data.
It is an error
to apply the last two procedures to a port which is not an input
or output port, respectively.
Scheme implementations may provide ports which are simultaneously
input and output ports, such as sockets; the ``close-input-port``
and ``close-output-port`` procedures can then be used to close the
input and output sides of the port independently.

These routines have no effect if the port has already been closed.


\end{entry}

\begin{entry}{
\proto{open-input-string}{ string}{procedure}}

Takes a string and returns a textual input port that delivers
characters from the string.
If the string is modified, the effect is unspecified.

\end{entry}

\begin{entry}{
\proto{open-output-string}{}{procedure}}

Returns a textual output port that will accumulate characters for
retrieval by ``get-output-string``.

\end{entry}

\begin{entry}{
\proto{get-output-string}{ port}{procedure}}

\domain{It is an error if _port_ was not created with
``open-output-string``.}
Returns a string consisting of the
characters that have been output to the port so far in the order they
were output.
If the result string is modified, the effect is unspecified.

```
(parameterize
    ((current-output-port
      (open-output-string)))
    (display "piece")
    (display " by piece ")
    (display "by piece.")
    (newline)
    (get-output-string (current-output-port)))
\lev "piece by piece by piece.\backwhack{}n"
```

\end{entry}

\begin{entry}{
\proto{open-input-bytevector}{ bytevector}{procedure}}

Takes a bytevector and returns a binary input port that delivers
bytes from the bytevector.

\end{entry}

\begin{entry}{
\proto{open-output-bytevector}{}{procedure}}

Returns a binary output port that will accumulate bytes for
retrieval by ``get-output-bytevector``.

\end{entry}

\begin{entry}{
\proto{get-output-bytevector}{ port}{procedure}}

\domain{It is an error if _port_ was not created with
``open-output-bytevector``.}  Returns a bytevector consisting
of the bytes that have been output to the port so far in the
order they were output.
\end{entry}


### 6.13.2. Input
{{< label "inputsection" >}}

If _port_ is omitted from any input procedure, it defaults to the
value returned by ``(current-input-port)``.
It is an error to attempt an input operation on a closed port.

\noindent \hbox{ }
\vspace{-5ex}


\begin{entry}{
\proto{read}{}{read library procedure}
\rproto{read}{ port}{read library procedure}}

The ``read`` procedure converts external representations of Scheme objects into the
objects themselves.  That is, it is a parser for the non-terminal
\meta{datum} (see sections~\ref{datum} and
\ref{listsection}).  It returns the next
object parsable from the given textual input _port_, updating
_port_ to point to
the first character past the end of the external representation of the object.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

If an end of file is encountered in the input before any
characters are found that can begin an object, then an end-of-file
object is returned.  The port remains open, and further attempts
to read will also return an end-of-file object.  If an end of file is
encountered after the beginning of an object's external representation,
but the external representation is incomplete and therefore not parsable,
an error that satisfies ``read-error?`` is signaled.

\end{entry}

\begin{entry}{
\proto{read-char}{}{procedure}
\rproto{read-char}{ port}{procedure}}

Returns the next character available from the textual input _port_,
updating
the _port_ to point to the following character.  If no more characters
are available, an end-of-file object is returned.

\end{entry}


\begin{entry}{
\proto{peek-char}{}{procedure}
\rproto{peek-char}{ port}{procedure}}

Returns the next character available from the textual input _port_,
but _without_ updating
the _port_ to point to the following character.  If no more characters
are available, an end-of-file object is returned.

*Note:&nbsp;*
The value returned by a call to ``peek-char`` is the same as the
value that would have been returned by a call to ``read-char`` with the
same _port_.  The only difference is that the very next call to
``read-char`` or ``peek-char`` on that _port_ will return the
value returned by the preceding call to ``peek-char``.  In particular, a call
to ``peek-char`` on an interactive port will hang waiting for input
whenever a call to ``read-char`` would have hung.


\end{entry}

\begin{entry}{
\proto{read-line}{}{procedure}
\rproto{read-line}{ port}{procedure}}

Returns the next line of text available from the textual input
_port_, updating the _port_ to point to the following character.
If an end of line is read, a string containing all of the text up to
(but not including) the end of line is returned, and the port is updated
to point just past the end of line. If an end of file is encountered
before any end of line is read, but some characters have been
read, a string containing those characters is returned. If an end of
file is encountered before any characters are read, an end-of-file
object is returned.  For the purpose of this procedure, an end of line
consists of either a linefeed character, a carriage return character, or a
sequence of a carriage return character followed by a linefeed character.
Implementations may also recognize other end of line characters or sequences.

\end{entry}


\begin{entry}{
\proto{eof-object?}{ obj}{procedure}}

Returns {{< tt "#t" >}} if _obj_ is an end-of-file object, otherwise returns
\schfalse.  The precise set of end-of-file objects will vary among
implementations, but in any case no end-of-file object will ever be an object
that can be read in using ``read``.

\end{entry}

\begin{entry}{
\proto{eof-object}{}{procedure}}

Returns an end-of-file object, not necessarily unique.

\end{entry}


\begin{entry}{
\proto{char-ready?}{}{procedure}
\rproto{char-ready?}{ port}{procedure}}

Returns {{< tt "#t" >}} if a character is ready on the textual input _port_ and
returns {{< tt "#f" >}} otherwise.  If ``char-ready`` returns {{< tt "#t" >}} then
the next ``read-char`` operation on the given _port_ is guaranteed
not to hang.  If the _port_ is at end of file then ``char-ready?``\
returns \schtrue.

\begin{rationale}
The ``char-ready?`` procedure exists to make it possible for a program to
accept characters from interactive ports without getting stuck waiting for
input.  Any input editors associated with such ports must ensure that
characters whose existence has been asserted by ``char-ready?`` cannot
be removed from the input.  If ``char-ready?`` were to return {{< tt "#f" >}} at end of
file, a port at end of file would be indistinguishable from an interactive
port that has no ready characters.
\end{rationale}
\end{entry}

\begin{entry}{
\proto{read-string}{ k}{procedure}
\rproto{read-string}{ k port}{procedure}}
{{< label "readstring" >}}

Reads the next _k_ characters, or as many as are available before the end of file,
from the textual
input _port_ into a newly allocated string in left-to-right order
and returns the string.
If no characters are available before the end of file,
an end-of-file object is returned.

\end{entry}


\begin{entry}{
\proto{read-u8}{}{procedure}
\rproto{read-u8}{ port}{procedure}}

Returns the next byte available from the binary input _port_,
updating the _port_ to point to the following byte.
If no more bytes are
available, an end-of-file object is returned.

\end{entry}

\begin{entry}{
\proto{peek-u8}{}{procedure}
\rproto{peek-u8}{ port}{procedure}}

Returns the next byte available from the binary input _port_,
but _without_ updating the _port_ to point to the following
byte.  If no more bytes are available, an end-of-file object is returned.

\end{entry}

\begin{entry}{
\proto{u8-ready?}{}{procedure}
\rproto{u8-ready?}{ port}{procedure}}

Returns {{< tt "#t" >}} if a byte is ready on the binary input _port_
and returns {{< tt "#f" >}} otherwise.  If ``u8-ready?`` returns
{{< tt "#t" >}} then the next ``read-u8`` operation on the given
_port_ is guaranteed not to hang.  If the _port_ is at end of
file then ``u8-ready?`` returns \schtrue.

\end{entry}

\begin{entry}{
\proto{read-bytevector}{ k}{procedure}
\rproto{read-bytevector}{ k port}{procedure}}

Reads the next _k_ bytes, or as many as are available before the end of file,
from the binary
input _port_ into a newly allocated bytevector in left-to-right order
and returns the bytevector.
If no bytes are available before the end of file,
an end-of-file object is returned.

\end{entry}

\begin{entry}{
\proto{read-bytevector!}{ bytevector}{procedure}
\rproto{read-bytevector!}{ bytevector port}{procedure}
\rproto{read-bytevector!}{ bytevector port start}{procedure}
\rproto{read-bytevector!}{ bytevector port start end}{procedure}}

Reads the next $end - start$ bytes, or as many as are available
before the end of file,
from the binary
input _port_ into _bytevector_ in left-to-right order
beginning at the _start_ position.  If _end_ is not supplied,
reads until the end of _bytevector_ has been reached.  If
_start_ is not supplied, reads beginning at position 0.
Returns the number of bytes read.
If no bytes are available, an end-of-file object is returned.

\end{entry}


### 6.13.3. Output
{{< label "outputsection" >}}

If _port_ is omitted from any output procedure, it defaults to the
value returned by ``(current-output-port)``.
It is an error to attempt an output operation on a closed port.

\noindent \hbox{}
\vspace{-5ex}

\begin{entry}{
\proto{write}{ obj}{write library procedure}
\rproto{write}{ obj port}{write library procedure}}

Writes a representation of _obj_ to the given textual output
_port_.  Strings
that appear in the written representation are enclosed in quotation marks, and
within those strings backslash and quotation mark characters are
escaped by backslashes.  Symbols that contain non-ASCII characters
are escaped with vertical lines.
Character objects are written using the ``#\backwhack`` notation.

If _obj_ contains cycles which would cause an infinite loop using
the normal written representation, then at least the objects that form
part of the cycle must be represented using datum labels as described
in section~\ref{labelsection}.  Datum labels must not be used if there
are no cycles.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

The ``write`` procedure returns an unspecified value.

\end{entry}

\begin{entry}{
\proto{write-shared}{ obj}{write library procedure}
\rproto{write-shared}{ obj port}{write library procedure}}

The ``write-shared`` procedure is the same as ``write``, except that
shared structure must be represented using datum labels for all pairs
and vectors that appear more than once in the output.

\end{entry}

\begin{entry}{
\proto{write-simple}{ obj}{write library procedure}
\rproto{write-simple}{ obj port}{write library procedure}}

The ``write-simple`` procedure is the same as ``write``, except that shared structure is
never represented using datum labels.  This can cause ``write-simple`` not to
terminate if _obj_ contains circular structure.

\end{entry}


\begin{entry}{
\proto{display}{ obj}{write library procedure}
\rproto{display}{ obj port}{write library procedure}}

Writes a representation of _obj_ to the given textual output _port_.
Strings that appear in the written representation are output as if by
``write-string`` instead of by ``write``.
Symbols are not escaped.  Character
objects appear in the representation as if written by ``write-char``
instead of by ``write``.

The ``display`` representation of other objects is unspecified.
However, ``display`` must not loop forever on
self-referencing pairs, vectors, or records.  Thus if the
normal ``write`` representation is used, datum labels are needed
to represent cycles as in ``write``.

Implementations may support extended syntax to represent record types or
other types that do not have datum representations.

The ``display`` procedure returns an unspecified value.

\begin{rationale}
The ``write`` procedure is intended
for producing mach\-ine-readable output and ``display`` for producing
human-readable output.
\end{rationale}
\end{entry}


\begin{entry}{
\proto{newline}{}{procedure}
\rproto{newline}{ port}{procedure}}

Writes an end of line to textual output _port_.  Exactly how this
is done differs
from one operating system to another.  Returns an unspecified value.

\end{entry}


\begin{entry}{
\proto{write-char}{ char}{procedure}
\rproto{write-char}{ char port}{procedure}}

Writes the character _char_ (not an external representation of the
character) to the given textual output _port_ and returns an unspecified
value.

\end{entry}

\begin{entry}{
\proto{write-string}{ string}{procedure}
\rproto{write-string}{ string port}{procedure}
\rproto{write-string}{ string port start}{procedure}
\rproto{write-string}{ string port start end}{procedure}}

Writes the characters of _string_
from _start_ to _end_
in left-to-right order to the
textual output _port_.

\end{entry}

\begin{entry}{
\proto{write-u8}{ byte}{procedure}
\rproto{write-u8}{ byte port}{procedure}}

Writes the _byte_ to
the given binary output _port_ and returns an unspecified value.

\end{entry}

\begin{entry}{
\proto{write-bytevector}{ bytevector}{procedure}
\rproto{write-bytevector}{ bytevector port}{procedure}
\rproto{write-bytevector}{ bytevector port start}{procedure}
\rproto{write-bytevector}{ bytevector port start end}{procedure}}

Writes the bytes of _bytevector_
from _start_ to _end_
in left-to-right order to the
binary output _port_.

\end{entry}

\begin{entry}{
\proto{flush-output-port}{}{procedure}
\rproto{flush-output-port}{ port}{procedure}}

Flushes any buffered output from the buffer of output-port to the
underlying file or device and returns an unspecified value.

\end{entry}


## 6.14. System interface

Questions of system interface generally fall outside of the domain of this
report.  However, the following operations are important enough to
deserve description here.


\begin{entry}{
\proto{load}{ filename}{load library procedure}
\rproto{load}{ filename environment-specifier}{load library procedure}}

\domain{It is an error if _filename_ is not a string.}
An implementation-dependent operation is used to transform
_filename_ into the name of an existing file
containing Scheme source code.  The ``load`` procedure reads
expressions and definitions from the file and evaluates them
sequentially in the environment specified by _environment-specifier_.
If _environment-specifier_ is omitted, ``(interaction-environment)``
is assumed.

It is unspecified whether the results of the expressions
are printed.  The ``load`` procedure does not affect the values
returned by ``current-input-port`` and ``current-output-port``.
It returns an unspecified value.


\begin{rationale}
For portability, ``load`` must operate on source files.
Its operation on other kinds of files necessarily varies among
implementations.
\end{rationale}
\end{entry}

\begin{entry}{
\proto{file-exists?}{ filename}{file library procedure}}

\domain{It is an error if _filename_ is not a string.}
The ``file-exists?`` procedure returns
{{< tt "#t" >}} if the named file exists at the time the procedure is called,
and {{< tt "#f" >}} otherwise.

\end{entry}

\begin{entry}{
\proto{delete-file}{ filename}{file library procedure}}

\domain{It is an error if _filename_ is not a string.}
The ``delete-file`` procedure deletes the
named file if it exists and can be deleted, and returns an unspecified
value.  If the file does not exist or cannot be deleted, an error
that satisfies ``file-error?`` is signaled.

\end{entry}

\begin{entry}{
\proto{command-line}{}{process-context library procedure}}

Returns the command line passed to the process as a list of
strings.  The first string corresponds to the command name, and is
implementation-dependent.  It is an error to mutate any of these strings.
\end{entry}

\begin{entry}{
\proto{exit}{}{process-context library procedure}
\rproto{exit}{ obj}{process-context library procedure}}

Runs all outstanding dynamic-wind _after_ procedures, terminates the
running program, and communicates an exit value to the operating system.
If no argument is supplied, or if _obj_ is {{< tt "#t" >}}, the {\cf
exit} procedure should communicate to the operating system that the
program exited normally.  If _obj_ is {{< tt "#f" >}}, the ``exit``
procedure should communicate to the operating system that the program
exited abnormally.  Otherwise, ``exit`` should translate _obj_ into
an appropriate exit value for the operating system, if possible.

The ``exit`` procedure
must not signal an exception or return to its continuation.

*Note:&nbsp;*
Because of the requirement to run handlers, this procedure is not just the
operating system's exit procedure.


\end{entry}

\begin{entry}{
\proto{emergency-exit}{}{process-context library procedure}
\rproto{emergency-exit}{ obj}{process-context library procedure}}

Terminates the program without running any
outstanding dynamic-wind _after_ procedures
and communicates an exit value to the operating system
in the same manner as ``exit``.

*Note:&nbsp;*
The ``emergency-exit`` procedure corresponds to the ``_exit`` procedure
in Windows and Posix.


\end{entry}




\begin{entry}{
\proto{get-environment-variable}{ name}{process-context library procedure}}

Many operating systems provide each running process with an
\defining{environment} consisting of \defining{environment variables}.
(This environment is not to be confused with the Scheme environments that
can be passed to ``eval``: see section~\ref{environments}.)
Both the name and value of an environment variable are strings.
The procedure ``get-environment-variable`` returns the value
of the environment variable _name_,
or {{< tt "#f" >}} if the named
environment variable is not found.  It may
use locale information to encode the name and decode the value
of the environment variable.  It is an error if \\
``get-environment-variable`` can't decode the value.
It is also an error to mutate the resulting string.

```
(get-environment-variable "PATH") \lev "/usr/local/bin:/usr/bin:/bin"
```

\end{entry}

\begin{entry}{
\proto{get-environment-variables}{}{process-context library procedure}}

Returns the names and values of all the environment variables as an
alist, where the car of each entry is the name of an environment
variable and the cdr is its value, both as strings.  The order of the list is unspecified.
It is an error to mutate any of these strings or the alist itself.

```
(get-environment-variables) \lev (("USER" . "root") ("HOME" . "/"))
```

\end{entry}

\begin{entry}{
\proto{current-second}{}{time library procedure}}

Returns an inexact number representing the current time on the International Atomic
Time (TAI) scale.  The value 0.0 represents midnight
on January 1, 1970 TAI (equivalent to ten seconds before midnight Universal Time)
and the value 1.0 represents one TAI
second later.  Neither high accuracy nor high precision are required; in particular,
returning Coordinated Universal Time plus a suitable constant might be
the best an implementation can do.
\end{entry}

\begin{entry}{
\proto{current-jiffy}{}{time library procedure}}

Returns the number of \defining{jiffies} as an exact integer that have elapsed since an arbitrary,
implementation-defined epoch. A jiffy is an implementation-defined
fraction of a second which is defined by the return value of the
``jiffies-per-second`` procedure. The starting epoch is guaranteed to be
constant during a run of the program, but may vary between runs.

\begin{rationale}
Jiffies are allowed to be implementation-dependent so that
``current-jiffy`` can execute with minimum overhead. It
should be very likely that a compactly represented integer will suffice
as the returned value.  Any particular jiffy size will be inappropriate
for some implementations: a microsecond is too long for a very fast
machine, while a much smaller unit would force many implementations to
return integers which have to be allocated for most calls, rendering
``current-jiffy`` less useful for accurate timing measurements.
\end{rationale}

\end{entry}

\begin{entry}{
\proto{jiffies-per-second}{}{time library procedure}}

Returns an exact integer representing the number of jiffies per SI
second. This value is an implementation-specified constant.

```
(define (time-length)
  (let ((list (make-list 100000))
        (start (current-jiffy)))
    (length list)
    (/ (- (current-jiffy) start)
       (jiffies-per-second))))
```
\end{entry}

\begin{entry}{
\proto{features}{}{procedure}}

Returns a list of the feature identifiers which ``cond-expand``
treats as true.  It is an error to modify this list.  Here is an
example of what ``features`` might return:

```
(features) \ev
  (r7rs ratios exact-complex full-unicode
   gnu-linux little-endian
   fantastic-scheme
   fantastic-scheme-1.0
   space-ship-control-system)
```
\end{entry}
