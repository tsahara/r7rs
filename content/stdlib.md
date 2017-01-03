+++
weight = 73
title = "Standard Libraries"
menu = "main"
+++
# Standard Libraries
\label{stdlibraries}

%% Note, this is used to generate stdmod.tex.  The bindings could be
%% extracted automatically from the document, but this lets us choose
%% the ordering and optionally format manually where needed.

This section lists the exports provided by the standard libraries.  The
libraries are factored so as to separate features which might not be
supported by all implementations, or which might be expensive to load.

The ``scheme`` library prefix is used for all standard libraries, and
is reserved for use by future standards.

\textbf{Base Library}

The \texttt{(scheme base)} library exports many of the procedures and
syntax bindings that are traditionally associated with Scheme.
The division between the base library and the other standard libraries is
based on use, not on construction. In particular, some facilities that are
typically implemented as primitives by a compiler or the run-time system
rather than in terms of other standard procedures or syntax are
not part of the base library, but are defined in separate libraries.
By the same token, some exports of the base library are implementable
in terms of other exports.  They are redundant in the strict sense of
the word, but they capture common patterns of usage, and are therefore
provided as convenient abbreviations.

```
``*``                       ``+``
``-``                       ``...``
``/``                       ``<``
``<=``                      ``=``
``=>``                      ``>``
``>=``                      ``_``
``abs``                     ``and``
``append``                  ``apply``
``assoc``                   ``assq``
``assv``                    ``begin``
``binary-port? ``           ``boolean=?``
``boolean? ``               ``bytevector``
``bytevector-append``       ``bytevector-copy``
``bytevector-copy!``        ``bytevector-length``
``bytevector-u8-ref``       ``bytevector-u8-set!``
``bytevector? ``            ``caar``
``cadr``
``call-with-current-continuation``
``call-with-port``          ``call-with-values``
``call/cc``                 ``car``
``case``                    ``cdar``
``cddr``                    ``cdr``
``ceiling``                 ``char->integer``
``char-ready? ``            ``char<=?``
``char<? ``                 ``char=?``
``char>=? ``                ``char>?``
``char? ``                  ``close-input-port``
``close-output-port``       ``close-port``
``complex? ``               ``cond``
``cond-expand``             ``cons``
``current-error-port``      ``current-input-port``
``current-output-port``     ``define``
``define-record-type``      ``define-syntax``
``define-values``           ``denominator``
``do``                      ``dynamic-wind``
``else``                    ``eof-object``
``eof-object? ``            ``eq?``
``equal? ``                 ``eqv?``
``error``                   ``error-object-irritants``
``error-object-message``    ``error-object?``
``even? ``                  ``exact``
``exact-integer-sqrt``      ``exact-integer?``
``exact? ``                 ``expt``
``features``                ``file-error?``
``floor``                   ``floor-quotient``
``floor-remainder``         ``floor/``
``flush-output-port``       ``for-each``
``gcd``                     ``get-output-bytevector``
``get-output-string``       ``guard``
``if``                      ``include``
``include-ci``              ``inexact``
``inexact? ``               ``input-port-open?``
``input-port? ``            ``integer->char``
``integer? ``               ``lambda``
``lcm``                     ``length``
``let``                     ``let*``
``let*-values``             ``let-syntax``
``let-values``              ``letrec``
``letrec*``                 ``letrec-syntax``
``list``                    ``list->string``
``list->vector``            ``list-copy``
``list-ref``                ``list-set!``
``list-tail``               ``list?``
``make-bytevector``         ``make-list``
``make-parameter``          ``make-string``
``make-vector``             ``map``
``max``                     ``member``
``memq``                    ``memv``
``min``                     ``modulo``
``negative? ``              ``newline``
``not``                     ``null?``
``number->string``          ``number?``
``numerator``               ``odd?``
``open-input-bytevector``   ``open-input-string``
``open-output-bytevector``  ``open-output-string``
``or``                      ``output-port-open?``
``output-port? ``           ``pair?``
``parameterize``            ``peek-char``
``peek-u8``                 ``port?``
``positive? ``              ``procedure?``
``quasiquote``              ``quote``
``quotient``                ``raise``
``raise-continuable``       ``rational?``
``rationalize``             ``read-bytevector``
``read-bytevector!``        ``read-char``
``read-error? ``            ``read-line``
``read-string``             ``read-u8``
``real? ``                  ``remainder``
``reverse``                 ``round``
``set!``                    ``set-car!``
``set-cdr!``                ``square``
``string``                  ``string->list``
``string->number``          ``string->symbol``
``string->utf8``            ``string->vector``
``string-append``           ``string-copy``
``string-copy!``            ``string-fill!``
``string-for-each``         ``string-length``
``string-map``              ``string-ref``
``string-set!``             ``string<=?``
``string<? ``               ``string=?``
``string>=? ``              ``string>?``
``string? ``                ``substring``
``symbol->string``          ``symbol=?``
``symbol? ``                ``syntax-error``
``syntax-rules``            ``textual-port?``
``truncate``                ``truncate-quotient``
``truncate-remainder``      ``truncate/``
``u8-ready? ``              ``unless``
``unquote``                 ``unquote-splicing``
``utf8->string``            ``values``
``vector``                  ``vector->list``
``vector->string``          ``vector-append``
``vector-copy``             ``vector-copy!``
``vector-fill!``            ``vector-for-each``
``vector-length``           ``vector-map``
``vector-ref``              ``vector-set!``
``vector? ``                ``when``
``with-exception-handler``  ``write-bytevector``
``write-char``              ``write-string``
``write-u8``                ``zero?``
```

\textbf{Case-Lambda Library}

The \texttt{(scheme case-lambda)} library exports the ``case-lambda``
syntax.

```
``case-lambda``
```

\textbf{Char Library}

The \texttt{(scheme char)} library provides the procedures for dealing with
characters that involve potentially large tables when supporting all of Unicode.

```
``char-alphabetic? ``       ``char-ci<=?``
``char-ci<? ``              ``char-ci=?``
``char-ci>=? ``             ``char-ci>?``
``char-downcase``           ``char-foldcase``
``char-lower-case? ``       ``char-numeric?``
``char-upcase``             ``char-upper-case?``
``char-whitespace? ``       ``digit-value``
``string-ci<=? ``           ``string-ci<?``
``string-ci=? ``            ``string-ci>=?``
``string-ci>? ``            ``string-downcase``
``string-foldcase``         ``string-upcase``
```

\textbf{Complex Library}

The \texttt{(scheme complex)} library exports procedures which are
typically only useful with non-real numbers.

```
``angle``                   ``imag-part``
``magnitude``               ``make-polar``
``make-rectangular``        ``real-part``
```

\textbf{CxR Library}

The \texttt{(scheme cxr)} library exports twenty-four procedures which
are the compositions of from three to four ``car`` and ``cdr``
operations.  For example ``caddar`` could be defined by

```
(define caddar
  (lambda (x) (car (cdr (cdr (car x)))))){\rm.}
```

The procedures ``car`` and ``cdr`` themselves and the four
two-level compositions are included in the base library.  See
section~\ref{listsection}.

```
``caaaar``                  ``caaadr``
``caaar``                   ``caadar``
``caaddr``                  ``caadr``
``cadaar``                  ``cadadr``
``cadar``                   ``caddar``
``cadddr``                  ``caddr``
``cdaaar``                  ``cdaadr``
``cdaar``                   ``cdadar``
``cdaddr``                  ``cdadr``
``cddaar``                  ``cddadr``
``cddar``                   ``cdddar``
``cddddr``                  ``cdddr``
```

\textbf{Eval Library}

The \texttt{(scheme eval)} library exports procedures for evaluating Scheme
data as programs.

```
``environment``             ``eval``
```

\textbf{File Library}

The \texttt{(scheme file)} library provides procedures for accessing
files.

```
``call-with-input-file``    ``call-with-output-file``
``delete-file``             ``file-exists?``
``open-binary-input-file``  ``open-binary-output-file``
``open-input-file``         ``open-output-file``
``with-input-from-file``    ``with-output-to-file``
```

\textbf{Inexact Library}

The \texttt{(scheme inexact)} library exports procedures which are
typically only useful with inexact values.

```
``acos``                    ``asin``
``atan``                    ``cos``
``exp``                     ``finite?``
``infinite? ``              ``log``
``nan? ``                   ``sin``
``sqrt``                    ``tan``
```

\textbf{Lazy Library}

The \texttt{(scheme lazy)} library exports procedures and syntax keywords for lazy evaluation.

```
``delay``                   ``delay-force``
``force``                   ``make-promise``
``promise?``
```

\textbf{Load Library}

The \texttt{(scheme load)} library exports procedures for loading
Scheme expressions from files.

```
``load``
```

\textbf{Process-Context Library}

The \texttt{(scheme process-context)} library exports procedures for
accessing with the program's calling context.

```
``command-line``            ``emergency-exit``
``exit``
``get-environment-variable``
``get-environment-variables``
```

\textbf{Read Library}

The \texttt{(scheme read)} library provides procedures for reading
Scheme objects.

```
``read``
```

\textbf{Repl Library}

The \texttt{(scheme repl)} library exports the {\cf
  interaction-environment} procedure.

```
``interaction-environment``
```

\textbf{Time Library}

The \texttt{(scheme time)} library provides access to time-related values.

```
``current-jiffy``           ``current-second``
``jiffies-per-second``
```

\textbf{Write Library}

The \texttt{(scheme write)} library provides procedures for writing
Scheme objects.

```
``display``                 ``write``
``write-shared``            ``write-simple``
```

\textbf{R5RS Library}

The \texttt{(scheme r5rs)} library provides the identifiers defined by
{{< rnrs 5 >}}, except that
``transcript-on`` and ``transcript-off`` are not present.
Note that
the ``exact`` and ``inexact`` procedures appear under their {{< rnrs 5 >}} names
``inexact->exact`` and ``exact->inexact`` respectively.
However, if an implementation does not provide a particular library such as the
complex library, the corresponding identifiers will not appear in this
library either.

```
``*``                       ``+``
``-``                       ``/``
``<``                       ``<=``
``=``                       ``>``
``>=``                      ``abs``
``acos``                    ``and``
``angle``                   ``append``
``apply``                   ``asin``
``assoc``                   ``assq``
``assv``                    ``atan``
``begin``                   ``boolean?``
``caaaar``                  ``caaadr``
``caaar``                   ``caadar``
``caaddr``                  ``caadr``
``caar``                    ``cadaar``
``cadadr``                  ``cadar``
``caddar``                  ``cadddr``
``caddr``                   ``cadr``
``call-with-current-continuation``
``call-with-input-file``    ``call-with-output-file``
``call-with-values``        ``car``
``case``                    ``cdaaar``
``cdaadr``                  ``cdaar``
``cdadar``                  ``cdaddr``
``cdadr``                   ``cdar``
``cddaar``                  ``cddadr``
``cddar``                   ``cdddar``
``cddddr``                  ``cdddr``
``cddr``                    ``cdr``
``ceiling``                 ``char->integer``
``char-alphabetic? ``       ``char-ci<=?``
``char-ci<? ``              ``char-ci=?``
``char-ci>=? ``             ``char-ci>?``
``char-downcase``           ``char-lower-case?``
``char-numeric? ``          ``char-ready?``
``char-upcase``             ``char-upper-case?``
``char-whitespace? ``       ``char<=?``
``char<? ``                 ``char=?``
``char>=? ``                ``char>?``
``char? ``                  ``close-input-port``
``close-output-port``       ``complex?``
``cond``                    ``cons``
``cos``                     ``current-input-port``
``current-output-port``     ``define``
``define-syntax``           ``delay``
``denominator``             ``display``
``do``                      ``dynamic-wind``
``eof-object? ``            ``eq?``
``equal? ``                 ``eqv?``
``eval``                    ``even?``
``exact->inexact``          ``exact?``
``exp``                     ``expt``
``floor``                   ``for-each``
``force``                   ``gcd``
``if``                      ``imag-part``
``inexact->exact``          ``inexact?``
``input-port? ``            ``integer->char``
``integer? ``               ``interaction-environment``
``lambda``                  ``lcm``
``length``                  ``let``
``let*``                    ``let-syntax``
``letrec``                  ``letrec-syntax``
``list``                    ``list->string``
``list->vector``            ``list-ref``
``list-tail``               ``list?``
``load``                    ``log``
``magnitude``               ``make-polar``
``make-rectangular``        ``make-string``
``make-vector``             ``map``
``max``                     ``member``
``memq``                    ``memv``
``min``                     ``modulo``
``negative? ``              ``newline``
``not``                     ``null-environment``
``null? ``                  ``number->string``
``number? ``                ``numerator``
``odd? ``                   ``open-input-file``
``open-output-file``        ``or``
``output-port? ``           ``pair?``
``peek-char``               ``positive?``
``procedure? ``             ``quasiquote``
``quote``                   ``quotient``
``rational? ``              ``rationalize``
``read``                    ``read-char``
``real-part``               ``real?``
``remainder``               ``reverse``
``round``
``scheme-report-environment``
``set!``                    ``set-car!``
``set-cdr!``                ``sin``
``sqrt``                    ``string``
``string->list``            ``string->number``
``string->symbol``          ``string-append``
``string-ci<=? ``           ``string-ci<?``
``string-ci=? ``            ``string-ci>=?``
``string-ci>? ``            ``string-copy``
``string-fill!``            ``string-length``
``string-ref``              ``string-set!``
``string<=? ``              ``string<?``
``string=? ``               ``string>=?``
``string>? ``               ``string?``
``substring``               ``symbol->string``
``symbol? ``                ``tan``
``truncate``                ``values``
``vector``                  ``vector->list``
``vector-fill!``            ``vector-length``
``vector-ref``              ``vector-set!``
``vector? ``                ``with-input-from-file``
``with-output-to-file``     ``write``
``write-char``              ``zero?``
```
