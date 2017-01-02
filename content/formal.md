+++
weight = 61
title = "Formal syntax and semantics"
menu = "main"
chapter = 7
+++
# 7. Formal syntax and semantics
\label{formalchapter}

This chapter provides formal descriptions of what has already been
described informally in previous chapters of this report.



## 7.1. Formal syntax
\label{BNF}

This section provides a formal syntax for Scheme written in an extended
BNF.

All spaces in the grammar are for legibility.  Case is not significant
except in the definitions of \meta{letter}, \meta{character name} and \meta{mnemonic escape}; for example, {\cf \#x1A}
and {\cf \#X1a} are equivalent, but {\cf foo} and {\cf Foo}
and {\cf \#\backwhack{}space} and {\cf \#\backwhack{}Space} are distinct.
\meta{empty} stands for the empty string.

The following extensions to BNF are used to make the description more
concise:  \arbno{\meta{thing}} means zero or more occurrences of
\meta{thing}; and \atleastone{\meta{thing}} means at least one
\meta{thing}.


### 7.1.1. Lexical structure

This section describes how individual tokens\index{token} (identifiers,
numbers, etc.) are formed from sequences of characters.  The following
sections describe how expressions and programs are formed from sequences
of tokens.

\meta{Intertoken space} can occur on either side of any token, but not
within a token.

Identifiers that do not begin with a vertical line are
terminated by a \meta{delimiter} or by the end of the input.
So are dot, numbers, characters, and booleans.
Identifiers that begin with a vertical line are terminated by another vertical line.

The following four characters from the ASCII repertoire
are reserved for future extensions to the
language: {\tt \verb"[" \verb"]" \verb"{" \verb"}"}

In addition to the identifier characters of the ASCII repertoire specified
below, Scheme implementations may permit any additional repertoire of
Unicode characters to be employed in identifiers,
provided that each such character has a Unicode general category of Lu,
Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So,
or Co, or is U+200C or U+200D (the zero-width non-joiner and joiner,
respectively, which are needed for correct spelling in Persian, Hindi,
and other languages).
However, it is an error for the first character to have a general category
of Nd, Mc, or Me.  It is also an error to use a non-Unicode character
in symbols or identifiers.

All Scheme implementations must permit the escape sequence
{\tt \backwhack{}x<hexdigits>;}
to appear in Scheme identifiers that are enclosed in vertical lines. If the character
with the given Unicode scalar value is supported by the implementation,
identifiers containing such a sequence are equivalent to identifiers
containing the corresponding character.

\begin{grammar}%
\meta{token} \: \meta{identifier} \| \meta{boolean} \| \meta{number}\index{identifier}
\>  \| \meta{character} \| \meta{string}
\>  \| ( \| ) \| \sharpsign( \| \sharpsign u8( \| \singlequote{} \| \backquote{} \| , \| ,@ \| {\bf.}
\meta{delimiter} \: \meta{whitespace} \| \meta{vertical line}
\> \| ( \| ) \| " \| ;
\meta{intraline whitespace} \: \meta{space or tab}
\meta{whitespace} \: \meta{intraline whitespace} \| \meta{line ending}
\meta{vertical line} \: |
\meta{line ending} \: \meta{newline} \| \meta{return} \meta{newline}
\> \| \meta{return}
\meta{comment} \: ; \= $\langle$\rm all subsequent characters up to a
		    \>\ \rm line ending$\rangle$\index{comment}
\> \| \meta{nested comment}
\> \| \#; \meta{intertoken space} \meta{datum}
\meta{nested comment} \: \#| \= \meta{comment text}
\> \arbno{\meta{comment cont}} |\#
\meta{comment text} \: \= $\langle$\rm character sequence not containing
\>\ \rm {\tt \#|} or {\tt |\#}$\rangle$
\meta{comment cont} \: \meta{nested comment} \meta{comment text}
\meta{directive} \: \#!fold-case \| \#!no-fold-case%
\end{grammar}

Note that it is ungrammatical to follow a \meta{directive} with anything
but a \meta{delimiter} or the end of file.

\begin{grammar}%
\meta{atmosphere} \: \meta{whitespace} \| \meta{comment} \| \meta{directive}
\meta{intertoken space} \: \arbno{\meta{atmosphere}}%
\end{grammar}

\label{extendedalphas}
\label{identifiersyntax}

% This is a kludge, but \multicolumn doesn't work in tabbing environments.
\setbox0\hbox{\cf\meta{identifier} \goesto{} $\langle$}

Note that {\cf +i}, {\cf -i} and \meta{infnan} below are exceptions to the
\meta{peculiar identifier} rule; they are parsed as numbers, not
identifiers.

\begin{grammar}%
\meta{identifier} \: \meta{initial} \arbno{\meta{subsequent}}
 \>  \| \meta{vertical line} \arbno{\meta{symbol element}} \meta{vertical line}
 \>  \| \meta{peculiar identifier}
\meta{initial} \: \meta{letter} \| \meta{special initial}
\meta{letter} \: a \| b \| c \| ... \| z
\> \| A \| B \| C \| ... \| Z
\meta{special initial} \: ! \| \$ \| \% \| \verb"&" \| * \| / \| : \| < \| =
 \>  \| > \| ? \| \verb"^" \| \verb"_" \| \verb"~"
\meta{subsequent} \: \meta{initial} \| \meta{digit}
 \>  \| \meta{special subsequent}
\meta{digit} \: 0 \| 1 \| 2 \| 3 \| 4 \| 5 \| 6 \| 7 \| 8 \| 9
\meta{hex digit} \: \meta{digit} \| a \| b \| c \| d \| e \| f
\meta{explicit sign} \: + \| -
\meta{special subsequent} \: \meta{explicit sign} \| . \| @
\meta{inline hex escape} \: \backwhack{}x\meta{hex scalar value};
\meta{hex scalar value} \: \atleastone{\meta{hex digit}}
\meta{mnemonic escape} \: \backwhack{}a \| \backwhack{}b \| \backwhack{}t \| \backwhack{}n \| \backwhack{}r
\meta{peculiar identifier} \: \meta{explicit sign}
 \> \| \meta{explicit sign} \meta{sign subsequent} \arbno{\meta{subsequent}}
 \> \| \meta{explicit sign} . \meta{dot subsequent} \arbno{\meta{subsequent}}
 \> \| . \meta{dot subsequent} \arbno{\meta{subsequent}}
 %\| 1+ \| -1+
\meta{dot subsequent} \: \meta{sign subsequent} \| .
\meta{sign subsequent} \: \meta{initial} \| \meta{explicit sign} \| @
\meta{symbol element} \:
 \> \meta{any character other than \meta{vertical line} or \backwhack}
 \> \| \meta{inline hex escape} \| \meta{mnemonic escape} \| \backwhack{}|

\meta{boolean} \: \schtrue{} \| \schfalse{} \| \sharptrue{} \| \sharpfalse{}
\label{charactersyntax}
\meta{character} \: \#\backwhack{} \meta{any character}
 \>  \| \#\backwhack{} \meta{character name}
 \>  \| \#\backwhack{}x\meta{hex scalar value}
\meta{character name} \: alarm \| backspace \| delete
\> \| escape \| newline \| null \| return \| space \| tab
\todo{Explain what happens in the ambiguous case.}
\meta{string} \: " \arbno{\meta{string element}} "
\meta{string element} \: \meta{any character other than \doublequote{} or \backwhack}
 \> \| \meta{mnemonic escape} \| \backwhack\doublequote{} \| \backwhack\backwhack
 \>  \| \backwhack{}\arbno{\meta{intraline whitespace}}\meta{line ending}
 \>  \> \arbno{\meta{intraline whitespace}}
 \>  \| \meta{inline hex escape}
\meta{bytevector} \: \#u8(\arbno{\meta{byte}})
\meta{byte} \: \meta{any exact integer between 0 and 255}%
\end{grammar}


\label{numbersyntax}

\begin{grammar}%
\meta{number} \: \meta{num $2$} \| \meta{num $8$}
   \>  \| \meta{num $10$} \| \meta{num $16$}
\end{grammar}

The following rules for \meta{num $R$}, \meta{complex $R$}, \meta{real
$R$}, \meta{ureal $R$}, \meta{uinteger $R$}, and \meta{prefix $R$}
are implicitly replicated for \hbox{$R = 2, 8, 10,$}
and $16$.  There are no rules for \meta{decimal $2$}, \meta{decimal
$8$}, and \meta{decimal $16$}, which means that numbers containing
decimal points or exponents are always in decimal radix.
Although not shown below, all alphabetic characters used in the grammar
of numbers can appear in either upper or lower case.
\begin{grammar}%
\meta{num $R$} \: \meta{prefix $R$} \meta{complex $R$}
\meta{complex $R$} \: %
         \meta{real $R$} %
      \| \meta{real $R$} @ \meta{real $R$}
   \> \| \meta{real $R$} + \meta{ureal $R$} i %
      \| \meta{real $R$} - \meta{ureal $R$} i
   \> \| \meta{real $R$} + i %
      \| \meta{real $R$} - i %
      \| \meta{real $R$} \meta{infnan} i
   \> \| + \meta{ureal $R$} i %
      \| - \meta{ureal $R$} i
   \> \| \meta{infnan} i %
      \| + i %
      \| - i
\meta{real $R$} \: \meta{sign} \meta{ureal $R$}
   \> \| \meta{infnan}
\meta{ureal $R$} \: %
         \meta{uinteger $R$}
   \> \| \meta{uinteger $R$} / \meta{uinteger $R$}
   \> \| \meta{decimal $R$}
\meta{decimal $10$} \: %
         \meta{uinteger $10$} \meta{suffix}
   \> \| . \atleastone{\meta{digit $10$}} \meta{suffix}
   \> \| \atleastone{\meta{digit $10$}} . \arbno{\meta{digit $10$}} \meta{suffix}
\meta{uinteger $R$} \: \atleastone{\meta{digit $R$}}
\meta{prefix $R$} \: %
         \meta{radix $R$} \meta{exactness}
   \> \| \meta{exactness} \meta{radix $R$}
\meta{infnan} \: +inf.0 \| -inf.0 \| +nan.0 \| -nan.0
\end{grammar}

\begin{grammar}%
\meta{suffix} \: \meta{empty}
   \> \| \meta{exponent marker} \meta{sign} \atleastone{\meta{digit $10$}}
\meta{exponent marker} \: e
\meta{sign} \: \meta{empty}  \| + \|  -
\meta{exactness} \: \meta{empty} \| \#i\sharpindex{i} \| \#e\sharpindex{e}
\meta{radix 2} \: \#b\sharpindex{b}
\meta{radix 8} \: \#o\sharpindex{o}
\meta{radix 10} \: \meta{empty} \| \#d
\meta{radix 16} \: \#x\sharpindex{x}
\meta{digit 2} \: 0 \| 1
\meta{digit 8} \: 0 \| 1 \| 2 \| 3 \| 4 \| 5 \| 6 \| 7
\meta{digit 10} \: \meta{digit}
\meta{digit 16} \: \meta{digit $10$} \| a \| b \| c \| d \| e \| f %
\end{grammar}


### 7.1.2. External representations
\label{datumsyntax}

\meta{Datum} is what the \ide{read} procedure (section~\ref{read})
successfully parses.  Note that any string that parses as an
\meta{ex\-pres\-sion} will also parse as a \meta{datum}.  \label{datum}

\begin{grammar}%
\meta{datum} \: \meta{simple datum} \| \meta{compound datum}
\>  \| \meta{label} = \meta{datum} \| \meta{label} \#
\meta{simple datum} \: \meta{boolean} \| \meta{number}
\>  \| \meta{character} \| \meta{string} \|  \meta{symbol} \| \meta{bytevector}
\meta{symbol} \: \meta{identifier}
\meta{compound datum} \: \meta{list} \| \meta{vector} \| \meta{abbreviation}
\meta{list} \: (\arbno{\meta{datum}}) \| (\atleastone{\meta{datum}} .\ \meta{datum})
\meta{abbreviation} \: \meta{abbrev prefix} \meta{datum}
\meta{abbrev prefix} \: ' \| ` \| , \| ,@
\meta{vector} \: \#(\arbno{\meta{datum}})
\meta{label} \: \# \meta{uinteger 10}%
\end{grammar}


### 7.1.3. Expressions

The definitions in this and the following subsections assume that all
the syntax keywords defined in this report have been properly imported
from their libraries, and that none of them have been redefined or shadowed.

\begin{grammar}%
\meta{expression} \: \meta{identifier}
\>  \| \meta{literal}
\>  \| \meta{procedure call}
\>  \| \meta{lambda expression}
\>  \| \meta{conditional}
\>  \| \meta{assignment}
\>  \| \meta{derived expression}
\>  \| \meta{macro use}
\>  \| \meta{macro block}
\>  \| \meta{includer}

\meta{literal} \: \meta{quotation} \| \meta{self-evaluating}
\meta{self-evaluating} \: \meta{boolean} \| \meta{number} \| \meta{vector}
\>  \| \meta{character} \| \meta{string} \| \meta{bytevector}
\meta{quotation} \: '\meta{datum} \| (quote \meta{datum})
\meta{procedure call} \: (\meta{operator} \arbno{\meta{operand}})
\meta{operator} \: \meta{expression}
\meta{operand} \: \meta{expression}

\meta{lambda expression} \: (lambda \meta{formals} \meta{body})
\meta{formals} \: (\arbno{\meta{identifier}}) \| \meta{identifier}
\>  \| (\atleastone{\meta{identifier}} .\ \meta{identifier})
\meta{body} \:  \arbno{\meta{definition}} \meta{sequence}
\meta{sequence} \: \arbno{\meta{command}} \meta{expression}
\meta{command} \: \meta{expression}

\meta{conditional} \: (if \meta{test} \meta{consequent} \meta{alternate})
\meta{test} \: \meta{expression}
\meta{consequent} \: \meta{expression}
\meta{alternate} \: \meta{expression} \| \meta{empty}

\meta{assignment} \: (set! \meta{identifier} \meta{expression})

\meta{derived expression} \:
\>  \> (cond \atleastone{\meta{cond clause}})
\>  \| (cond \arbno{\meta{cond clause}} (else \meta{sequence}))
\>  \| (c\=ase \meta{expression}
\>       \>\atleastone{\meta{case clause}})
\>  \| (c\=ase \meta{expression}
\>       \>\arbno{\meta{case clause}}
\>       \>(else \meta{sequence}))
\>  \| (c\=ase \meta{expression}
\>       \>\arbno{\meta{case clause}}
\>       \>(else => \meta{recipient}))
\>  \| (and \arbno{\meta{test}})
\>  \| (or \arbno{\meta{test}})
\>  \| (when \meta{test} \meta{sequence})
\>  \| (unless \meta{test} \meta{sequence})
\>  \| (let (\arbno{\meta{binding spec}}) \meta{body})
\>  \| (let \meta{identifier} (\arbno{\meta{binding spec}}) \meta{body})
\>  \| (let* (\arbno{\meta{binding spec}}) \meta{body})
\>  \| (letrec (\arbno{\meta{binding spec}}) \meta{body})
\>  \| (letrec* (\arbno{\meta{binding spec}}) \meta{body})
\>  \| (let-values (\arbno{\meta{mv binding spec}}) \meta{body})
\>  \| (let*-values (\arbno{\meta{mv binding spec}}) \meta{body})
\>  \| (begin \meta{sequence})
\>  \| (d\=o \=(\arbno{\meta{iteration spec}})
\>       \>  \>(\meta{test} \meta{do result})
\>       \>\arbno{\meta{command}})
\>  \| (delay \meta{expression})
\>  \| (delay-force \meta{expression})
\>  \| (p\=arameterize (\arbno{(\meta{expression} \meta{expression})})
\>       \> \meta{body})
\>  \| (guard (\meta{identifier} \arbno{\meta{cond clause}}) \meta{body})
\>  \| \meta{quasiquotation}
\>  \| (c\=ase-lambda \arbno{\meta{case-lambda clause}})

\meta{cond clause} \: (\meta{test} \meta{sequence})
\>   \| (\meta{test})
\>   \| (\meta{test} => \meta{recipient})
\meta{recipient} \: \meta{expression}
\meta{case clause} \: ((\arbno{\meta{datum}}) \meta{sequence})
\>   \| ((\arbno{\meta{datum}}) => \meta{recipient})
\meta{binding spec} \: (\meta{identifier} \meta{expression})
\meta{mv binding spec} \: (\meta{formals} \meta{expression})
\meta{iteration spec} \: (\meta{identifier} \meta{init} \meta{step})
\> \| (\meta{identifier} \meta{init})
\meta{case-lambda clause} \: (\meta{formals} \meta{body})
\meta{init} \: \meta{expression}
\meta{step} \: \meta{expression}
\meta{do result} \: \meta{sequence} \| \meta{empty}

\meta{macro use} \: (\meta{keyword} \arbno{\meta{datum}})
\meta{keyword} \: \meta{identifier}

\meta{macro block} \:
\>  (let-syntax (\arbno{\meta{syntax spec}}) \meta{body})
\>  \| (letrec-syntax (\arbno{\meta{syntax spec}}) \meta{body})
\meta{syntax spec} \: (\meta{keyword} \meta{transformer spec})

\meta{includer} \:
\> \| (include \atleastone{\meta{string}})
\> \| (include-ci \atleastone{\meta{string}})
\end{grammar}

### 7.1.4. Quasiquotations

The following grammar for quasiquote expressions is not context-free.
It is presented as a recipe for generating an infinite number of
production rules.  Imagine a copy of the following rules for $D = 1, 2,
3, \ldots$, where $D$ is the nesting depth.

\begin{grammar}%
\meta{quasiquotation} \: \meta{quasiquotation 1}
\meta{qq template 0} \: \meta{expression}
\meta{quasiquotation $D$} \: `\meta{qq template $D$}
\>    \| (quasiquote \meta{qq template $D$})
\meta{qq template $D$} \: \meta{simple datum}
\>    \| \meta{list qq template $D$}
\>    \| \meta{vector qq template $D$}
\>    \| \meta{unquotation $D$}
\meta{list qq template $D$} \: (\arbno{\meta{qq template or splice $D$}})
\>    \| (\atleastone{\meta{qq template or splice $D$}} .\ \meta{qq template $D$})
\>    \| '\meta{qq template $D$}
\>    \| \meta{quasiquotation $D+1$}
\meta{vector qq template $D$} \: \#(\arbno{\meta{qq template or splice $D$}})
\meta{unquotation $D$} \: ,\meta{qq template $D-1$}
\>    \| (unquote \meta{qq template $D-1$})
\meta{qq template or splice $D$} \: \meta{qq template $D$}
\>    \| \meta{splicing unquotation $D$}
\meta{splicing unquotation $D$} \: ,@\meta{qq template $D-1$}
\>    \| (unquote-splicing \meta{qq template $D-1$}) %
\end{grammar}

In \meta{quasiquotation}s, a \meta{list qq template $D$} can sometimes
be confused with either an \meta{un\-quota\-tion $D$} or a \meta{splicing
un\-quo\-ta\-tion $D$}.  The interpretation as an
\meta{un\-quo\-ta\-tion} or \meta{splicing
un\-quo\-ta\-tion $D$} takes precedence.

### 7.1.5. Transformers

\begin{grammar}%
\meta{transformer spec} \:
\> (syntax-rules (\arbno{\meta{identifier}}) \arbno{\meta{syntax rule}})
\> \| (syntax-rules \meta{identifier} (\arbno{\meta{identifier}})
\> \> \ \ \arbno{\meta{syntax rule}})
\meta{syntax rule} \: (\meta{pattern} \meta{template})
\meta{pattern} \: \meta{pattern identifier}
\>  \| \meta{underscore}
\>  \| (\arbno{\meta{pattern}})
\>  \| (\atleastone{\meta{pattern}} . \meta{pattern})
\>  \| (\arbno{\meta{pattern}} \meta{pattern} \meta{ellipsis} \arbno{\meta{pattern}})
\>  \| (\arbno{\meta{pattern}} \meta{pattern} \meta{ellipsis} \arbno{\meta{pattern}}
\> \> \ \ . \meta{pattern})
\>  \| \#(\arbno{\meta{pattern}})
\>  \| \#(\arbno{\meta{pattern}} \meta{pattern} \meta{ellipsis} \arbno{\meta{pattern}})
\>  \| \meta{pattern datum}
\meta{pattern datum} \: \meta{string}
\>  \| \meta{character}
\>  \| \meta{boolean}
\>  \| \meta{number}
\meta{template} \: \meta{pattern identifier}
\>  \| (\arbno{\meta{template element}})
\>  \| (\atleastone{\meta{template element}} .\ \meta{template})
\>  \| \#(\arbno{\meta{template element}})
\>  \| \meta{template datum}
\meta{template element} \: \meta{template}
\>  \| \meta{template} \meta{ellipsis}
\meta{template datum} \: \meta{pattern datum}
\meta{pattern identifier} \: \meta{any identifier except {\cf ...}}
\meta{ellipsis} \: \meta{an identifier defaulting to {\cf ...}}
\meta{underscore} \: \meta{the identifier {\cf \_}}
\end{grammar}

### 7.1.6. Programs and definitions

\begin{grammar}%
\meta{program} \:
\> \atleastone{\meta{import declaration}}
\> \atleastone{\meta{command or definition}}
\meta{command or definition} \: \meta{command}
\> \| \meta{definition}
\> \| (begin \atleastone{\meta{command or definition}})
\meta{definition} \: (define \meta{identifier} \meta{expression})
\>   \| (define (\meta{identifier} \meta{def formals}) \meta{body})
\>   \| \meta{syntax definition}
\>   \| (define-values \meta{formals} \meta{body})
\>   \| (define-record-type \meta{identifier}
\> \> \ \ \meta{constructor} \meta{identifier} \arbno{\meta{field spec}})
\>   \| (begin \arbno{\meta{definition}})
\meta{def formals} \: \arbno{\meta{identifier}}
\>   \| \arbno{\meta{identifier}} .\ \meta{identifier}
\meta{constructor} \: (\meta{identifier} \arbno{\meta{field name}})
\meta{field spec} \: (\meta{field name} \meta{accessor})
\>   \| (\meta{field name} \meta{accessor} \meta{mutator})
\meta{field name} \: \meta{identifier}
\meta{accessor} \: \meta{identifier}
\meta{mutator} \: \meta{identifier}
\meta{syntax definition} \:
\>  (define-syntax \meta{keyword} \meta{transformer spec})
\end{grammar}

### 7.1.7. Libraries

\begin{grammar}%
\meta{library} \:
\> (d\=efine-library \meta{library name}
\>   \> \arbno{\meta{library declaration}})
\meta{library name} \: (\atleastone{\meta{library name part}})
\meta{library name part} \: \meta{identifier} \| \meta{uinteger 10}
\meta{library declaration} \: (export \arbno{\meta{export spec}})
\> \| \meta{import declaration}
\> \| (begin \arbno{\meta{command or definition}})
\> \| \meta{includer}
\> \| (include-library-declarations \atleastone{\meta{string}})
\> \| (cond-expand \atleastone{\meta{cond-expand clause}})
\> \| (cond-expand \atleastone{\meta{cond-expand clause}}
\hbox to 1\wd0{\hfill}\ (else \arbno{\meta{library declaration}}))
\meta{import declaration} \: (import \atleastone{\meta{import set}})
\meta{export spec} \: \meta{identifier}
\> \| (rename \meta{identifier} \meta{identifier})
\meta{import set} \: \meta{library name}
\> \| (only \meta{import set} \atleastone{\meta{identifier}})
\> \| (except \meta{import set} \atleastone{\meta{identifier}})
\> \| (prefix \meta{import set} \meta{identifier})
\> \| (rename \meta{import set} \atleastone{(\meta{identifier} \meta{identifier})})
\meta{cond-expand clause} \:
\> (\meta{feature requirement} \arbno{\meta{library declaration}})
\meta{feature requirement} \: \meta{identifier}
\> \| \meta{library name}
\> \| (and \arbno{\meta{feature requirement}})
\> \| (or \arbno{\meta{feature requirement}})
\> \| (not \meta{feature requirement})
\end{grammar}
%\vfill\eject
## 7.2. Formal semantics
\label{formalsemanticssection}

\bgroup

\newcommand{\sembrack}[1]{[\![#1]\!]}
\newcommand{\fun}[1]{\hbox{\it #1}}
\newenvironment{semfun}{\begin{tabbing}$}{$\end{tabbing}}
\newcommand\LOC{{\tt{}L}}
\newcommand\NAT{{\tt{}N}}
\newcommand\TRU{{\tt{}T}}
\newcommand\SYM{{\tt{}Q}}
\newcommand\CHR{{\tt{}H}}
\newcommand\NUM{{\tt{}R}}
\newcommand\FUN{{\tt{}F}}
\newcommand\EXP{{\tt{}E}}
\newcommand\STV{{\tt{}E}}
\newcommand\STO{{\tt{}S}}
\newcommand\ENV{{\tt{}U}}
\newcommand\ANS{{\tt{}A}}
\newcommand\ERR{{\tt{}X}}
\newcommand\DP{\tt{P}}
\newcommand\EC{{\tt{}K}}
\newcommand\CC{{\tt{}C}}
\newcommand\MSC{{\tt{}M}}
\newcommand\PAI{\hbox{\EXP$_{\rm p}$}}
\newcommand\VEC{\hbox{\EXP$_{\rm v}$}}
\newcommand\STR{\hbox{\EXP$_{\rm s}$}}

\newcommand\elt{\downarrow}
\newcommand\drop{\dagger}

\newcommand{\wrong}[1]{\fun{wrong }\hbox{\rm``#1''}}
\newcommand{\go}[1]{\hbox{\hspace*{#1em}}}

This section provides a formal denotational semantics for the primitive
expressions of Scheme and selected built-in procedures.  The concepts
and notation used here are described in~\cite{Stoy77}; the definition of
{\cf dynamic-wind} is taken from~\cite{GasbichlerKnauelSperberKelsey2003}.
The notation is summarized below:

\begin{tabular}{ll}
$\langle\,\ldots\,\rangle$ & sequence formation \\
$s \elt k$                 & $k$th member of the sequence $s$ (1-based) \\
$\#s$                      & length of sequence $s$ \\
$s \:\S\: t$               & concatenation of sequences $s$ and $t$ \\
$s \drop k$                & drop the first $k$ members of sequence $s$ \\
$t \rightarrow a, b$       & McCarthy conditional ``if $t$ then $a$ else $b$'' \\
$\rho[x/i]$                & substitution ``$\rho$ with $x$ for $i$'' \\
$x\hbox{ \rm in }{\texttt{D}}$         & injection of $x$ into domain $\texttt{D}$ \\
$x\,\vert\,\texttt{D}$       & projection of $x$ to domain $\texttt{D}$
\end{tabular}

The reason that expression continuations take sequences of values instead
of single values is to simplify the formal treatment of procedure calls
and multiple return values.

The boolean flag associated with pairs, vectors, and strings will be true
for mutable objects and false for immutable objects.

The order of evaluation within a call is unspecified.  We mimic that
here by applying arbitrary permutations {\it permute} and {\it
unpermute}, which must be inverses, to the arguments in a call before
and after they are evaluated.  This is not quite right since it suggests,
incorrectly, that the order of evaluation is constant throughout a program (for
any given number of arguments), but it is a closer approximation to the intended
semantics than a left-to-right evaluation would be.

The storage allocator {\it new} is implementation-dependent, but it must
obey the following axiom:  if \hbox{$\fun{new}\:\sigma\:\elem\:\LOC$}, then
$\sigma\:(\fun{new}\:\sigma\:\vert\:\LOC)\elt 2 = {\it false}$.

\def\P{\hbox{\rm P}}
\def\I{\hbox{\rm I}}
\def\Ksem{\hbox{$\cal K$}}
\def\Esem{\hbox{$\cal E$}}

The definition of $\Ksem$ is omitted because an accurate definition of
$\Ksem$ would complicate the semantics without being very interesting.

If \P{} is a program in which all variables are defined before being
referenced or assigned, then the meaning of \P{} is
$$\Esem\sembrack{\hbox{\texttt{((lambda (\arbno{\I}) \P')
\hyper{undefined} \dotsfoo)}}}$$
where \arbno{\I} is the sequence of variables defined in \P, $\P'$
is the sequence of expressions obtained by replacing every definition
in \P{} by an assignment, \hyper{undefined} is an expression that evaluates
to \fun{undefined}, and
$\Esem$ is the semantic function that assigns meaning to expressions.

%The semantics in this section was translated by machine from an
%executable version of the semantics written in Scheme itself.
%[This was once true, but then I modified the semantics without
%going back to the executable version.  -- Will]


### 7.2.8. Abstract syntax

\def\K{\hbox{\rm K}}
\def\I{\hbox{\rm I}}
\def\E{\hbox{\rm E}}
\def\C{\hbox{$\Gamma$}}
\def\Con{\hbox{\rm Con}}
\def\Ide{\hbox{\rm Ide}}
\def\Exp{\hbox{\rm Exp}}
\def\Com{\hbox{\rm Com}}
\def\|{$\vert$}

\begin{tabular}{r@{ }c@{ }l@{\qquad}l}
\K & \elem & \Con & constants, including quotations \\
\I & \elem & \Ide & identifiers (variables) \\
\E & \elem & \Exp & expressions\\
\C & \elem & \Com{} $=$ \Exp & commands
\end{tabular}

\setbox0=\hbox{\texttt{\Exp \goesto{}}}  %\tt for spacing
\setbox1=\hbox to 1\wd0{\hfil \|}
\begin{grammar}
\Exp{} \goesto{} \K{} \| \I{} \| (\E$_0$ \arbno{\E})
 \copy1{} (lambda (\arbno{\I}) \arbno{\C} \E$_0$)
 \copy1{} (lambda (\arbno{\I} {\bf.}\ \I) \arbno{\C} \E$_0$)
 \copy1{} (lambda \I{} \arbno{\C} \E$_0$)
 \copy1{} (if \E$_0$ \E$_1$ \E$_2$) \| (if \E$_0$ \E$_1$)
 \copy1{} (set! \I{} \E)
\end{grammar}

### 7.2.9. Domain equations

\begin{tabular}{@{}r@{ }c@{ }l@{ }l@{ }ll}
$\alpha$   & \elem & \LOC & &          & locations \\
$\nu$      & \elem & \NAT & &          & natural numbers \\
           &       & \TRU &=& $\{$\it false, true$\}$ & booleans \\
           &       & \SYM & &          & symbols \\
           &       & \CHR & &          & characters \\
           &       & \NUM & &          & numbers \\
           &       & \PAI &=& $\LOC \times \LOC \times \TRU$  & pairs \\
           &       & \VEC &=& $\arbno{\LOC} \times \TRU$ & vectors \\
           &       & \STR &=& $\arbno{\LOC} \times \TRU$ & strings \\
           &       & \MSC &=& \makebox[0pt][l]{$\{$\it false, true,
                                null, undefined, unspecified$\}$} \\
           &       &      & &          & miscellaneous \\
$\phi$     & \elem & \FUN &=& $\LOC\times(\arbno{\EXP} \to \DP \to \EC \to \CC)$
                                       & procedure values \\
$\epsilon$ & \elem & \EXP &=& \makebox[0pt][l]{$\SYM+\CHR+\NUM+\PAI+\VEC+\STR+\MSC+\FUN$}\\
           &       &      & &          & expressed values \\
%          &       & \STV &=& \EXP     & stored values \\
$\sigma$   & \elem & \STO &=& $\LOC\to(\STV\times\TRU)$ & stores \\
$\rho$     & \elem & \ENV &=& $\Ide\to\LOC$  & environments \\
$\theta$   & \elem & \CC  &=& $\STO\to\ANS$  & command conts \\
$\kappa$   & \elem & \EC  &=& $\arbno{\EXP}\to\CC$ & expression conts \\
           &       & \ANS & &                & answers \\
           &       & \ERR & &                & errors \\
$\omega$   & \elem & \DP  &=& $(\FUN \times \FUN \times \DP) + \{\textit{root}\}$ & dynamic points\\
\end{tabular}

### 7.2.10. Semantic functions

\def\Ksem{\hbox{$\cal K$}}
\def\Esem{\hbox{$\cal E$}}
\def\Csem{\hbox{$\cal C$}}

\begin{tabular}{@{}r@{ }l}
  $\Ksem:$ & $\Con\to\EXP$  \\
  $\Esem:$ & $\Exp\to\ENV\to\DP\to\EC\to\CC$ \\
$\arbno{\Esem}:$ & $\arbno{\Exp}\to\ENV\to\DP\to\EC\to\CC$ \\
  $\Csem:$ & $\arbno{\Com}\to\ENV\to\DP\to\CC\to\CC$
\end{tabular}

\bgroup\small

\vspace{1ex}

Definition of \Ksem{} deliberately omitted.

\begin{semfun}
\Esem\sembrack{\K} =
  \lambda\rho\omega\kappa\:.\:\fun{send}\,(\Ksem\sembrack{\K})\,\kappa
\end{semfun}

\begin{semfun}
\Esem\sembrack{\I} =
  \lambda\rho\omega\kappa\:.\:\fun{hold}\:
    $\=$(\fun{lookup}\:\rho\:\I)$\\
     \>$(\fun{single}(\lambda\epsilon\:.\:
        $\=$\epsilon = \fun{undefined}\rightarrow$\\
     \>  \> \go{2}$\wrong{undefined variable},$\\
     \>  \>\go{1}$\fun{send}\:\epsilon\:\kappa))
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{($\E_0$ \arbno{\E})}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:\arbno{\Esem}
    $\=$(\fun{permute}(\langle\E_0\rangle\:\S\:\arbno{\E}))$\\
     \>$\rho\:$\\
     \>$\omega\:$\\
     \>$(\lambda\arbno{\epsilon}\:.\:
        ($\=$(\lambda\arbno{\epsilon}\:.\:
                 \fun{applicate}\:(\arbno{\epsilon}\elt 1)
                                \:(\arbno{\epsilon}\drop 1)
                                \:\omega\kappa)$\\
     \>   \>$(\fun{unpermute}\:\arbno{\epsilon})))
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(\ide{lambda} (\arbno{\I}) \arbno{\C} $\E_0$)}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:\lambda\sigma\:.\:$\\
  \go{2}$\fun{new}\:\sigma\:\elem\:\LOC\rightarrow$\\
   \go{3}$\fun{send}\:
     $\=$(\langle
         $\=$\fun{new}\:\sigma\,\vert\,\LOC,$\\
      \>  \>$\lambda\arbno{\epsilon}\omega^\prime\kappa^\prime\:.\:
               $\=$\#\arbno{\epsilon} = \#{\arbno{\I}}\rightarrow$\\
      \>  \>    $\go{1}\fun{tievals}
                   $\=$(\lambda\arbno{\alpha}\:.\:
                         $\=$(\lambda\rho^\prime\:.\:\Csem\sembrack{\arbno{\C}}\rho^\prime\omega^\prime
                              (\Esem\sembrack{\E_0}\rho^\prime\omega^\prime\kappa^\prime))$\\
      \>  \>      \>    \>$(\fun{extends}\:\rho\:{\arbno{\I}}\:\arbno{\alpha}))$\\
      \>  \>      \>$\arbno{\epsilon},$\\
      \>  \>    \go{1}$\wrong{wrong number of arguments}\rangle$\\
      \>  \>$\hbox{ \rm in }\EXP)$\\
      \>$\kappa$\\
      \>$(\fun{update}\:(\fun{new}\:\sigma\,\vert\,\LOC)
                           \:\fun{unspecified}
                           \:\sigma),$\\
  \go{3}$\wrong{out of memory}\:\sigma
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(lambda (\arbno{\I} {\bf.}\ \I) \arbno{\C} $\E_0$)}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:\lambda\sigma\:.\:$\\
  \go{2}$\fun{new}\:\sigma\:\elem\:\LOC\rightarrow$\\
   \go{3}$\fun{send}\:
     $\=$(\langle
         $\=$\fun{new}\:\sigma\,\vert\,\LOC,$\\
      \>  \>$\lambda\arbno{\epsilon}\omega^\prime\kappa^\prime\:.\:
               $\=$\#\arbno{\epsilon} \geq \#\arbno{\I}\rightarrow$\\
      \>  \>    \>\go{1}$\fun{tievalsrest}$\\
      \>  \>    \>\go{2}\=$(\lambda\arbno{\alpha}\:.\:
                           $\=$(\lambda\rho^\prime\:.\:\Csem\sembrack{\arbno{\C}}\rho^\prime\omega^\prime
                               (\Esem\sembrack{\E_0}\rho^\prime\omega^\prime\kappa^\prime))$\\
      \>  \>    \>       \> \>$(\fun{extends}\:\rho
                               \:(\arbno{\I}\:\S\:\langle\I\rangle)
                               \:\arbno{\alpha}))$\\
      \>  \>    \>       \>$\arbno{\epsilon}$\\
      \>  \>    \>       \>$(\#\arbno{\I}),$\\
      \>  \>    \>\go{1}$\wrong{too few arguments}\rangle\hbox{ \rm in }\EXP)$\\
      \>$\kappa$\\
      \>$(\fun{update}\:(\fun{new}\:\sigma\,\vert\,\LOC)
                           \:\fun{unspecified}
                           \:\sigma),$\\
  \go{3}$\wrong{out of memory}\:\sigma
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(lambda \I{} \arbno{\C} $\E_0$)}}} =
 \Esem\sembrack{\hbox{\texttt{(lambda ({\bf.}\ \I) \arbno{\C} $\E_0$)}}}
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(\ide{if} $\E_0$ $\E_1$ $\E_2$)}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:
   \Esem\sembrack{\E_0}\:\rho\omega\:(\fun{single}\:(\lambda\epsilon\:.\:
    $\=$\fun{truish}\:\epsilon\rightarrow\Esem\sembrack{\E_1}\rho\omega\kappa,$\\
     \>\go{1}$\Esem\sembrack{\E_2}\rho\omega\kappa))
\end{semfun}

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(if $\E_0$ $\E_1$)}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:
   \Esem\sembrack{\E_0}\:\rho\omega\:(\fun{single}\:(\lambda\epsilon\:.\:
    $\=$\fun{truish}\:\epsilon\rightarrow\Esem\sembrack{\E_1}\rho\omega\kappa,$\\
     \>\go{1}$\fun{send}\:\fun{unspecified}\:\kappa))
\end{semfun}

Here and elsewhere, any expressed value other than {\it undefined} may
be used in place of {\it unspecified}.

\begin{semfun}
\Esem\sembrack{\hbox{\texttt{(\ide{set!} \I{} \E)}}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:\Esem\sembrack{\E}\:\rho\:\omega\:
     (\fun{single}(\lambda\epsilon\:.\:\fun{assign}\:
       $\=$(\fun{lookup}\:\rho\:\I)$\\
        \>$\epsilon$\\
        \>$(\fun{send}\:\fun{unspecified}\:\kappa)))
\end{semfun}

\begin{semfun}
\arbno{\Esem}\sembrack{\:} =
  \lambda\rho\omega\kappa\:.\:\kappa\langle\:\rangle
\end{semfun}

\begin{semfun}
\arbno{\Esem}\sembrack{\E_0\:\arbno{\E}} =$\\
 \go{1}$\lambda\rho\omega\kappa\:.\:
      \Esem\sembrack{\E_0}\:\rho\omega\:
         (\fun{single}
            (\lambda\epsilon_0\:.\:\arbno{\Esem}\sembrack{\arbno{\E}}
                \:\rho\omega\:(\lambda\arbno{\epsilon}\:.\:
                           \kappa\:(\langle\epsilon_0\rangle\:\S\:\arbno{\epsilon}))))
\end{semfun}

\begin{semfun}
\Csem\sembrack{\:} = \lambda\rho\omega\theta\,.\:\theta
\end{semfun}

\begin{semfun}
\Csem\sembrack{\C_0\:\arbno{\C}} =
  \lambda\rho\omega\theta\:.\:\Esem\sembrack{\C_0}\:\rho\omega\:(\lambda\arbno{\epsilon}\:.\:
   \Csem\sembrack{\arbno{\C}}\rho\omega\theta)
\end{semfun}

\egroup  % end smallish

### 7.2.11. Auxiliary functions

\bgroup\small

\begin{semfun}
\fun{lookup}        :  \ENV \to \Ide \to \LOC$\\$
\fun{lookup} =
 \lambda\rho\I\:.\:\rho\I
\end{semfun}

\begin{semfun}
\fun{extends}       :  \ENV \to \arbno{\Ide} \to \arbno{\LOC} \to \ENV$\\$
\fun{extends} =$\\
 \go{1}$\lambda\rho\arbno{\I}\arbno{\alpha}\:.\:
   $\=$\#\arbno{\I}=0\rightarrow\rho,$\\
    \>$\go{1}\fun{extends}\:(\rho[(\arbno{\alpha}\elt 1)/(\arbno{\I}\elt 1)])
                               \:(\arbno{\I}\drop 1)
                               \:(\arbno{\alpha}\drop 1)
\end{semfun}

\begin{semfun}
\fun{wrong}  :  \ERR \to \CC    \hbox{\qquad [implementation-dependent]}
\end{semfun}

\begin{semfun}
\fun{send}          :  \EXP \to \EC \to \CC$\\$
\fun{send} =
 \lambda\epsilon\kappa\:.\:\kappa\langle\epsilon\rangle
\end{semfun}

\begin{semfun}
\fun{single}        :  (\EXP \to \CC) \to \EC$\\$
\fun{single} =$\\
 \go{1}$\lambda\psi\arbno{\epsilon}\:.\:
   $\=$\#\arbno{\epsilon}=1\rightarrow\psi(\arbno{\epsilon}\elt 1),$\\
    \>$\go{1}\wrong{wrong number of return values}
\end{semfun}

\begin{semfun}
\fun{new}           :  \STO \to (\LOC + \{ \fun{error} \})
    \hbox{\qquad [implementation-dependent]}
\end{semfun}

\begin{semfun}
\fun{hold}          :  \LOC \to \EC \to \CC$\\$
\fun{hold} =
 \lambda\alpha\kappa\sigma\:.\:\fun{send}\,(\sigma\alpha\elt 1)\kappa\sigma
\end{semfun}

\begin{semfun}
\fun{assign}        :  \LOC \to \EXP \to \CC \to \CC$\\$
\fun{assign} =
 \lambda\alpha\epsilon\theta\sigma\:.\:\theta(\fun{update}\:\alpha\epsilon\sigma)
\end{semfun}

\begin{semfun}
\fun{update}        :  \LOC \to \EXP \to \STO \to \STO$\\$
\fun{update} =
 \lambda\alpha\epsilon\sigma\:.\:\sigma[\langle\epsilon,\fun{true}\rangle/\alpha]
\end{semfun}

\begin{semfun}
\fun{tievals}       :  (\arbno{\LOC} \to \CC) \to \arbno{\EXP} \to \CC$\\$
\fun{tievals} =$\\
 \go{1}$\lambda\psi\arbno{\epsilon}\sigma\:.\:
   $\=$\#\arbno{\epsilon}=0\rightarrow\psi\langle\:\rangle\sigma,$\\
    \>$\fun{new}\:\sigma\:\elem\:\LOC\rightarrow\fun{tievals}\,
       $\=$(\lambda\arbno{\alpha}\:.\:\psi(\langle\fun{new}\:\sigma\:\vert\:\LOC\rangle
                                     \:\S\:\arbno{\alpha}))$\\
    \>  \>$(\arbno{\epsilon}\drop 1)$\\
    \>  \>$(\fun{update}(\fun{new}\:\sigma\:\vert\:\LOC)
                                 (\arbno{\epsilon}\elt 1)
                                 \sigma),$\\
    \>$\go{1}\wrong{out of memory}\sigma
\end{semfun}

\begin{semfun}
\fun{tievalsrest}   :  (\arbno{\LOC} \to \CC) \to \arbno{\EXP} \to \NAT \to \CC$\\$
\fun{tievalsrest} =$\\
 \go{1}$\lambda\psi\arbno{\epsilon}\nu\:.\:\fun{list}\:
   $\=$(\fun{dropfirst}\:\arbno{\epsilon}\nu)$\\
    \>$(\fun{single}(\lambda\epsilon\:.\:\fun{tievals}\:\psi\:
           ((\fun{takefirst}\:\arbno{\epsilon}\nu)\:\S\:\langle\epsilon\rangle)))
\end{semfun}

\begin{semfun}
\fun{dropfirst} =
 \lambda l n \:.\:  n=0 \rightarrow l, \fun{dropfirst}\,(l \drop 1)(n - 1)
\end{semfun}

\begin{semfun}
\fun{takefirst} =
 \lambda l n \:.\: n=0 \rightarrow \langle\:\rangle,
     \langle l \elt 1\rangle\:\S\:(\fun{takefirst}\,(l \drop 1)(n - 1))
\end{semfun}

\begin{semfun}
\fun{truish}        :  \EXP \to \TRU$\\$
\fun{truish} =
  \lambda\epsilon\:.\:
%    (\epsilon = \fun{false}\vee\epsilon = \fun{null})\rightarrow
     \epsilon = \fun{false}\rightarrow
          \fun{false},
          \fun{true}
\end{semfun}

\begin{semfun}
\fun{permute}       :  \arbno{\Exp} \to \arbno{\Exp}
    \hbox{\qquad [implementation-dependent]}
\end{semfun}

\begin{semfun}
\fun{unpermute}     :  \arbno{\EXP} \to \arbno{\EXP}
    \hbox{\qquad [inverse of \fun{permute}]}
\end{semfun}

\begin{semfun}
\fun{applicate}     :  \EXP \to \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{applicate} =$\\
 \go{1}$\lambda\epsilon\arbno{\epsilon}\omega\kappa\:.\:
   $\=$\epsilon\:\elem\:\FUN\rightarrow(\epsilon\:\vert\:\FUN\elt 2)\arbno{\epsilon}\omega\kappa,
          \wrong{bad procedure}
\end{semfun}

\begin{semfun}
\fun{onearg}      :  (\EXP \to \DP \to \EC \to \CC) \to (\arbno{\EXP} \to \DP \to \EC \to \CC)$\\$
\fun{onearg} =$\\
 \go{1}$\lambda\zeta\arbno{\epsilon}\omega\kappa\:.\:
   $\=$\#\arbno{\epsilon}=1\rightarrow\zeta(\arbno{\epsilon}\elt 1)\omega\kappa,$\\
    \>$\go{1}\wrong{wrong number of arguments}
\end{semfun}

\begin{semfun}
\fun{twoarg}      :  (\EXP \to \EXP \to \DP \to \EC \to \CC) \to (\arbno{\EXP} \to \DP \to \EC \to \CC)$\\$
\fun{twoarg} =$\\
 \go{1}$\lambda\zeta\arbno{\epsilon}\omega\kappa\:.\:
   $\=$\#\arbno{\epsilon}=2\rightarrow\zeta(\arbno{\epsilon}\elt 1)(\arbno{\epsilon}\elt 2)\omega\kappa,$\\
    \>$\go{1}\wrong{wrong number of arguments}
\end{semfun}

\begin{semfun}
\fun{threearg}      :  (\EXP \to \EXP \to \EXP \to \DP \to \EC \to \CC) \to (\arbno{\EXP} \to \DP \to \EC \to \CC)$\\$
\fun{threearg} =$\\
 \go{1}$\lambda\zeta\arbno{\epsilon}\omega\kappa\:.\:
   $\=$\#\arbno{\epsilon}=3\rightarrow\zeta(\arbno{\epsilon}\elt 1)(\arbno{\epsilon}\elt 2)(\arbno{\epsilon}\elt 3)\omega\kappa,$\\
    \>$\go{1}\wrong{wrong number of arguments}
\end{semfun}

\begin{semfun}
\fun{list}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{list} =$\\
 \go{1}$\lambda\arbno{\epsilon}\omega\kappa\:.\:
   $\=$\#\arbno{\epsilon}=0\rightarrow\fun{send}\:\fun{null}\:\kappa,$\\
    \>$\go{1}\fun{list}\,(\arbno{\epsilon}\drop 1)
             (\fun{single}(\lambda\epsilon\:.\:
                   \fun{cons}\langle\arbno{\epsilon}\elt 1,\epsilon\rangle\kappa))
\end{semfun}

\begin{semfun}
\fun{cons}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{cons} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\kappa\omega\sigma\:.\:
   $\=$\fun{new}\:\sigma\:\elem\:\LOC\rightarrow$\\
    \>
        \=$(\lambda\sigma^\prime\:.\:
           $\=$\fun{new}\:\sigma^\prime\:\elem\:\LOC\rightarrow$\\
    \>  \>$\go{1}\fun{send}\,
               $\=$($\=$\langle\fun{new}\:\sigma\:\vert\:\LOC,
                                            \fun{new}\:\sigma^\prime\:\vert\:\LOC,
         \fun{true}\rangle$\\
                                \>  \>  \>  \>$\hbox{ \rm in }\EXP)$\\
    \>  \>  \>$\kappa$\\
    \>  \>  \>$(\fun{update}(\fun{new}\:\sigma^\prime\:\vert\:\LOC)
                                     \epsilon_2
                                     \sigma^\prime),$\\
    \>  \>$\go{1}\wrong{out of memory}\sigma^\prime)$\\
    \>  $(\fun{update}(\fun{new}\:\sigma\:\vert\:\LOC)\epsilon_1\sigma),$\\
    \>$\wrong{out of memory}\sigma)
\end{semfun}

\schindex{<}
\begin{semfun}
\fun{less}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{less} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$(\epsilon_1\:\elem\:\NUM\wedge\epsilon_2\:\elem\:\NUM)\rightarrow$\\
    \>$\go{1}\fun{send}\,
               (\epsilon_1\:\vert\:\NUM<\epsilon_2\:\vert\:\NUM\rightarrow
                   \fun{true},
                   \fun{false})
               \kappa,$\\
    \>$\go{1}\wrong{non-numeric argument to {\cf <}})
\end{semfun}

\schindex{+}
\begin{semfun}
\fun{add}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{add} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$(\epsilon_1\:\elem\:\NUM\wedge\epsilon_2\:\elem\:\NUM)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$((\epsilon_1\:\vert\:\NUM+\epsilon_2\:\vert\:\NUM)\hbox{ \rm in }\EXP)
           \kappa,$\\
    \>$\go{1}\wrong{non-numeric argument to {\cf +}})
\end{semfun}

\schindex{car}
\begin{semfun}
\fun{car}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{car} =$\\
 \go{1}$\fun{onearg}\,(\lambda\epsilon\omega\kappa\:.\:
   $\=$\epsilon\:\elem\:\PAI\rightarrow
          \fun{car-internal}\:\epsilon\kappa,$\\
    \>$\go{1}\wrong{non-pair argument to {\cf car}})
\end{semfun}

\schindex{car-internal}
\begin{semfun}
\fun{car-internal}          :  \EXP \to \EC \to \CC$\\$
\fun{car-internal} =
 $\go{1}$\lambda\epsilon\omega\kappa\:.\:
   $\=$\fun{hold}\, (\epsilon\:\vert\:\PAI\elt 1) \kappa
\end{semfun}

\begin{semfun}
\fun{cdr}          :  \arbno{\EXP} \to \DP \to \EC \to \CC %$\\$
\hbox{\qquad [similar to \fun{car}]}
\end{semfun}

\begin{semfun}
\fun{cdr-internal} :  \EXP \to \EC \to \CC %$\\$
\hbox{\qquad [similar to \fun{car-internal}]}
\end{semfun}

\schindex{setcar}
\begin{semfun}
\fun{setcar}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{setcar} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$\epsilon_1\:\elem\:\PAI\rightarrow$\\
    \>$(\epsilon_1\:\vert\:\PAI\elt 3)\rightarrow
          \fun{assign}\,$\=$(\epsilon_1\:\vert\:\PAI\elt 1)$\\
    \>                           \>$\epsilon_2$\\
    \>                                  \>$(\fun{send}\:\fun{unspecified}\:\kappa),$\\
    \>$\wrong{immutable argument to {\cf set-car!}},$\\
    \>$\wrong{non-pair argument to {\cf set-car!}})
\end{semfun}

\schindex{eqv?}
\begin{semfun}
\fun{eqv}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{eqv} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$(\epsilon_1\:\elem\:\MSC\wedge\epsilon_2\:\elem\:\MSC)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$(\epsilon_1\:\vert\:\MSC = \epsilon_2\:\vert\:\MSC\rightarrow\fun{true},
            \fun{false})\kappa,$\\
    \>$(\epsilon_1\:\elem\:\SYM\wedge\epsilon_2\:\elem\:\SYM)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$(\epsilon_1\:\vert\:\SYM = \epsilon_2\:\vert\:\SYM\rightarrow\fun{true},
            \fun{false})\kappa,$\\
    \>$(\epsilon_1\:\elem\:\CHR\wedge\epsilon_2\:\elem\:\CHR)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$(\epsilon_1\:\vert\:\CHR = \epsilon_2\:\vert\:\CHR \rightarrow\fun{true},
            \fun{false})\kappa,$\\
    \>$(\epsilon_1\:\elem\:\NUM\wedge\epsilon_2\:\elem\:\NUM)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$(\epsilon_1\:\vert\:\NUM=\epsilon_2\:\vert\:\NUM\rightarrow\fun{true},
            \fun{false})\kappa,$\\
    \>$(\epsilon_1\:\elem\:\PAI\wedge\epsilon_2\:\elem\:\PAI)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$($\=$(\lambda{p_1}{p_2}\:.\:
                ($\=$({p_1}\elt 1) = ({p_2}\elt 1)\wedge$\\
    \>  \>   \>   \>$({p_1}\elt 2) = ({p_2}\elt 2))
                     \rightarrow\fun{true},$\\
    \>  \>   \>   \>$\go{1}\fun{false})$\\
    \>  \>   \>$(\epsilon_1\:\vert\:\PAI)$\\
    \>  \>   \>$(\epsilon_2\:\vert\:\PAI))$\\
    \>  \>$\kappa,$\\
    \>$(\epsilon_1\:\elem\:\VEC\wedge\epsilon_2\:\elem\:\VEC)\rightarrow
%\fun{send}\,
%       $\=$((\#(\epsilon_1\:\vert\:\VEC)=\#(\epsilon_2\:\vert\:\VEC)
%         \wedge\hbox{\rm Y}(\lambda\fun{loop}\:.\:\lambda\fun{v1}\fun{v2}\:.\:
%       $\=$\#\fun{v1}=0\rightarrow\fun{true},$\\
%    \>  \>  \>$(\fun{v1}\elt 1) = (\fun{v2}\elt 1)\rightarrow
%       \fun{loop}(\fun{v1}\drop 1)(\fun{v2}\drop 1),$\\
%    \>  \>  \>$\go{1}\fun{false})(\epsilon_1\:\vert\:\VEC)(\epsilon_2\:\vert\:\VEC))
%          \rightarrow\fun{true},$\\
%    \>  \>$\go{1}\fun{false})\kappa
\ldots,$\\
    \>$(\epsilon_1\:\elem\:\STR\wedge\epsilon_2\:\elem\:\STR)\rightarrow
%\fun{send}\,
%       $\=$((\#(\epsilon_1\:\vert\:\STR)=\#(\epsilon_2\:\vert\:\STR)\wedge
%    \hbox{\rm Y}(\lambda\fun{loop}\:.\:\lambda\fun{v1}\fun{v2}\:.\:
%       $\=$\#\fun{v1}=0\rightarrow\fun{true},$\\
%    \>  \>  \>$(\fun{v1}\elt 1) = (\fun{v2}\elt 1)\rightarrow
%     \fun{loop}(\fun{v1}\drop 1)(\fun{v2}\drop 1),$\\
%    \>  \>  \>$\go{1}\fun{false})(\epsilon_1\:\vert\:\STR)(\epsilon_2\:\vert\:\STR))
%      \rightarrow\fun{true},$\\
%    \>  \>$\go{1}\fun{false})\kappa
\ldots,$\\
    \>$(\epsilon_1\:\elem\:\FUN\wedge\epsilon_2\:\elem\:\FUN)\rightarrow$\\
    \>$\go{1}\fun{send}\,
       $\=$((\epsilon_1\:\vert\:\FUN\elt 1) = (\epsilon_2\:\vert\:\FUN\elt 1)
               \rightarrow\fun{true},
                          \fun{false})$\\
    \>  \>$\kappa,$\\
    \>$\go{1}\fun{send}\,\:\fun{false}\:\kappa)
\end{semfun}

\schindex{apply}
\begin{semfun}
\fun{apply}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{apply} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$\epsilon_1\:\elem\:\FUN\rightarrow
         \fun{valueslist}\:\epsilon_2
            (\lambda\arbno{\epsilon}\:.\:\fun{applicate}\:\epsilon_1\arbno{\epsilon}\omega\kappa),$\\
    \>$\go{1}\wrong{bad procedure argument to {\cf apply}})
\end{semfun}

\begin{semfun}
\fun{valueslist}          :  \EXP \to \EC \to \CC$\\$
\fun{valueslist} =$\\
 \go{1}$\lambda\epsilon\kappa\:.\:
   $\=$\epsilon\:\elem\:\PAI\rightarrow$\\
    \>$\go{1}\fun{cdr-internal}\:
         $\=$\epsilon$\\
    \>    \>$(\lambda\arbno{\epsilon}\:.\:
                  $\=$\fun{valueslist}\:$\\
    \>    \>       \>$\arbno{\epsilon}$\\
    \>    \>       \>$(\lambda\arbno{\epsilon}\:.\:$\=$\fun{car-internal}$\\
    \>    \>       \>                               \>$\:\epsilon$\\
    \>    \>       \>                               \>$ (\fun{single}(\lambda\epsilon\:.\:
              \kappa(\langle\epsilon\rangle\:\S\:\arbno{\epsilon}))))),$\\
    \>$\epsilon = \fun{null}\rightarrow\kappa\langle\:\rangle,$\\
    \>$\go{1}\wrong{non-list argument to {\cf values-list}}
\end{semfun}

\begin{semfun}
\fun{cwcc}          $\=$:  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
    $\>$ \hbox{\qquad [\ide{call-with-current-continuation}]}$\\$
\fun{cwcc} =$\\
 \go{1}$\fun{onearg}\,(\lambda\epsilon\omega\kappa\:.\:
   $\=$\epsilon\:\elem\:\FUN\rightarrow$\\
    \>$(\lambda\sigma\:.\:
       $\=$\fun{new}\:\sigma\:\elem\:\LOC\rightarrow$\\
    \>  \>$\go{1}\fun{applicate}\:
           $\=$\epsilon$\\
    \>  \>  \>$\langle\langle$\=$\fun{new}\:\sigma\:\vert\:\LOC,$\\
    \>  \>  \>  \>$          \lambda\arbno{\epsilon}\omega^\prime\kappa^\prime\:.\:
                             \fun{travel}\:\omega^\prime\omega(\kappa\arbno{\epsilon})\rangle$\\
    \>  \>  \>$                      \hbox{ \rm in }\EXP\rangle$\\
    \>  \>  \>$\omega$\\
    \>  \>  \>$\kappa$\\
    \>  \>  \>$(\fun{update}\,
                $\=$(\fun{new}\:\sigma\:\vert\:\LOC)$\\
    \>  \>  \>   \>$\fun{unspecified}$\\
    \>  \>  \>   \>$\sigma),$\\
    \>  \>$\go{1}\wrong{out of memory}\,\sigma),$\\
    \>$\wrong{bad procedure argument})
\end{semfun}

\begin{semfun}
\fun{travel} : \DP \to \DP \to \CC \to \CC$\\$
\fun{travel} = $\\
  \go{1}$\lambda\omega_1\omega_2\:.\:
  \fun{travelpath}\:($\=$(\fun{pathup}\:\omega_1(\fun{commonancest}\:\omega_1\omega_2)) \:\S\:$\\
  \>$ (\fun{pathdown}\:(\fun{commonancest}\:\omega_1\omega_2)\omega_2))
\end{semfun}

\begin{semfun}
\fun{pointdepth} : \DP \to \NAT$\\$
\fun{pointdepth} = $\\
  \go{1}$\lambda\omega\:.\: \omega = \textit{root} \rightarrow 0,
  1 + (\fun{pointdepth}\:(\omega\:\vert\:(\FUN \times \FUN \times
  \DP)\elt 3))
\end{semfun}

\begin{semfun}
\fun{ancestors} : \DP \to \mathcal{P}\DP$\\$
\fun{ancestors} = $\\
  \go{1}$\lambda\omega\:.\: \omega = \textit{root} \rightarrow \{\omega\},
  \{\omega\}\:\cup\:(\fun{ancestors}\:(\omega\:\vert\:(\FUN \times \FUN \times
  \DP)\elt 3))
\end{semfun}

\begin{semfun}
\fun{commonancest} : \DP \to \DP \to \DP$\\$
\fun{commonancest} = $\\
  \go{1}$\lambda\omega_1\omega_2\:.\:$\=$
  \textrm{the only element of }$\\
  \>$\{ \omega^\prime \:\mid\:$\=$
  \omega^\prime\in(\fun{ancestors}\:\omega_1)\:\cap\:(\fun{ancestors}\:\omega_2),$\\
  \>\>$\fun{pointdepth}\:\omega^\prime\geq \fun{pointdepth}\:\omega^{\prime\prime}$\\
  \>\>$\forall
  \omega^{\prime\prime}\in(\fun{ancestors}\:\omega_1)\:\cap\:(\fun{ancestors}\:\omega_2)\}
\end{semfun}

\begin{semfun}
\fun{pathup} : \DP \to \DP \to \arbno{(\DP \times \FUN)}$\\$
\fun{pathup} = $\\
  \go{1}$\lambda\omega_1\omega_2\:.\:
  $\=$\omega_1=\omega_2\rightarrow\langle\rangle,$\\
  \>$\langle(\omega_1, \omega_1\:\vert\:(\FUN \times \FUN \times \DP)\elt 2)\rangle
  \:\S\:$\\
  \>$(\fun{pathup}\:(\omega_1\:\vert\:(\FUN \times \FUN \times \DP)\elt 3)\omega_2)
\end{semfun}

\begin{semfun}
\fun{pathdown} : \DP \to \DP \to \arbno{(\DP \times \FUN)}$\\$
\fun{pathdown} = $\\
  \go{1}$\lambda\omega_1\omega_2\:.\:
  $\=$\omega_1=\omega_2\rightarrow\langle\rangle,$\\
  \>$(\fun{pathdown}\:\omega_1(\omega_2\:\vert\:(\FUN \times \FUN \times \DP)\elt 3))
  \:\S\:$\\
  \>$\langle(\omega_2, \omega_2\:\vert\:(\FUN \times \FUN \times \DP)\elt 1)\rangle
\end{semfun}

\begin{semfun}
\fun{travelpath} : \arbno{(\DP \times \FUN)} \to \CC \to \CC$\\$
\fun{travelpath} = $\\
  \go{1}$\lambda\arbno{\pi}\theta\:.\:
  $\=$\#\arbno{\pi}=0\rightarrow\theta,$\\
  \>$((\arbno{\pi}\elt 1)\elt 2)$\=$\langle\rangle((\arbno{\pi}\elt 1)\elt 1)$\\
  \>\>$(\lambda\arbno{\epsilon}\:.\:\fun{travelpath}\:(\arbno{\pi} \drop 1)\theta)
\end{semfun}

\begin{semfun}
\fun{dynamicwind} : \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{dynamicwind} = $\\
\go{1}$\fun{threearg}\,(\lambda$\=$\epsilon_1\epsilon_2\epsilon_3\omega\kappa\:.\:
  (\epsilon_1\:\elem\:\FUN\wedge\epsilon_2\:\elem\:\FUN\wedge\epsilon_3\:\elem\:\FUN)\rightarrow$\\
  \>$\fun{applicate}\:
  $\=$\epsilon_1\langle\rangle\omega(\lambda\arbno{\zeta}$\=$\:.\:$\\
  \>\>$\fun{applicate}\:$\=$\epsilon_2\langle\rangle
  ((\epsilon_1\:\vert\:\FUN,\epsilon_3\:\vert\:\FUN,\omega)\textrm{ in }\DP)$\\
  \>\>\>$(\lambda\arbno{\epsilon}\:.\:\fun{applicate}\:\epsilon_3\langle\rangle\omega(\lambda\arbno{\zeta}\:.\:\kappa\arbno{\epsilon}))),$\\
  \>$\wrong{bad procedure argument})
\end{semfun}

\begin{semfun}
\fun{values}          :  \arbno{\EXP} \to \DP \to \EC \to \CC$\\$
\fun{values} =
 \lambda\arbno{\epsilon}\omega\kappa\:.\:\kappa\arbno{\epsilon}
\end{semfun}

\begin{semfun}
\fun{cwv}          :  \arbno{\EXP} \to \DP \to \EC \to \CC
    \hbox{\qquad [\ide{call-with-values}]}$\\$
\fun{cwv} =$\\
 \go{1}$\fun{twoarg}\,(\lambda\epsilon_1\epsilon_2\omega\kappa\:.\:
   $\=$\fun{applicate}\:\epsilon_1\langle\:\rangle\omega
(\lambda\arbno{\epsilon}\:.\:\fun{applicate}\:\epsilon_2\:\arbno{\epsilon}\omega))
\end{semfun}

\egroup  % end smallish

\egroup
## 7.3. Derived expression types
\label{derivedsection}

This section gives syntax definitions for the derived expression types in
terms of the primitive expression types (literal, variable, call, {\cf lambda},
{\cf if}, and {\cf set!}), except for {\cf quasiquote}.

Conditional derived syntax types:

\begin{scheme}
(define-syntax \ide{cond}
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{case}
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{and}
  (syntax-rules ()
    ((and) \sharpfoo{t})
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) \sharpfoo{f}))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{or}
  (syntax-rules ()
    ((or) \sharpfoo{f})
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{when}
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{unless}
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))
\end{scheme}

Binding constructs:

\begin{scheme}
(define-syntax \ide{let}
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{let*}
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))
\end{scheme}

The following {\cf letrec} macro uses the symbol {\cf <undefined>}
in place of an expression which returns something that when stored in
a location makes it an error to try to obtain the value stored in the
location.  (No such expression is defined in Scheme.)
A trick is used to generate the temporary names needed to avoid
specifying the order in which the values are evaluated.
This could also be accomplished by using an auxiliary macro.

\begin{scheme}
(define-syntax \ide{letrec}
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
     (letrec "generate\_temp\_names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((letrec "generate\_temp\_names"
       ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     (let ((var1 <undefined>) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((letrec "generate\_temp\_names"
       (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (letrec "generate\_temp\_names"
       (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{letrec*}
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 <undefined>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))%
\end{scheme}

\begin{scheme}
(define-syntax \ide{let-values}
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
         (binding ...) () (begin body0 body1 ...)))

    ((let-values "bind" () tmps body)
     (let tmps body))

    ((let-values "bind" ((b0 e0)
         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
         (binding ...) tmps body))

    ((let-values "mktmp" () e0 args
         bindings tmps body)
     (call-with-values
       (lambda () e0)
       (lambda args
         (let-values "bind"
             bindings tmps body))))

    ((let-values "mktmp" (a . b) e0 (arg ...)
         bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
         bindings (tmp ... (a x)) body))

    ((let-values "mktmp" a e0 (arg ...)
        bindings (tmp ...) body)
     (call-with-values
       (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind"
             bindings (tmp ... (a x)) body))))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{let*-values}
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))

    ((let*-values (binding0 binding1 ...)
         body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{define-values}
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
                         (lambda args \schfalse))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
                           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
       (define varn
         (let ((v (cadr var0)))
           (set! var0 (car var0))
           v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
                           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
       (define varn
         (let ((v (cdr var0)))
           (set! var0 (car var0))
           v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr)
                         list)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{begin}
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))
\end{scheme}

The following alternative expansion for {\cf begin} does not make use of
the ability to write more than one expression in the body of a lambda
expression.  In any case, note that these rules apply only if the body
of the {\cf begin} contains no definitions.

\begin{scheme}
(define-syntax begin
  (syntax-rules ()
    ((begin exp)
     exp)
    ((begin exp1 exp2 ...)
     (call-with-values
         (lambda () exp1)
       (lambda args
         (begin exp2 ...))))))
\end{scheme}

The following syntax definition
of {\cf do} uses a trick to expand the variable clauses.
As with {\cf letrec} above, an auxiliary macro would also work.
The expression {\cf (if \#f \#f)} is used to obtain an unspecific
value.

\begin{scheme}
(define-syntax \ide{do}
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if \#f \#f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
\end{scheme}

Here is a possible implementation of {\cf delay}, {\cf force} and {\cf
  delay-force}.  We define the expression

\begin{scheme}
(delay-force \hyper{expression})%
\end{scheme}

to have the same meaning as the procedure call

\begin{scheme}
(make-promise \schfalse{} (lambda () \hyper{expression}))%
\end{scheme}

as follows

\begin{scheme}
(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise \schfalse{} (lambda () expression)))))%
\end{scheme}

and we define the expression

\begin{scheme}
(delay \hyper{expression})%
\end{scheme}

to have the same meaning as:

\begin{scheme}
(delay-force (make-promise \schtrue{} \hyper{expression}))%
\end{scheme}

as follows

\begin{scheme}
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise \schtrue{} expression)))))%
\end{scheme}

where {\cf make-promise} is defined as follows:

\begin{scheme}
(define make-promise
  (lambda (done? proc)
    (list (cons done? proc))))%
\end{scheme}

Finally, we define {\cf force} to call the procedure expressions in
promises iteratively using a trampoline technique following
\cite{srfi45} until a non-lazy result (i.e. a value created by {\cf
  delay} instead of {\cf delay-force}) is returned, as follows:

\begin{scheme}
(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (unless (promise-done? promise)
          (promise-update! promise* promise))
        (force promise))))%
\end{scheme}

with the following promise accessors:

\begin{scheme}
(define promise-done?
  (lambda (x) (car (car x))))
(define promise-value
  (lambda (x) (cdr (car x))))
(define promise-update!
  (lambda (new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old))))%
\end{scheme}

The following implementation of {\cf make-parameter} and {\cf
parameterize} is suitable for an implementation with no threads.
Parameter objects are implemented here as procedures, using two
arbitrary unique objects \texttt{<param-set!>} and
\texttt{<param-convert>}:

\begin{scheme}
(define (make-parameter init . o)
  (let* ((converter
          (if (pair? o) (car o) (lambda (x) x)))
         (value (converter init)))
    (lambda args
      (cond
       ((null? args)
        value)
       ((eq? (car args) <param-set!>)
        (set! value (cadr args)))
       ((eq? (car args) <param-convert>)
        converter)
       (else
        (error "bad parameter syntax"))))))%
\end{scheme}

Then {\cf parameterize} uses {\cf dynamic-wind} to dynamically rebind
the associated value:

\begin{scheme}
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new ((p <param-convert>) value)) ...)
         (dynamic-wind
          (lambda () (p <param-set!> new) ...)
          (lambda () . body)
          (lambda () (p <param-set!> old) ...)))))
    ((parameterize ("step")
                   args
                   ((param value) . rest)
                   body)
     (parameterize ("step")
                   ((param value p old new) . args)
                   rest
                   body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
                   ()
                   ((param value) ...)
                   body))))
\end{scheme}

The following implementation of {\cf guard} depends on an auxiliary
macro, here called {\cf guard-aux}.

\begin{scheme}
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
               (lambda (handler-k)
                 (guard-k
                  (lambda ()
                    (let ((var condition))
                      (guard-aux
                        (handler-k
                          (lambda ()
                            (raise-continuable condition)))
                        clause ...))))))))
          (lambda ()
            (call-with-values
             (lambda () e1 e2 ...)
             (lambda args
               (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))
\end{scheme}

\begin{scheme}
(define-syntax \ide{case-lambda}
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (let-syntax
             ((cl (syntax-rules ::: ()
                    ((cl)
                     (error "no matching clause"))
                    ((cl ((p :::) . body) . rest)
                     (if (= len (length '(p :::)))
                         (apply (lambda (p :::)
                                  . body)
                                args)
                         (cl . rest)))
                    ((cl ((p ::: . tail) . body)
                         . rest)
                     (if (>= len (length '(p :::)))
                         (apply
                          (lambda (p ::: . tail)
                            . body)
                          args)
                         (cl . rest))))))
           (cl (params body0 ...) ...)))))))

\end{scheme}

This definition of {\cf cond-expand} does not interact with the
{\cf features} procedure.  It requires that each feature identifier provided
by the implementation be explicitly mentioned.

\begin{scheme}
(define-syntax cond-expand
  ;; Extend this to mention all feature ids and libraries
  (syntax-rules (and or not else r7rs library scheme base)
    ((cond-expand)
     (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...)
                  more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...)
                  more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (r7rs body ...)
                  more-clauses ...)
       (begin body ...))
    ;; Add clauses here for each
    ;; supported feature identifier.
    ;; Samples:
    ;; ((cond-expand (exact-closed body ...)
    ;;               more-clauses ...)
    ;;   (begin body ...))
    ;; ((cond-expand (ieee-float body ...)
    ;;               more-clauses ...)
    ;;   (begin body ...))
    ((cond-expand ((library (scheme base))
                   body ...)
                  more-clauses ...)
      (begin body ...))
    ;; Add clauses here for each library
    ((cond-expand (feature-id body ...)
                  more-clauses ...)
       (cond-expand more-clauses ...))
    ((cond-expand ((library (name ...))
                   body ...)
                  more-clauses ...)
       (cond-expand more-clauses ...))))

\end{scheme}
