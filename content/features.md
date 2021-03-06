+++
weight = 77
title = "B Standard Feature Identifiers"
menu = "main"
+++
# Standard Feature Identifiers
{{< label "stdfeatures" >}}

An implementation may provide any or all of the feature identifiers
listed below for use by ``cond-expand`` and ``features``,
but must not provide a feature identifier if it does not
provide the corresponding feature.

{{< label "standard_features" >}}

\feature{r7rs}{All {{< rnrs 7 >}} Scheme implementations have this feature.}
\feature{exact-closed}{All algebraic operations except ``/`` produce
  exact values given exact inputs.}
\feature{exact-complex}{Exact complex numbers are provided.}
\feature{ieee-float}{Inexact numbers are IEEE 754 binary floating point
  values.}
\feature{full-unicode}{All Unicode characters present in Unicode version 6.0 are supported as Scheme characters.}
\feature{ratios}{``/`` with exact arguments produces an exact result
  when the divisor is nonzero.}
\feature{posix}{This implementation is running on a POSIX
  system.}
\feature{windows}{This implementation is running on Windows.}
\feature{unix, darwin, gnu-linux, bsd, freebsd, solaris, ...}{Operating
  system flags (perhaps more than one).}
\feature{i386, x86-64, ppc, sparc, jvm, clr, llvm, ...}{CPU architecture flags.}
\feature{ilp32, lp64, ilp64, ...}{C memory model flags.}
\feature{big-endian, little-endian}{Byte order flags.}
\feature{{{< hyper "name" >}}}{The name of this implementation.}
\feature{{{< hyper "name-version" >}}}{The name and version of this
  implementation.}
