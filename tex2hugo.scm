#!/usr/bin/env gosh

(use gauche.record)
(use srfi-13)

(define (usage)
  (print "usage: tex2hugo <filename>..."))

(define-record-type context #t #t
  (chapter) (section) (subsection))

(define-syntax zero!
  (syntax-rules ()
    ((_ loc)
     (set! loc 0))))

(define (convert-text text)
  (regexp-replace-all* text
		       #/\\chapter{(.*?)}/ "# \\1"
		       #/\\section{(.*?)}/ "## \\1"
		       #/\\subsection\*?{(.*?)}/ "### \\1"
		       #/\\begin{scheme}/   "```"
		       #/\\end{scheme}/     "```"
		       #/\\vest / ""
		       #/(?<!`)``(?!`)(.*?)''/ "\"\\1\""

		       #/\\medskip/ "{{< medskip >}}"

		       #/\\rthreers/ "{{< rnrs 3 >}}"
		       #/\\rfourrs/  "{{< rnrs 4 >}}"
		       #/\\rfivers/  "{{< rnrs 5 >}}"
		       #/\\rsixrs/   "{{< rnrs 6 >}}"
		       #/\\rsevenrs/ "{{< rnrs 7 >}}"

		       #/%\n/ "\n"
		       #/\\todo{(.*?)}/ ""
		       ))

(define (format-line line c)
  (rxmatch-if (rxmatch #/^chapter = (.*)/ line) (#f chapter)
    (context-chapter-set! c chapter) #f)

  (regexp-replace* line

		   #/^# ([0-9]+\. )?([^ ].*)/
		   (lambda (m)
		     (if (context-chapter c)
			 (begin
			   (zero! (context-section c))
			   (format #f "# ~a. ~a" (context-chapter c) (m 2)))
			 (m 0)))

		   #/^## ([0-9]+\.[0-9]+\. )?([^ ].*)/
		   (lambda (m)
		     (if (context-chapter c)
			 (begin
			   (inc!  (context-section c))
			   (zero! (context-subsection c))
			   (format #f "## ~a.~a. ~a"
				   (context-chapter c)
				   (context-section c)
				   (m 2)))
			 (m 0)))

		   #/^### ([0-9]+\.[0-9]+\.[0-9]+\. )?([^ ].*)/
		   (lambda (m)
		     (if (context-chapter c)
			 (begin
			   (inc! (context-subsection c))
			   (format #f "### ~a.~a.~a. ~a"
				   (context-chapter    c)
				   (context-section    c)
				   (context-subsection c)
				   (m 2)))
			 (m 0)))
		   ))

(define (convert-file filename)
  (let* ((tmpfile (string-append filename ".tmp"))
	 (in      (open-input-file filename))
	 (out     (open-output-file tmpfile :if-does-not-exist :create))
	 (c       (make-context #f 0 0)))
    (for-each (lambda (line)
		(display (format-line line c) out)
		(newline out))
	      (string-split (convert-text (string-trim-right
					   (port->string in)))
			    #\newline))
    (close-port in)
    (close-port out)
    (sys-rename tmpfile filename)))

(define (main args)
  (if (= (length args) 1)
      (begin
	(usage)
	(exit 1)))
  (for-each convert-file (cdr args)))
