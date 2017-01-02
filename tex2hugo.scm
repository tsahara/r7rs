#!/usr/bin/env gosh

(use gauche.record)

(define (usage)
  (print "usage: tex2hugo <filename>..."))

(define-record-type context #t #t
  (chapter) (section) (subsection))

(define (convert-line line)
  (regexp-replace-all* line
		       #/\\chapter{(.*?)}/ "# \\1"
		       #/\\section{(.*?)}/ "## \\1"
		       #/\\subsection{(.*?)}/ "### \\1"
		       #/\\vest / ""
		       #/``(.*?)''/ "\"\\1\""
		       ))

(define (format-line line c)
  (rxmatch-if (rxmatch #/^chapter = (.*)/ line) (#f chapter)
    (context-chapter-set! c chapter) #f)

  (regexp-replace* line

		   #/^# +([0-9]+\. +)?(.*)/
		   (lambda (m)
		     (if (context-chapter c)
			 (format #f "# ~a. ~a" (context-chapter c) (m 2))
			 (m 0)))

		   #/^## +([0-9]+\.[0-9]+\. +)?(.*)/
		   (lambda (m)
		     (if (context-chapter c)
			 (begin
			   (inc! (context-section c))
			   (format #f "## ~a.~a. ~a"
				   (context-chapter c)
				   (context-section c)
				   (m 2)))
			 (m 0)))

		   #/^### +([0-9]+\.[0-9]+\.[0-9]+\. +)?(.*)/
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
    (let loop ((line (read-line in)))
      (unless (eof-object? line)
	(display (format-line (convert-line line) c)
		 out)
	(newline out)
	(loop (read-line in))))
    (close-port in)
    (close-port out)
    (sys-rename tmpfile filename)))

(define (main args)
  (if (= (length args) 1)
      (begin
	(usage)
	(exit 1)))
  (for-each convert-file (cdr args)))
