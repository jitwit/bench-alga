(import (srfi :1)
	(srfi :13)
	(srfi :14))

;; take a criterion json report and get name and average time
(define (reports)
  (filter (lambda (file)
	    (string=? (path-extension file) "json"))
	  (directory-list ".")))

(define (summarize-result-for graph result)
  (let* ((pts (filter (lambda (pt)
			(string=? graph (car pt)))
		      result))
	 (new-alga (list-ref (car (filter (lambda (x)
					    (string=? "new-alga" (list-ref x 1)))
					  pts))
			     2)))
    (cons graph
	  (map (lambda (pt)
		 `(,(list-ref pt 1)
		   ,(/ (list-ref pt 2)
		       new-alga)))
	       pts))))

(define (report->sexp file)
  (let ((sexp (format "~a.ss" (path-root file))))
    (system (format "sexp_of_json < ~a > ~a" file sexp))
    (let* ((results (vector->list
		      (vector-map result->summary
				  (criterion-results
				   (with-input-from-file sexp read)))))
	   (graphs (delete-duplicates (map car results) string=?)))
      (cons (path-root file)
	    (map (lambda (graph)
		   (summarize-result-for graph results))
		 graphs)))))

(define (criterion-results json)
  (vector-ref json 2))

(define (result->summary json)
  (let ((name (string-tokenize (symbol->string
				 (cdr
				   (assq 'reportName json)))
			       (char-set-complement (char-set #\/))))
	(mean (cdr (assq 'estPoint
			 (cdr (assq 'anMean
				    (cdr (assq 'reportAnalysis
					       json))))))))
    `(,@name ,mean)))

(define (main)
  (let ((results (map report->sexp (reports)))
	(out-file "alga-bench.ss"))
    (when (file-exists? out-file)
      (delete-file out-file))
    (with-output-to-file out-file
      (lambda ()
	(pretty-print results)))))
