; /home/lavm/lecture-notes/cm315/scheme/eval-eng/toplevel.scm created by Lavm on kronos on Wed Aug  6 10:01:08 1997 
; $Id: toplevel.scm,v 1.6 1998/09/30 22:13:40 lavm Exp cm203l $ 

'(module toplevel
  (main main)
  (import
     (primitives "primitives.scm")
     (tester "tester.scm")
     (eps "eps.scm")
     (core-cps "core-cps.scm")
     (eval-cps "eval-cps.scm")))


(define *usage*
  "

-bound num              Bound on the execution length
-summary                Display a summary of failed executions
-r5rs                   Supports r5rs extensions (all?)
-help                   Displays this text
-tests filenames...     Check some test suites
-verif ????
[no option]             Runs the interpreter.
" )

(define default-banner 
  "**********************************************************************
*                                                                    *
*                         COMP2209 Scheme Tester                     *
*                                                                    *
**********************************************************************")

(define *summary-OK* '())
(define *summary-FAIL* '())

(define (try v handler) v)  ;;; hack, LUC: to be fixed

(set! banner default-banner)
(define (test-on-file suite-test file)
  (suite-test
   file
   "\n>>> "
   "--> "
   #t
   (lambda (read check error)
     (lambda ()
       (set! *count* 0)
       (let ((p (read)))
	 (set! resume-terminate (error p))
         (let* ((handler (lambda (escape proc message object)
			   (println "Caught error")
			   (wrong proc message object)))
		(v (try (evaluate p)
			handler)))
           (check p (list v))))))
   check-result
   (lambda ()
     (> *count* *bound*))
   (lambda (msg-result)
     (lambda (expression status expected value)
       (set! *summary-OK* (cons (list expression
				      (msg-result status expected value)
				      expected
				      value)
				*summary-OK*))))
   (lambda (msg-result)
     (lambda (expression status expected value)
       (set! *summary-FAIL* (cons (list expression
					(msg-result status expected value)
					expected
					value)
				  *summary-FAIL*)))))
  (println "exiting test-on-file"))

(define (display-result expected obtained)
  (let ((obtained2 (if (and (pair? obtained)
			    (null? (cdr obtained)))
		       (car obtained)
		       obtained)))
    (display "expected ")
    (display expected)
    (display " obtained ")
    (display obtained2)
    (newline)))

(define (check-result expected obtained)
  (display-result expected obtained)
  (or (and (pair? expected)
           (case (car expected)
             ((multiple)
              (set-equal? (cdr expected) obtained) )
             ((or)
              (and (pair? (cdr expected))
                   (or (check-result (cadr expected) obtained)
                       (check-result `(or . ,(cddr expected)) obtained) ) ) )
             (else #f
					;(naive-match expected obtained)
		   ) ) )
      
      (and (pair? obtained)
	   (null? (cdr obtained))
	   (naive-match expected (car obtained)) ) ))





(define (tester-entry args)

  (newline)
;  (print (date))
;  (newline)

  (if (member "-help" args)
      (begin (display *usage*)
             (newline)
             (exit 0) ) )

  (let ((val (member "-bound" args)))
    (if val
	(set! *bound*
	      (string->number (cadr val)))))

  (if (member "-r5rs" args)
      (set! *the-global-environment*
	    (r5rs-global-environment! *the-global-environment*)))

  (let ((make-resumer (lambda (cont)
                        (resume-cps cont
                                    (lambda (cont)
                                      (resume-core cont
                                                   (lambda (cont)
                                                     (wrong 'resume
                                                            "Unknown type of continuation"
                                                            cont))))))))
    (set! resume
          (lambda (cont val)
            ((make-resumer cont) cont val)))
    (set! return-values
          (lambda (cont vals)
            (apply (make-resumer cont) cont vals))))
       


  (println banner)
      
  (let* ((test? (member "-tests" args)))
    (if (pair? test?)
        (do-the-test test? suite-test-file args)
	(let ((test? (member "-urls" args)))
	  (if (pair? test?)
	      (do-the-test test? suite-test-url args)
	      (driver-loop))) ) ) )


(define (do-the-test test? how-test args)
  (letrec ((filter-to-first (lambda (p l)
			      (cond ((null? l) '())
				    ((p (car l)) (cons (car l)
						       (filter-to-first p (cdr l))))
				    (else '())))))
    (let ((files (filter-to-first (lambda (x)
				    (not (equal? (string-ref x 0) #\-)))
				  (cdr test?))))
      (println files)
      (set! *load-path* (cons (current-directory) *load-path*))
      (for-each (lambda (file)
		  (test-on-file how-test file)) files)
      (println "Done all files")
      (let ((args (member "-summary" args)))
	(if args
	    (let ((file (cadr args)))
	      (display-summary file))))
      (exit 0))))

(define (display-final-score score-value test-total)
  (display "Score: ")
  (display score-value)
  (display "/")
  (display test-total))

(define display-summary
  (lambda (file)
    (with-output-to-file file
		      (lambda ()
			(newline)
			(display "======================================================================")
			(newline)
			(let* ((latest-score *latest-score*)
			       (score-value (car latest-score))
			       (test-total (cdr latest-score)))
			  (if (= score-value test-total)
			      (begin
				(display "Total success")
				(newline)
				(display-final-score score-value test-total)
				(newline))
			      (begin ;;; failures
				(for-each (lambda (x)
					    (display (car x))
					    (display " :")
					    (newline)
					    (display "	")
					    (display (cadr x))
					    (newline)
					    (display "	expected: ")
					    (display (caddr x))
					    (display ", received: ")
					    (display (cadddr x))
					    (newline))
					  (reverse *summary-FAIL*))
				(display-final-score score-value test-total)
				(newline))))
			(display "======================================================================")
			(newline)))
    (if (null? *summary-FAIL*)
	(exit 0)
	(exit 1))))



; end of toplevel.scm 

