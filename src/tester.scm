;;; $Id: tester.scm,v 1.4 1998/09/27 18:24:31 lavm Exp cm203l $


;;; Copyright (c) 1996 by Luc Moreau. All rights reserved.

;;;      ***********************************************
;;;      *                                             *
;;;      *                   NeXeme                    *
;;;      *                                             *
;;;      ***********************************************




'(module tester
	(import (core-cps "core-cps.scm"))
	(export 
		(suite-test file
			    prompt-in
			    prompt-out
			    echo?
			    make-toplevel
			    compare
			    diverged?
			    output-OK
			    output-FAIL)
		(set-equal? x y)
		(naive-match pattern expression)))


(define tester-error
  (lambda (string val)
    (error 'tester-error
	   string
	   val)))

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                         *********************************************
;;;                          An engine to test interpreters or compilers
;;;                                    Christian Queinnec 
;;;                          \'Ecole Polytechnique  & INRIA-Rocquencourt
;;;                         *********************************************

;;; This file contains the basic facility to easily run an interpreter
;;; defining a language. It can ask the user for expressions to be 
;;; evaluated and print their results or take expressions from a file,
;;; evaluate them and compare their value to the expected value. If an 
;;; error occurs then the test suite is aborted. 

;;; A validation suite is a sequence of expressions followed by their
;;; expected results. The expression is evaluated in the language being
;;; tested then its result is compared to the expected result. 
;;; An example of a test suite is:
;;; (car '(a b))
;;;   a
;;; (car (list))
;;;   ***      ; means that an error is expected
;;; (oblist)
;;;    ---     ; means that no error is expected but the value is unimportant
;;; It is an error of course to expect an error if no error occurrs.

;;;   oo      ; means that divergence is expected.  It is an error
;;;            if an error occurs or a result is returned.
;;;   +++     ; means that anything is expected, error or not.


;;;; new, optional test name allowed. Should follow the expression. Should start with $ sign.

;;; nameless:
;;;  expression
;;;  expected

;;; with name:
;;;  expression
;;;  $myname
;;;  expected

;;; This package may use the tester-error function in case of an
;;; internal error. It is not defined here since errors are not
;;; portable. You must have a location to define that variable for
;;; some compilers. Two cases may trigger this error: a suite test
;;; missing an expecting result or a (toplevel) form returning an
;;; unprinted value. Other errors are caught within engine-tester.

;;; The testing engine can be parameterized in various ways. 
;;; Apart the engine itself, two functions are offered that ease its use.




;;; suite-test is similar to the preceding one except that tests are taken
;;; from a file, possibly echoed on the console and checked to be correct. 
;;; The suite contains expressions followed by their expected result.
;;; The result of the evaluation is compared to this result, the 
;;; suite is aborted if an error occurs.

;;; suite-test takes six arguments:
;;;  -- a file-name: The file contains the expressions to be evaluated and
;;;     the expected results.
;;;  -- an input prompt
;;;  -- an output prompt
;;;  -- a boolean flag which governs if read expressions from the file are
;;;     echoed on the console.
;;;  -- a make-toplevel function that will return the toplevel function
;;;     make-toplevel may be roughly defined as
;;;       (lambda (test-read test-checker wrong) 
;;;         (lambda () (test-checker (tested-eval (test-read)))) )
;;;     The toplevel function will be repeatedly invoked from the 
;;;     interpreter. The arguments of make-toplevel are
;;;   == (test-read): the function that reads an expression, echoing it
;;;      after printing the input-prompt.
;;;   == (test-checker v): this function takes a value, reads the expected
;;;      result, compares them and if according, prints the value preceded by
;;;      the output prompt. A new toplevel is started after that.
;;;   == ((wrong expression) message . culprits): a function that reports the error,
;;;      aborts the current computation and restart a new cycle.
;;;  -- a (comparator) function that takes the obtained result and the
;;;     expected result and compares them yielding a boolean.
;;;     Very often, result-eval is just `equal?' but must
;;;     recognize the *** and --- items which meaning is "an error is
;;;     expected" or "an unimportant value (but no error)".
;;; diverged?
;;; ((output-OK msg-status) the-expression expected result)
;;; ((output-FAIL msg-status) the-expression expected result)

(define (suite-test-file file                ; the test suite
			 prompt-in           ; the prompt to read
			 prompt-out          ; the prompt to display
			 echo?               ; echo expressions ?
			 make-toplevel       ; a toplevel generator
			 compare             ; how to compare results
			 diverged?            ;
			 output-OK           ;
			 output-FAIL        )
  (let ((in (open-input-file file)))
    (suite-test-port in 
		     prompt-in
		     prompt-out
		     echo?     
		     make-toplevel
		     compare      
		     diverged?    
		     output-OK    
		     output-FAIL)))

(define (suite-test-url  url                ; the test suite
			 prompt-in           ; the prompt to read
			 prompt-out          ; the prompt to display
			 echo?               ; echo expressions ?
			 make-toplevel       ; a toplevel generator
			 compare             ; how to compare results
			 diverged?            ;
			 output-OK           ;
			 output-FAIL        )
  (if (equal? (substring url 0 4) "http")
      (let ((in (open-process (list path: "curl"
				    arguments: (list url)))))
	(suite-test-port in 
			 prompt-in
			 prompt-out
			 echo?     
			 make-toplevel
			 compare      
			 diverged?    
			 output-OK    
			 output-FAIL))))

(define *latest-score* '(0 . 0))

(define (suite-test-port in                  ; the test suite input port
			 prompt-in           ; the prompt to read
			 prompt-out          ; the prompt to display
			 echo?               ; echo expressions ?
			 make-toplevel       ; a toplevel generator
			 compare             ; how to compare results
			 diverged?            ;
			 output-OK           ;
			 output-FAIL        )
  (let ((native-display display)
        (native-newline newline)
	(score-value 0)
	(test-count 0))
    ;; Two small utilities to display things
    (define (display exp)
      (if echo? (native-display exp)) )
    (define (newline)
      (if echo? (native-newline)) )

    (define (score val marks)
      (set! test-count (+ test-count marks))
      (if (> val 0)
	  (set! score-value (+ score-value marks)))
      (native-display "Score: ")
      (native-display score-value)
      (native-display "/")
      (native-display test-count)
      (native-newline)
      (set! *latest-score* (cons score-value test-count)))

    (define (some-marks marks)
      (and marks (> marks 0)))

    ;; Display the result of the test, return a boolean to indicate
    ;; whether the tests should continue or not.
    (define (display-status status expected v marks)
      (cons status
	    (case status
	      ((expected-error)
	       (set! echo? #t)
	       (display prompt-out)
	       (display v)
	       (display "  an ERROR was expected !!! ")
	       (newline)
	       (if (some-marks marks) (score -1 marks))
	       #f )			; stop iteration
	      ((error-occurred)
	       (display " OK OK")
	       (newline)
	       (if (some-marks marks) (score +1 marks))
	       #t )			; continue iteration
	      ((expected-divergence)
	       (display " DIVERGES OK")
	       (newline)
	       (if (some-marks marks) (score +1 marks))
	       #t )			; continue iteration
	      ((unexpected-error)
	       (newline)
	       (display v)
	       (set! echo? #t)
	       (display "  an unexpected ERROR occured !!!")
	       (newline)
	       (display "  value expected(1): ")
	       (display expected)
	       (newline)
	       (if (some-marks marks) (score -1 marks))
	       #f )			; stop iteration
	      ((interrupted-divergence)
	       (newline)
	       (display v)
	       (set! echo? #t)
	       (display "  an unexpected ERROR occured !!!")
	       (newline)
	       (display "  divergence was expected.")
	       (newline)
	       (if (some-marks marks) (score -1 marks))
	       #f )
	      ((return-non-divergence)
	       (newline)
	       (display v)
	       (set! echo? #t)
	       (display "  divergence was expected.")
	       (newline)
	       (if (some-marks marks) (score -1 marks))
	       #f )
	      ((correct-result)
	       (display prompt-out)
	       (display v)
	       (display "  OK")
	       (newline)
	       (if (some-marks marks) (score +1 marks))
	       #t )			; continue iteration
	      ((incorrect-result)
	       (set! echo? #t)
	       (display prompt-out)
	       (display v)
	       (display "  ERROR !!!")
	       (newline)
	       (display "value expected(2):")
	       (display expected)
	       (newline)
	       (if (some-marks marks) (score -1 marks))
	       #f )			; stop iteration
	      ((uninteresting-result)
	       (display "  OK")
	       (newline)
	       (if (some-marks marks) (score +1 marks))
	       #t )			; continue iteration
	      (else (display "No such status")
		    (newline)
		    #f ) )) )		; stop iteration

       (define (msg-status status expected v)
	 (case status
	   ((expected-error)
	    "an ERROR was expected !!! ")
	   ((error-occurred)
	    "OK OK" )
	   ((expected-divergence)
	    "DIVERGES OK")
	   ((unexpected-error)
	    "an unexpected ERROR occured !!!")
	   ((interrupted-divergence)
	    "an unexpected ERROR occured !!!") 
	   ((return-non-divergence)
	    "divergence was expected.")
	   ((correct-result)
	    "OK")
	   ((incorrect-result)
	    "value expected:" )		
	   ((uninteresting-result)
	    "OK" )
	   (else (display "No such status")
		 (newline)
		 #f ) ) )
    (call/cc
     (lambda (exit)                     ; exit when test suite is finished
       (engine-tester 
        (lambda ()                      ; read test
          (let ((e (read in)))
            (if (eof-object? e)
                (begin
		  (close-input-port in)
		  (exit 'done)) )
            (display prompt-in)
            (display e)
            (newline)
            e ) )
        (lambda ()                      ; read result
          (let ((expected (read in))
		(testid #f)
		(marks 1))  ;;; by default one mark
	    (if (and (symbol? expected)
		     (equal? (string-ref (symbol->string expected) 0) #\$))
		(begin
		  (set! testid expected)
		  (set! expected (read in))))
	    (if (and (symbol? expected)
		     (equal? expected ':marks))
		(begin
		  (set! marks (read in))
		  (set! expected (read in))))
            (if (eof-object? expected)
                (tester-error "Missing expected result" expected) 
                (list expected testid marks) )))
        compare
	diverged?
        display-status
        make-toplevel
	(output-OK msg-status)
	(output-FAIL msg-status) ) ) )))

;;; A test engine on top of which the two previous are written:
;;;  (read-test)    reads an expression to evaluate
;;;  (read-result)  reads the expected result
;;;  (compare expected obtained)  compares what was obtained from what
;;;                 was expected. The value of `expected' can also 
;;;                 be *** or ---
;;;  (display-status message expected obtained marks) displays the result of the
;;;                 test. It usually prints the result and a comment like `OK'.
;;;  (make-toplevel read print error) returns a thunk implementing one step
;;;                 of the intepreter. 
;;; (diverged?) indicates if the test reached the bound on the length
;;;    of the computation
;;; (output-OK the-expression status expected result)
;;; (output-FAIL the-expression status expected result)

;;; This one is similar to the previous one, except that it does not
;;; terminate when a non expected behaviour occurs.
;;; Instead, it applies output-OK or ouput-FAIL, and continues

(define (engine-tester read-test        ; read a test
                       read-result      ; read the expected result
                       compare          ; compare the two
		       diverged?
		       display-status   ; display the comparison
		       make-toplevel   ; make a toplevel
		       output-OK       ;  
		       output-FAIL )    ;
  (call/cc 
   (lambda (abort)                      ; exit all tests
     (let ((resume 'wait))
       ;; compare the result V with what was expected. If that
       ;; function is called then no error ocurred (unless *** is
       ;; given to it simulating an error internally caught).
       (define (check-result p v)
         (let* ((expected+testid+marks (read-result))
		(expected (car expected+testid+marks))
		(testid (cadr expected+testid+marks))
		(marks (caddr expected+testid+marks)))
	   (if testid
	       (begin
		 (display "Test ")
		 (display testid)
		 (newline)))
           (let ((result (cond ((compare expected v)
				(display-status 'correct-result expected v marks) )
			       ((eq? expected '***)
				(display-status 'expected-error expected v marks) )
			       ((eq? expected '---)
				(display-status 'uninteresting-result expected v marks) )
			       ((eq? expected '+++)
				(display-status 'uninteresting-result expected v marks) )
			       ((eq? expected 'oo)
				(if (diverged?)
				    (display-status 'expected-divergence expected v marks)
				    (display-status 'return-non-divergence expected v marks)))
			       (else 
				(display-status 'incorrect-result expected v marks) ))))
	     (let ((success? (cdr result)))
	       (if success?
		   (output-OK p (car result) expected v)
		   (output-FAIL p (car result) expected v)))
	      (resume #t) ) )) 

       ;; This function is called whenever an error is detected.
       (define handle-exception
	 (lambda (p)
	   (lambda (msg . culprits)
	     ;;(write `(handle-exception called))(newline) ; DEBUG
	     (let* ((expected+testid+marks (read-result))
		    (expected (car expected+testid+marks))
		    (testid (cadr expected+testid+marks))
		    (marks (caddr expected+testid+marks))
		    (v        (cons msg culprits)))
	       (if testid
		   (begin
		     (display "Test ")
		     (display testid)
		     (newline)))
	       (let ((result (cond ((eq? expected '***)
				    (display-status 'error-occurred expected v marks) )
				   ((eq? expected 'oo)
				    (if (diverged?)
					(display-status 'expected-divergence expected v marks)
					(display-status 'interrupted-divergence expected v marks)))
				   ((eq? expected '+++)
				    (display-status 'uninteresting-result expected v marks) )
				   (else 
				    (display-status 'unexpected-error expected v marks) ) )))
		 (let ((success? (cdr result)))
		   (if success?
		       (output-OK p (car result) expected v)
		       (output-FAIL p (car result) expected v))))
		  (resume #t) ) )))
       (let ((toplevel (make-toplevel read-test 
                                      check-result 
                                      handle-exception )))
         (call/cc (lambda (k) (set! resume k)))
         ;;(write `(back to (toplevel)))(newline) ; DEBUG
         (let ((r (toplevel)))
           ;; if this error is triggered, see note below.
           (tester-error "(toplevel) should not return!" r) )
         ;; The previous lines capture `resume' only once and this is
         ;; better for Scheme->C which seems to have problems to
         ;; scavenge this sort of continuations. This is the same as the
         ;; the following (in Common Lisp): (loop toplevel)
         ) ) ) ) )

;;; Examples:
;;; Suppose you have written an interpreter called `evaluate', then the 
;;; following will start a toplevel loop. Errors detected in evaluate
;;; are supposed to call the `wrong' function.
;;;(define (scheme)
;;;  (interpreter "?? " " == " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)   ;; Errors in the interpreter calls wrong
;;;      (lambda () (print (evaluate (read)))) ) ) )
;;; The problem is that errors in the underlying system are not caught.
;;; Suppose at that time to have something to trap errors, say catch-error
;;; as in Mac-Lisp (it returns the result in a pair or the string that names 
;;; the error if any), then you can write:
;;;(define (scheme)
;;;  (interpreter "?? " " == " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)   ;; Errors in the interpreter calls wrong
;;;      (lambda () (let ((r (catch-error (evaluate (read)))))
;;;                   (if (pair? r) (print (car r))
;;;                       (error r) ) )) ) ) )

;;; NOTE: Both the print and error functions (in interpreter and
;;; suite-test) have a control effect. They restart a new toplevel
;;; iteration. So it is *important* not to forget to call them to
;;; reiterate the toplevel. If you return a value from toplevel
;;; without calling print or error, you'll get an internal error (ie
;;; an invocation of tester-error). [The reason lies with toplevel
;;; returning more than once in some concurrent interpreter I wrote].

;;; If you have a file containing a test suite, say suite.tst, then you 
;;; can try it with: 
;;;(define (test-scheme)
;;;  (suite-test "suite.tst"
;;;    "?? " "== " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)
;;;      (lambda ()
;;;         (print (eval (read))) ) )
;;;    equal? ) )
;;; Another comparison function could be:
;;;    (lambda (expected obtained)
;;;      (cond ((or (eq? obtained '---)(eq? obtained '***))
;;;             (equal? expected obtained) )
;;;            (else (member obtained expected)) ) )
;;; Other suggestions: tests and results can be read from two different files.
;;; You can use other compare functions such as member, set-equal? or even
;;; use pattern-matching. Here are the two lastly mentioned comparators.

;;; Compares if sets X and Y have the same (with equal?) elements.

(define (set-equal? x y)
  (define (remove-one item list)
    (if (pair? list)
        (if (equal? item (car list))
            (cdr list)
            (cons (car list) (remove-one item (cdr list))) )
        '() ) )
  (if (pair? x)
      (and (member (car x) y)
           (set-equal? (cdr x) (remove-one (car x) y)) )
      (null? y) ) )

;;; Compares if the expression fits the pattern. Two special patterns exist: 
;;;     ?-   which accepts anything
;;;     [?-]  which accepts a (possibly empty) sequence of anything.
;;; Otherwise comparisons are performed with equal?.

(define (naive-match pattern expression)
  (define (naive-match-list patterns expressions)
    (if (pair? patterns)
        (if (eq? (car patterns) '[?-]) ; accepts any sequence of things
            (or (naive-match-list (cdr patterns) expressions)
                (and (pair? expressions)
                     (naive-match-list patterns (cdr expressions)) ) )
            (and (pair? expressions)
                 (naive-match (car patterns) (car expressions))
                 (naive-match-list (cdr patterns) (cdr expressions)) ) )
        (naive-match patterns expressions) ) )
  (or (eq? pattern '?-)              ; accepts anything
      (if (pair? pattern)
          (naive-match-list pattern expression)
          (equal? pattern expression) ) ) )


;;; AGAIN A NOTE:
;;; To catch the errors of the underlying Scheme is difficult.

;;; This tester engine has been used for two years on a wide variety of
;;; interpreters, some of which are concurrent and return multiple results.

;;; end of tester.scm
