; /home/lavm/lecture-notes/cm315/scheme/eval-dist/primitives.scm created by Lavm on kronos on Tue Sep  8 09:20:43 1998 
; $Id: primitives.scm,v 1.5 1998/09/27 18:24:44 lavm Exp cm203l $ 

'(module primitives
        (export r4rs-global-environment!
		r5rs-global-environment!)
        (include "macros.scm")
        (import
         (core-cps "core-cps.scm")
	 (eps "eps.scm")))


(include "macros.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define r4rs-global-environment!
  (lambda (initial-env)

    (control-global-environment!
     (io-global-environment!
      (sequences-global-environment!
       (numbers-global-environment!
        (sexp-global-environment! initial-env)))))))

(define r5rs-global-environment!
  (lambda (initial-env)
    (r5control-global-environment!
     (control-global-environment!
      (io-global-environment!
       (sequences-global-environment!
	(numbers-global-environment!
	 (sexp-global-environment! initial-env))))))))
         


(define sexp-global-environment!
  (lambda (initial-env)

    ;;; boolean

    (define-primitive not 1 -)
    (define-primitive boolean? 1 -)

    ;;; equivalence predicate

    (define-primitive eqv? 2 -)
    (define-primitive eq? 2 -)
    (define-primitive equal? 2 (lambda (x y)
				 (and (not (cycle? x '()))
				      (not (cycle? y '())))))

    ;;; pairs and lists
    
    (define-primitive pair? 1 -)
    (define-primitive atom? 1 - (lambda (x)
				  (not (pair? x))))

    (define-primitive cons 2 - (lambda (x y)
				 (instrument 'cons 'cons 1)
				 (cons x y)))
    (define-primitive car 1 pair?)
    (define-primitive cdr 1 pair?)


    (define-primitive macro-car 1 pair?)
    (define-primitive macro-cdr 1 pair?)


    (define-primitive set-car! 2 (lambda (x y)
				   (pair? x))
      (lambda (x y)
	(instrument 'set-cxr! x y)
	(set-car! x y)
	unspecified-value))

    (define-primitive set-cdr! 2 (lambda (x y)
				   (pair? x))
      (lambda (x y)
	(instrument 'set-cxr! x y)
	(set-cdr! x y)
	unspecified-value))

    (define-primitive caar 1 (cxr? car car))
    (define-primitive cadr 1 (cxr? cdr car))
    (define-primitive cdar 1 (cxr? car cdr))
    (define-primitive cddr 1 (cxr? cdr cdr))
    (define-primitive caaar 1 (cxr? car car car))
    (define-primitive caadr 1 (cxr? cdr car car))
    (define-primitive cadar 1 (cxr? car cdr car))
    (define-primitive caddr 1 (cxr? cdr cdr car))
    (define-primitive cdaar 1 (cxr? car car cdr))
    (define-primitive cdadr 1 (cxr? cdr car cdr))
    (define-primitive cddar 1 (cxr? car cdr cdr))
    (define-primitive cdddr 1 (cxr? cdr cdr cdr))
    (define-primitive caaaar 1 (cxr? car car car car))
    (define-primitive caaadr 1 (cxr? cdr car car car))
    (define-primitive caadar 1 (cxr? car cdr car car))
    (define-primitive caaddr 1 (cxr? cdr cdr car car))
    (define-primitive cadaar 1 (cxr? car car cdr car))
    (define-primitive cadadr 1 (cxr? cdr car cdr car))
    (define-primitive caddar 1 (cxr? car cdr cdr car))
    (define-primitive cadddr 1 (cxr? cdr cdr cdr car))
    (define-primitive cdaaar 1 (cxr? car car car cdr))
    (define-primitive cdaadr 1 (cxr? cdr car car cdar))
    (define-primitive cdadar 1 (cxr? car cdr car cdr))
    (define-primitive cdaddr 1 (cxr? cdr cdr car cdr))
    (define-primitive cddaar 1 (cxr? car car cdr cdr))
    (define-primitive cddadr 1 (cxr? cdr car cdr cdr))
    (define-primitive cdddar 1 (cxr? car cdr cdr cdr))
    (define-primitive cddddr 1 (cxr? cdr cdr cdr cdr))
    (define-primitive null? 1 -)
    (define-primitive list? 1 -)
    (define-primitive list -1 - (lambda (args)
				  (instrument 'cons 'list (length args))
				  args))

    (define-primitive cons* -2 - (lambda (arg1 args)
				   (wrong 'cons*
					  "is not supported"
					  (cons arg1 args))))

    (define-primitive head 1 pair? (lambda (x) (car x)))


    (define-primitive length 1 list?)
    (define-primitive append -1 (lambda (args)
				  (and-map list? (butlast args)))
                                (lambda (args)
				  (for-each (lambda (arg)
					      (instrument 'cons 'append (length arg)))
					    (butlast args))
                                  (apply append args)))
    (define-primitive reverse 1 list? (lambda (l)
					(instrument 'cons 'reverse (length l))
					(reverse l)))
    (define-primitive list-ref 2 (lambda (x y)
				   (and (list? x)
					(integer? y)
					(>= y 0)
					(> (length x) y))))
    (define-primitive memq 2 (lambda (x y)
			       (list? y)))
    (define-primitive memv 2 (lambda (x y)
			       (list? y)))
    (define-primitive member 2 (lambda (x y)
				 (list? y)))
    (define-primitive assq 2 (lambda (x y)
			       (alist? y)))
    (define-primitive assv 2 (lambda (x y)
			       (alist? y)))
    (define-primitive assoc 2 (lambda (x y)
				(alist? y)))


    ;;; symbols
    
    (define-primitive symbol? 1 -)
    (define-primitive symbol->string 1 symbol?)
    (define-primitive string->symbol 1 string?)

    initial-env))


(define numbers-global-environment!
  (lambda (initial-env)

    ;;; numbers

    (define-primitive number? 1 -)
    (define-primitive real? 1 -)
    (define-primitive rational? 1 -)
    (define-primitive integer? 1 -)

    (define-primitive = -3 numbers-3?
			   (lambda (a1 a2 args)
                             (apply = a1 a2 args)))
    (define-primitive < -3 numbers-3?
                           (lambda (a1 a2 args)
                             (apply < a1 a2 args)))
    (define-primitive <= -3 numbers-3?
                            (lambda (a1 a2 args)
                              (apply <= a1 a2 args)))
    (define-primitive > -3  numbers-3?
                            (lambda (a1 a2 args)
                              (apply > a1 a2 args)))
    (define-primitive >= -3 numbers-3?
                            (lambda (a1 a2 args)
                              (apply >= a1 a2 args)))

    (define-primitive zero? 1 number?)
    (define-primitive positive? 1 number?)
    (define-primitive negative? 1 number?)
    (define-primitive odd? 1 number?)
    (define-primitive even? 1 number?)

    (define-primitive max -2 numbers-2?
                             (lambda (a args)
                               (apply max a args)))
    (define-primitive min -2 numbers-2?
                             (lambda (a args)
                               (apply min a args)))
    
    (define-primitive + -1 numbers-1?
                           (lambda (args)
                               (apply + args)))
    (define-primitive * -1 numbers-1?
                           (lambda (args)
			     (apply * args)))

    (define-primitive - -2 numbers-2?
                           (lambda (a args)
                             (if (null? args)
                                 (- 0 a)
                                 (- a (car args))))) ;;;; strictly speaking - accepts one or two arguments
    (define-primitive / -2 (lambda (a args)
			     (and (numbers-2? a args)
				  (if (not (null? args))
				      (not (zero? (car args)))
				      #t)))
                           (lambda (a args)
                             (if (null? args)
                                 (/ 1 a)
                                 (/ a (car args))))) ;;;; strictly speaking / accepts one or two arguments

    (define-primitive abs 1 number?)
    (define-primitive quotient 2 numbers2?)
    (define-primitive remainder 2 numbers2?)
    (define-primitive modulo 2 numbers2?)

    (define-primitive gcd -2 numbers-2?
                             (lambda (a args)
                               (apply gcd a args)))
    (define-primitive lcm -2 numbers-2?
                             (lambda (a args)
                               (apply lcm a args)))

                                        ;(define-primitive numerator 1)
                                        ;(define-primitive denominator 1)

    (define-primitive exp 1 number?)
    (define-primitive log 1 number?)
    (define-primitive sin 1 number?)
    (define-primitive cos 1 number?)
    (define-primitive tan 1 number?)
    (define-primitive asin 1 number?)
    (define-primitive acos 1 number?)
    (define-primitive atan 1 number?);; **

    (define-primitive sqrt 1 number?)
    (define-primitive expt 2 numbers2?)

    initial-env))



  
(define string-space-remove 
  (lambda (s)
    (define (filter p l)
      (cond ((null? l) '())
	    ((p (car l)) (cons (car l) (filter p (cdr l))))
	    (else (filter p (cdr l)))))
    
    (define (simplify x ) 
      (list->string (filter (lambda (char)
			      (and (not (char-whitespace? char))
				   (not (equal? char #\newline))
				   (not (equal? char #\tab))))
			    (string->list x))))
    (simplify s)))

(define sequences-global-environment!
  (lambda (initial-env)

    (define-primitive number->string 1 number?);;**
    (define-primitive string->number 1 string?);;**

    ;;; characters

    (define-primitive char? 1 -)

    (define-primitive char=? 2 chars2?)
    (define-primitive char<? 2 chars2?)
    (define-primitive char<=? 2 chars2?)
    (define-primitive char>? 2 chars2?)
    (define-primitive char>=? 2 chars2?)

    (define-primitive char-ci=? 2 chars2?)
    (define-primitive char-ci<? 2 chars2?)
    (define-primitive char-ci<=? 2 chars2?)
    (define-primitive char-ci>? 2 chars2?)
    (define-primitive char-ci>=? 2 chars2?)

    (define-primitive char-alphabetic? 1 char?)
    (define-primitive char-numeric? 1 char?)
    (define-primitive char-whitespace? 1 char?)
    (define-primitive char-upper-case? 1 char?)
    (define-primitive char-lower-case? 1 char?)


    (define-primitive char->integer 1 char?)
    (define-primitive integer->char 1 integer?)

    (define-primitive char-upcase 1 char?)
    (define-primitive char-downcase 1 char?)

    ;;; strings

    (define-primitive string? 1 -)
    (define-primitive make-string -2 (lambda (a args)
				       (and (integer? a)
					    (or (null? args)
						(char? (car args)))))
				     (lambda (a args)
                                       (if (null? args)
                                           (make-string a)
                                           (make-string a (car args)))));;;!!
    (define-primitive string -1 (lambda (args)
				  (and-map char? args))
                                (lambda (args)
                                  (apply string args)))
    (define-primitive string-length 1 string?)
    (define-primitive string-ref 2 (lambda (a b)
				     (and (string? a)
					  (integer? b)
					  (> (string-length a) b))))
    (define-primitive string-set! 3 (lambda (a b c)
				      (and (string? a)
					   (integer? b)
					   (> (string-length a) b)
					   (char? c))))

    (define-primitive string-space-remove 1 -)

    (define-primitive string=? 2 strings2?)
    (define-primitive string-ci=? 2 strings2?)
    (define-primitive string<? 2 strings2?)
    (define-primitive string>? 2 strings2?)
    (define-primitive string<=? 2 strings2?)
    (define-primitive string>=? 2 strings2?)
    (define-primitive string-ci<? 2 strings2?)
    (define-primitive string-ci>? 2 strings2?)
    (define-primitive string-ci<=? 2 strings2?)
    (define-primitive string-ci>=? 2 strings2?)

    (define-primitive list->string 1 -)
    (define-primitive string->list 1 - (lambda (s)
					 (let ((res (string->list s)))
					   (instrument 'cons 'string-list (length res))
					   res)))

    (define-primitive substring 3 (lambda (x y z)
				    (and (string? x)
					 (integer? y)
					 (integer? z))))
    (define-primitive string-append -1 (lambda (args)
					 (and-map string? args))
                                       (lambda (args)
                                         (apply string-append args)))

    ;;; vectors

    (define-primitive vector? 1 -)
    (define-primitive make-vector -2 (lambda (a args)
				       (integer? a))
                                     (lambda (a args)
                                       (if (null? args)
                                           (make-vector a)
                                           (make-vector a (car args)))))

    (define-primitive vector -1 - (lambda (args)
                                  (apply vector args)))
    (define-primitive vector-length 1 vector?)
    (define-primitive vector-ref 2 (lambda (x y)
				     (and (vector? x)
					  (integer? y)
					  (> (vector-length x) y))))
    (define-primitive vector-set! 3 (lambda (x y z)
				      (and (vector? x)
					   (integer? y)
					   (> (vector-length x) y))))

    initial-env))



(define io-global-environment!
  (lambda (initial-env)

    (define-primitive procedure? 1 - procedure-primitive?)

    (define-k-primitive for-each -2 (lambda (f l)
				      (and (procedure-primitive? f)
					   (and-map list? l)))
				    (lambda (cont f args)
				      (for-each-primitive f args cont)))
    (define-k-primitive map      -2 (lambda (f l)
				      (and (procedure-primitive? f)
					   (and-map list? l)))
                                    (lambda (cont f args)
				      (map-primitive f args cont)))

    (define-k-primitive append-map      -2 (lambda (f l)
				      (and (procedure-primitive? f)
					   (and-map list? l)))
                                    (lambda (cont f args)
				      (append-map-primitive f args cont)))


    ;;; I/O

    (define-primitive input-port? 1 -)
    (define-primitive output-port? 1 -)
    (define-primitive current-input-port 0 -)
    (define-primitive current-output-port 0 -)
    (define-primitive open-input-file 1 string?)
    (define-primitive open-output-file 1 string?)
    (define-primitive close-input-port 1 port?)
    (define-primitive close-output-port 1 port?)

    (define-primitive eof-object? 1 -)
    (define-primitive read -1 (lambda (args)
				(and-map port? args))
			      (lambda (args)
                                (apply read args)));; **
    (define-primitive read-char -1 (lambda (args)
				     (and-map port? args))
                                   (lambda (args)
                                     (apply read-char args)));; **
    (define-primitive peek-char -1 (lambda (args)
				     (and-map port? args))
                                   (lambda (args)
                                     (apply peek-char args)));; **

    (define-primitive write -2 (lambda (a args)
				 (and (and-map port? args)
				      (not (cycle? a '()))))
                               (lambda (a args)
                                 (apply write a args)
				 unspecified-value));; **
    (define-primitive display -2 (lambda (a args)
				   (and (and-map port? args)
					(not (cycle? a '()))))
                                 (lambda (a args)
                                   (apply display a args)
				   unspecified-value));; **
    (define-primitive newline -1 (lambda (args)
				   (and-map port? args))
                                 (lambda (args)
                                   (apply newline args)
				   unspecified-value));; **
    (define-primitive write-char -2 (lambda (a args)
				      (and (char? a)
					   (and-map port? args)))
                                    (lambda (a args)
                                      (apply write-char a args)
				      unspecified-value));; **

    (define-k-primitive load 1 (lambda (filename)
				  (string? filename))
                                load-primitive)


    initial-env))


(define control-global-environment!
  (lambda (initial-env)

    (define-k-primitive call/cc 1 procedure-primitive?
      (lambda (cont receiver)
        (apply-cps receiver
                   (list (make-escape-procedure cont))
                   cont)))

    (define-k-primitive force 1 procedure-primitive?
      (lambda (cont fun)
        (apply-cps fun
                   '()
                   cont)))


    (define-k-primitive tail 1 pair?
      (lambda (cont stream)
        (apply-cps (cdr stream)
                   '()
                   cont)))


    (define-k-primitive eval 1 -
      (lambda (cont exp)
        (eval-cps (embed-expand exp)
		  initial-env
		  cont)))

;    (define-k-primitive call-with-current-continuation call/cc)
    (define-k-primitive apply -3 (lambda (f l)
				   (procedure-primitive? f))

      (lambda (cont proc arguments rest)
;        (if (null? arguments)
;            (wrong 'apply-k-primitive-procedure
;                   "wrong number of arguments for apply"
;                   arguments))
        (letrec ((loop (lambda (l)
                         (if (null? (cdr l))
                             (car l)
                             (cons (car l) (loop (cdr l)))))))
          (apply-cps proc
                     (loop (cons arguments rest))
                     cont))))

    (define-k-primitive error -1 -
      (lambda (cont rest)
        (apply wrong rest)))
    
    (define-primitive instrument 3 -)

    initial-env

    ))




(define chars2?
  (lambda (a b)
    (and (char? a)
	 (char? b))))
(define numbers2?
  (lambda (a b)
    (and (number? a)
	 (number? b))))
(define strings2?
  (lambda (a b)
    (and (string? a)
	 (string? b))))


(define numbers-1?
  (lambda (args)
    (and-map number? args)))

(define numbers-2?
  (lambda (a args)
    (and-map number? (cons a args))))

(define numbers-3?
  (lambda (a1 a2 args)
    (and-map number? (cons a1 (cons a2 args)))))




(define r5control-global-environment!
  (lambda (initial-env)
    (define-k-primitive values -1           - return-values)
    (define-k-primitive call-with-values 2  - call/values)

    initial-env))


; end of primitives.scm 

