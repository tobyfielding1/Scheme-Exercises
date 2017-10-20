; /home/lavm/lecture-notes/cm315/scheme/eval-dist/acont.scm created by Lavm on gadget.ecs.soton.ac.uk on Tue Sep 15 21:00:46 1998 
; $Id: acont.scm,v 1.1 1998/09/24 19:38:30 lavm Exp $ 

;;; We define here a macro to represent continuations.
;;; The macro can be expanded in a regular lambda expression
;;; or it can be expanded into a data structure.

;;; The interest is that the same code is used to generate
;;; abstract cps or regular cps.

;;; For instance, the following continuation
;;;  (Clambda (args)
;;;     rand : (funct cont)
;;;    (apply-cps funct args cont))
;;; can be expanded into
;;;  (lambda (args)
;;;    (apply-cps funct args cont))
;;; or into
;;; (make-rand funct cont)
;;; and a function
;;; (define resume-rand
;;;   (lambda (args c)
;;;     (apply-cps (rand-funct c) args (rand-cont c))))


(define symbol-append
  (lambda l
    (string->symbol (apply string-append (map symbol->string l)))))

(define-macro (Clambda args name colon closed-vars . body)
  (define symbol-append
    (lambda l
      (string->symbol (apply string-append (map symbol->string l)))))

  (if (not (eq? colon ':))
      (error 'expanding-Clambda
	     "incorrect syntax for Clambda"
	     colon))
  (let ((resume-name (symbol-append 'resume- name))
	(make-name   (symbol-append 'make-acont- name))
	(predicate-name (symbol-append 'acont- name '?))
	(accessors (map (lambda (x)
			  (symbol-append 'acont- name '- x))
			closed-vars))
	(c (gensym)))

    `(begin
       (define-global! ,resume-name
	 (lambda (,c . ,args)
	   (let ,(if (null? closed-vars)
		     (list (list c c)) ;; dummy binding 'cos let must have at least one!
		     (map (lambda (x y)
			    (list x (list y c)))
			  closed-vars
			  accessors))
	     ,@body)))

       (define-global! ,make-name
	 (lambda ,closed-vars
	   (make-acont ',name ,@closed-vars)))

       (define-global! ,predicate-name
	 (test-acont? ',name))

       ,@(let loop ((n 2) ;; 0 is for acont-tag, 1 for specific tag
		    (l accessors))
	   (if (null? l)
	       '()
	       (cons (list 'define-global! (car l)
			   (list 'lambda '(x)
				 (list 'list-ref 'x n)))
		     (loop (+ n 1) (cdr l)))))

       (,make-name ,@closed-vars)  )))

;;; Just redefining the behaviour of the continuation, without declaring all the other stuff.

(define-macro (Clambda-behaviour args name colon closed-vars . body)
  (if (not (eq? colon ':))
      (error 'expanding-Clambda
	     "incorrect syntax for Clambda"
	     colon))
  (let ((resume-name (symbol-append 'resume- name))
	(accessors (map (lambda (x)
			  (symbol-append 'acont- name '- x))
			closed-vars))
	(c (gensym)))

    `(set-global! ,resume-name
       (lambda (,c . ,args)
	 (let ,(if (null? closed-vars)
		   (list (list c c));; dummy binding 'cos let must have at least one!
		   (map (lambda (x y)
			  (list x (list y c)))
			closed-vars
			accessors))
	   ,@body)))))

(define-macro (define-global! var val)
  (begin
    (set! *global-definitions*
	  (cons (list 'define var val)
		*global-definitions*))
    `',var))

(define-macro (set-global! var val)
  (begin
    (set! *global-definitions*
	  (cons (list 'set! var val)
		*global-definitions*))
    `',var))

;(define *global-definitions* '())

(define-macro (init-global-definitions)
  (eval '(define *global-definitions* '()))
  0)

(define-macro (process-global-definitions)
  (if (null? *global-definitions*)
      0
      (let ((l *global-definitions*))
	(set! *global-definitions* '())
	`(begin
	   ,@l
	   (process-global-definitions)
	   ))))

;(pp (expand '(Clambda (args)
;		      rand : (funct cont)
;		      (apply-cps funct args cont))))

;(pp (expand '(process-global-definitions)))

;(init-global-definitions)


  
; end of acont.scm 

