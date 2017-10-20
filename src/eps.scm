; /home/lavm/lecture-notes/cm315/scheme/eps/eps.scm created by Lavm on kronos on Tue Aug  5 17:56:09 1997 
; $Id: eps.scm,v 1.1 1998/09/24 19:38:39 lavm Exp $ 

; $Id: eps.scm,v 1.1 1998/09/24 19:38:39 lavm Exp $ 

;;; Expansion passing style: directly copied from
 ;;; Dybvig, Friedman, Haynes's paper

'(module eps
	(export embed-expand initial-expander macro-car macro-cdr))

(define *embed-expansion-table* '())
(define embed-expand
  (lambda (x)
    (initial-expander x initial-expander)))

(define *eps-super-debug* #f)
(define *eps-debug* #f)

(define initial-expander
  (lambda (x e)
    (if *eps-super-debug*
	(print "Expanding " x))
    (let ((e1 (cond
	       ((symbol? x) *identifier-expander*)
	       ((not (pair? x)) (lambda (x e) x))
	       ((expander? (car x)) => (lambda (x) x))
	       (else *application-expander*))))
      (e1 x e))))

(define embed-expand-once
  (lambda (x)
    (initial-expander x (lambda (x e) x))))

(define *identifier-expander* (lambda (x e) x))
(define *application-expander*
  (lambda (x e)
    (map (lambda (x)
	   (e x e))
	 x)))

(define basic-embed-install-expander
  (lambda (keyword function)
    (let ((res (assq keyword *embed-expansion-table*)))
      (if (not res)
	  (set! *embed-expansion-table*
		(cons (cons keyword function)
		      *embed-expansion-table*))
	  (set-cdr! res function)))))

(define embed-install-expander
  (lambda (keyword function)
    (basic-embed-install-expander keyword
				 (lambda (x e)
				   (if *eps-debug*
				       (print "Expander for " keyword))
				   (if *eps-super-debug*
				       (print "       on " x))
				   (function x e)))))

(define expander?
  (lambda (x)
    (and (symbol? x)
	 (let ((res (assq x *embed-expansion-table*)))
	   (and res
		(cdr res))))))

(embed-install-expander 'lambda
  (lambda (x e)
    `(lambda ,(cadr x)
       ,@(map (lambda (x) (e x e)) (cddr x)))))

(embed-install-expander 'quote
  (lambda (x e)
    x))


(embed-install-expander 'quasiquote
   (lambda (e r)
     (define (walk e)
       (if (pair? e)
           (if (eq? (car e) 'unquote)
               (cadr e)
               (if (eq? (car e) 'quasiquote)
                   (syntax-error "No embedded quasiquotation" 'quasiquote e)
                   (walk-pair e) ) )
           (list 'quote e) ) )
     (define (walk-pair e)
       (if (pair? (car e))
           (if (eq? (car (car e)) 'unquote-splicing)
               (list 'append
                     (cadr (car e))
                     (walk (cdr e)) )
               (list 'cons
                     (walk (car e))
                     (walk (cdr e)) ) )
           (list 'cons
                 (list 'quote (car e))
                 (walk (cdr e)) ) ) )
     (r (walk (cadr e)) r) ) )

(embed-install-expander 'define-macro
  (lambda (x e)
    (let* ((call (cadr x))
	   (keyword (car call))
	   (pattern (cdr call))
	   (body (cddr x)))
      (e `(install-non-pervasive-expander ',keyword
	    ,(make-macro pattern body))
	 e))))


(define-macro (embed-define-macro call . body)
  (define make-macro
    (lambda (pat body)
      `(lambda (x e)
	 (e (let ,(destructure 'x pat '(cdr x) '())
	      ,@body)
	    e))))
  (define destructure
    (lambda (name pat arg bindings)
      (cond ((null? pat) bindings)
	    ((symbol? pat) (cons `(,pat ,arg) bindings))
	    ((pair? pat)
	     (destructure name
			  (car pat)
			  `(macro-car ,name ,arg)
			  (destructure name
				       (cdr pat)
				       `(macro-cdr ,name ,arg)
				       bindings))))))

  (let ((keyword (car call))
	(pattern (cdr call)))
    `(embed-install-expander ',keyword
       ,(make-macro pattern body))))
       
       
;;; some macros for the embedded evaluator


;;; for now, ignore define-syntax
(embed-define-macro (define-syntax . body)
  0)

(embed-define-macro (let args . body)
  (if (null? args)
      (cons 'begin body)
      (if (symbol? args)
	  (expand-named-let args (car body) (cdr body))
	  (cons (cons 'lambda
		      (cons (map (lambda (x)
				   (if (pair? x)
				       (car x)
				       x))
				 args)
			    body))
		(map (lambda (x)
		       (if (pair? x)
			   (cadr x)
			   (list 'quote 'any)))  ;;unspecified-value
		     args)))))

(define expand-named-let
  (lambda (name args body)
    (list 'letrec
	  (list (list name (cons 'lambda
			   (cons (map (lambda (x)
					(if (pair? x)
					    (car x)
					    x))
				      args)
				 body))))
	  (cons name
		(map (lambda (x)
		       (if (pair? x)
			   (cadr x)
			   (list 'quote 'any)))  ;;unspecified-value
		     args)))))

  
(embed-define-macro (letrec bindings . body)
  (let ((vars (map (lambda (b) (gensym (car b))) bindings)))
    (cons 'let
	  (cons (map (lambda (binding)
		       (list (car binding) (list 'quote 'any)))
		     bindings)
		(list (cons 'let
			    (cons (map (lambda (var binding)
					 (list var (cadr binding)))
				       vars bindings)
				  (cons (cons 'begin
					      (map (lambda (var binding)
						     (list 'set! (car binding) var))
						   vars bindings))
					body))))))))

(embed-define-macro (let* args . body)
  (if (null? args)
      (cons 'begin body)
      (list 'let
	    (list (car args))
	    (cons 'let*
		  (cons (cdr args) body)))))

(embed-define-macro (if pred then . else)
  (cons 'cond
	(cons (list pred then)
	      (if (null? else)
		  '()
		  (list (cons 'else else))))))

(embed-define-macro (cons-stream a d)
  (list 'cons a (list 'delay d)))

(embed-define-macro (delay e)
  (list 'lambda '() e))    ;;; no lazy???

(embed-define-macro (and . rest)
  (if (null? rest)
      #t
      (if (null? (cdr rest))
	  (car rest)
	  (let ((v (gensym)))
	    (list 'let
		  (list (list v (car rest)))
		  (list 'if
			v
			(cons 'and (cdr rest))
			#f))))))

(embed-define-macro (or . rest)
  (if (null? rest)
      #f
      (let ((v (gensym)))
	(list 'let
	      (list (list v (car rest)))
	      (list 'if
		    v
		    v
		    (cons 'or (cdr rest)))))))



(embed-define-macro (do args condition . body)
  (let ((loop (gensym)))
    (list 'letrec (list (list loop (list 'lambda
					 (map car args)
					 (list 'if (car condition)
					       (cadr condition)
					       (append (list 'begin)
						       body
						       (list (cons loop
								   (map (lambda (triple)
									  (if (null? (cddr triple))
									      (car triple)
									      (caddr triple)))
									args))))))))
	  (cons loop (map cadr args)))))
					       


(embed-install-expander 'define-pervasive-macro
  (lambda (x e)
    (let* ((call (cadr x))
	   (keyword (car call))
	   (pattern (cdr call))
	   (body (cddr x)))
      (e `(install-pervasive-expander ',keyword
	    ,(make-macro pattern body))
	 e))))

(embed-install-expander 'install-non-pervasive-expander
  (lambda (x e)
    (let ((name (cadr (cadr x)))
	  (expanser (caddr x)))
      (if *eps-debug*
	  (print "[Installing " name "]"))
      (embed-install-expander name
	(eval expanser))
      (e 0
	 e))))

(embed-install-expander 'install-pervasive-expander
  (lambda (x e)
    (let ((name (cadr (cadr x)))
	  (expanser (caddr x)))
      (if *eps-debug*
	  (print "[Installing " name "]"))
      (embed-install-expander name
	(eval expanser))
      (e `(runtime-install-expander ',name
				    ,expanser)
	 e))))


(define make-macro
  (lambda (pat body)
    `(lambda (x e)
       (e (let ,(destructure 'x pat '(cdr x) '())
	    ,@body)
	  e))))



(define destructure
  (lambda (name pat arg bindings)
    (cond ((null? pat) bindings)
	  ((symbol? pat) (cons `(,pat ,arg) bindings))
	  ((pair? pat)
	   (destructure name
			(car pat)
			`(car ,arg)   ;;; `(macro-car ,name ,arg) ???
			(destructure name
				     (cdr pat)
				     `(cdr ,arg) ;;; `(macro-cdr ,name ,arg)
				     bindings))))))
(define macro-car
  (lambda (name exp)
    (if (pair? exp)
	(car exp)
	(syntax-error "When accessing car of " name exp))))

(define macro-cdr
  (lambda (name exp)
    (if (pair? exp)
	(cdr exp)
	(syntax-error "When accessing cdr of " name exp))))

(define syntax-error
  (lambda (str val1 val2)
    (newline)
    (display "While expansing ")
    (display val1)
    (error 'syntax str val2)))
	   

(define *initial-expansion-table*
  (append *embed-expansion-table* '()))

;(embed-expand-once '(define-macro (luc a b c d) `(list a b c d)))
			
;(destructure '(a b c d) 'a '())
;(make-macro '(a b c d) '(`(list a b c d)))
			    
;(embed-expand-once '(let*  ((a 2)
;			   (b (+ a 4))
;			   (a (+ a 2)))
;		     (+ a b)))

;(embed-expand-once '(do ((a b (+ a 1))
;			(d e))
;		       ((> x 0) k)
;		     do1
;		     do2))



; end of eps.scm 

