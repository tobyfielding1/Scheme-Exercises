; /home/lavm/lecture-notes/cm315/scheme/eval-eng/core-cps.scm created by Lavm on kronos on Fri Aug  8 14:27:51 1997 
; $Id: core-cps.scm,v 1.6 1998/09/27 20:07:06 lavm Exp $ 

'(module core-cps
        (export evaluate eval-cps apply-cps  test-first-element atom?
		return resume
		*bound*
		*count*
                extend-environment lookup-variable-value define-variable!
                make-escape-procedure make-closure
		lambda-body
		cycle?
                map-primitive
                append-map-primitive
                for-each-primitive
		procedure-primitive?
		load-primitive
		quoted?
		unspecified-value
		uninitialised
		cxr?
		and-map
		butlast
		alist?
		lambda-parameters
		;closure-function-parameters
		;closure-function-body
		closure-lambda
		lambda?
		compound-procedure?
                wrong user-print
                error-continuation
                match-arity?
                make-primitive
                make-k-primitive
                *the-global-environment*
                *primitive-vector*
		test-acont?
		make-acont
		resume-core
		escape-procedure-tag
		k-primitive-tag
		primitive-tag
		procedure-tag
		return-values
		call/values
                banner)
	(include "acont.scm")
        (import 
         (eps "eps.scm")))

(include "acont.scm")


(init-global-definitions)

(define unspecified-value (string->symbol "<unspecified-value>"))

(define uninitialised (string->symbol "<uninitialised-value>"))


(define banner uninitialised)

(define eval-cps
  (lambda (exp env cont)
    (cond
     ((self-evaluating? exp) (return exp cont))
     ((quoted? exp) (return (text-of-quotation exp) cont))
     ((variable? exp) (lookup-variable-value exp env cont))
     ((definition? exp)  (eval-definition-cps exp env cont))
     ((assignment? exp)  (eval-assignment-cps exp env cont))
     ((sequence? exp) (eval-sequence-cps (cdr exp) env cont))
     ((lambda? exp) (return (make-closure exp env) cont))
     ((conditional? exp) (eval-cond-cps (clauses exp) env cont))
     ((application? exp)
      (eval-cps (operator exp)
                env
                (Clambda (funct)
		  rator : (exp env cont)
                  (list-of-values-cps (operands exp)
                                      env
                                      (Clambda (args)
					rand : (funct cont)
                                        (apply-cps funct args cont))))))
     (else (wrong 'eval-cps "Unknown expression type -- EVAL-CPS" exp)))))


;;; test-first-element is a one argument function. Its value is a
;;; function that expects un argument (an S-expression).  The latter
;;; function is a predicate that indicates if its argument is a pair
;;; whose first element was given to test-first-element

(define test-first-element
  (lambda (sym)
    (lambda (exp)
      (if (atom? exp)
          #f
          (eq? (car exp) sym)))))

;;; accesseurs et predicats pour les expressions quotees, self-evaluating,
;;; les variables et pour les lambda


(define self-evaluating?
  (lambda (exp)
    (or (number? exp) (boolean? exp) (char? exp) (string? exp))))


(define quoted?
  (lambda (x)
    (or ((test-first-element 'quote) x)
	((test-first-element 'quasiquote) x))))

(define text-of-quotation
  (lambda (exp) (cadr exp)))

(define variable?
  (lambda (exp) (symbol? exp)))

(define lambda? (test-first-element 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

;;; accesseurs et predicats pour une application

(define application?
  (lambda (exp) (not (atom? exp))))

(define operator
  (lambda (app) (car app)))

(define operands
  (lambda (app) (cdr app)))

(define no-operands?
  (lambda (args) (null? args)))

(define first-operand
  (lambda (args) (car args)))

(define rest-operands
  (lambda (args) (cdr args)))

;;; application d'une fonction a une liste d'arguments deja evalues

(define apply-cps
  (lambda (procedure arguments cont)
    (cond ((escape-procedure? procedure)
           (apply-escape-procedure procedure arguments))
          ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments cont))
          ((k-primitive-procedure? procedure)
           (apply-k-primitive-procedure procedure arguments cont))
          ((compound-procedure? procedure)
	   (let ((lambda-exp (closure-lambda procedure)))
	     (extend-environment
	      (lambda-parameters lambda-exp)
	      arguments
	      (closure-environment procedure)
	      (Clambda (new-env)
		extend-env : (lambda-exp cont)
		(eval-sequence-cps (lambda-body lambda-exp)
				   new-env
				   cont)))))
          (else
           (wrong 'apply-cps "Unknown procedure type -- APPLY-CPS" procedure)))))

;;; evaluation d'une liste d'arguments dans un meme environnement

(define list-of-values-cps
  (lambda (exps env cont)
    (cond
     ((no-operands? exps) (return '() cont))
     (else (eval-cps (first-operand exps)
                     env
                     (Clambda (first-value)
		       lof-first : (exps env cont)
                       (list-of-values-cps (rest-operands exps)
                                           env
                                           (Clambda (rest-values)
					     lof-rest : (first-value cont)
                                             (return (cons first-value rest-values)
                                                     cont)))))))))

;;; Representation des objets closure

(define-type closure lambda ( environment unprintable:))

;; (define make-closure
;;   (lambda (lambda-exp env)
;;     (list procedure-tag lambda-exp env)))

;; (define procedure-tag (list 'procedure))

;; (define compound-procedure? (test-first-element procedure-tag))

;; ;(define closure-function-parameters
;; ;  (lambda (proc) (cadr (cadr proc))))

;; ;(define closure-function-body
;; ;  (lambda (proc) (cddr (cadr proc))))

;; (define closure-lambda
;;   (lambda (proc) (cadr proc)))
;; (define closure-environment
;;   (lambda (proc) (caddr proc)))


(define compound-procedure? closure?)

;;; evaluation d'une forme conditionnelle

(define eval-cond-cps
  (lambda (clist env cont)
    (cond
     ((no-clauses? clist) (return unspecified-value cont))
     ((else-clause? (first-clause clist))
      (eval-sequence-cps (actions (first-clause clist)) env cont))
     (else (eval-cps (predicate (first-clause clist))
                     env
                     (Clambda (predicate-value)
		       cond : (clist env cont)
                       (cond
                        (predicate-value (eval-sequence-cps (actions (first-clause clist))
                                                            env
                                                            cont))
                        (else (eval-cond-cps (rest-clauses clist)
                                             env
                                             cont)))))))))

;;; accesseurs et predicats relatifs a la forme conditionnelle

(define conditional? (test-first-element 'cond))

(define clauses
  (lambda (exp) (cdr exp)))

(define no-clauses?
  (lambda (clauses) (null? clauses)))

(define first-clause
  (lambda (clauses) (car clauses)))

(define rest-clauses
  (lambda (clauses) (cdr clauses)))

(define predicate
  (lambda (clause) (car clause)))

(define actions
  (lambda (clause) (cdr clause)))

(define true?
  (lambda (x) (not (eq? x #f))))

(define else-clause?
  (lambda (clause)
    (eq? (predicate clause) 'else)))

;;;  evaluation d'une sequence d'expressions

(define eval-sequence-cps
  (lambda (exps env cont)
    (cond
     ((last-exp? exps) (eval-cps (first-exp exps) env cont))
     (else (eval-cps (first-exp exps)
                     env
                     (Clambda (first-value)
		       seq : (exps env cont)
                       (eval-sequence-cps (rest-exps exps)
                                          env
                                          cont)))))))

;;;  accesseurs et predicats relatifs aux sequences

(define sequence? (test-first-element 'begin))

(define last-exp?
  (lambda (seq) (null? (cdr seq))))

(define first-exp
  (lambda (seq) (car seq)))

(define rest-exps
  (lambda (seq) (cdr seq)))

;;;  evaluation d'une forme d'assignation

(define eval-assignment-cps
  (lambda (exp env cont)
    (eval-cps (assignment-value exp)
              env
              (Clambda (new-value)
		set : (exp env cont)
                (set-variable-value! (assignment-variable exp)
                                     new-value
                                     env
				     cont)))))

;;; accesseurs et predicat relatifs a l'assignation

(define assignment? (test-first-element 'set!))

(define assignment-variable
  (lambda (exp) (cadr exp)))

(define assignment-value
  (lambda (exp) (caddr exp)))

;;;  evaluation d'une definition

(define eval-definition-cps
  (lambda (exp env cont)
    (eval-cps (definition-value exp)
              env
              (Clambda (value)
		define : (exp env cont)
                (define-variable! (definition-variable exp)
                  value
                  env)
                (return unspecified-value cont)))))

;;; accesseurs et predicat pour une definition

(define definition? (test-first-element 'define))

(define definition-variable
  (lambda (exp)
    (if (variable? (cadr exp))
        (cadr exp)
        (caadr exp))))

(define definition-value
  (lambda (exp) 
    (if (variable? (cadr exp))
        (caddr exp)
        (cons 'lambda
              (cons (cdadr exp)         ;formal parameters
                    (cddr exp))))))     ;body


(define make-escape-procedure
  (lambda (cont)
    (list escape-procedure-tag cont)))
(define escape-procedure-tag (list 'escape-procedure))
(define escape-procedure?
  (lambda (obj)
    (and (pair? obj)
         (eq? (car obj) escape-procedure-tag))))
(define escape-procedure->cont
  (lambda (obj)
    (cadr obj)))



;;; Operations sur les environnements 

(define lookup-variable-value
  (lambda (var env cont)
    (let ((b (binding-in-env var env)))
      (if (found-binding? b)
          (return (binding-value b) cont)
          (wrong 'lookup-variable-value "Unbound variable" var)))))



(define binding-in-env
  (lambda (var env)
    (if (no-more-frames? env)
        no-binding
        (let ((b (binding-in-frame var (first-frame env))))
          (if (found-binding? b)
              b
              (binding-in-env var (rest-frames env)))))))

;;; Extension d'un environnement avec un ensemble de variables et de valeurs

(define extend-environment
  (lambda (variables values base-env cont)
    (make-frame variables
		values
		(Clambda (new-frame)
		  adjoin : (base-env cont)
		  (return (adjoin-frame new-frame base-env)
			  cont)))))

;;; Alteration de la liaison d'une variable dans l'environnement courant.  
;;; Si la variable n'est pas definie, il y a une erreur.

(define set-variable-value!
  (lambda (var val env cont)
    (let ((b (binding-in-env var env)))
      (if (found-binding? b)
          (begin
	    (set-binding-value! b val)
	    (instrument 'set! b val)
	    (return unspecified-value cont))
          (wrong 'set-variable-value! "Unbound variable" var)))))

;;; Creation d'une liaison dans l'environnement courant.  Si la variable est
;;; deja liee dans cet environnement, la liaison est redefinie

(define define-variable!
  (lambda (var val env)
    (let ((b (binding-in-frame var (first-frame env))))
      (if (found-binding? b)
          (begin
	    (set-binding-value! b val)
	    (instrument 'set! b val))  ;;; this is an assignment, really!
          (set-first-frame!
           env
           (adjoin-binding (make-binding var val)
                           (first-frame env)))))))

;;; Accesseurs et predicats pour la structure de donnees representant les 
;;; environnements.

(define first-frame
  (lambda (env) (car env)))

(define rest-frames
  (lambda (env) (cdr env)))

(define no-more-frames?
  (lambda (env) (null? env)))

(define adjoin-frame
  (lambda (frame env) (cons frame env)))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! env new-frame)))

;;; Fonctions relatives aux frames

(define make-frame
  (lambda (variables values cont)
    (cond ((and (null? variables) (null? values)) (return '() cont))
          ((null? variables)
           (wrong 'make-frame "Too many values supplied" values))
          ((symbol? variables)
           (return (cons (make-binding variables values) '())
		   cont))
          ((null? values)
           (wrong 'make-frame "Too few values supplied" variables))
          (else
	   (make-frame (cdr variables)
		       (cdr values)
		       (Clambda (frame)
			 frame : (variables values cont)
			 (return (cons (make-binding (car variables) (car values))
				       frame)
				 cont)))))))

(define adjoin-binding
  (lambda (binding frame)
    (cons binding frame)))

;(define assq
;  (lambda (key bindings)
;    (cond ((null? bindings) no-binding)
;         ((eq? key (binding-variable (car bindings))) 
;          (car bindings))
;         (else (assq key (cdr bindings))))))

(define binding-in-frame
  (lambda (var frame)
    (assq var frame)))

(define found-binding?
  (lambda (b)
    (not (eq? b no-binding))))

(define no-binding #f)

;;; Representation des liaisons

(define make-binding
  (lambda (variable value)
    (cons variable value)))

(define binding-variable
  (lambda (binding)
    (car binding)))

(define binding-value
  (lambda (binding)
    (cdr binding)))

(define set-binding-value!
  (lambda (binding value)
    (set-cdr! binding value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (exp) (not (pair? exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define make-primitive
  (lambda (name index)
    (list primitive-tag name index)))

(define make-k-primitive
  (lambda (name index)
    (list k-primitive-tag name index)))

(define primitive-tag (list 'primitive))
(define k-primitive-tag (list 'k-primitive))


(define apply-primitive-procedure
  (lambda (procedure arguments cont)
    (let ((index (caddr procedure)))
      ((vector-ref *primitive-vector* index) cont arguments))))

(define apply-k-primitive-procedure
  (lambda (procedure arguments cont)
    (let ((index (caddr procedure)))
      ((vector-ref *primitive-vector* index) arguments cont))))
                                         





    

(define wrong
  (lambda args
    (newline)
    (display "Error in ")
    (for-each (lambda (x)
                (display x)
                (display " "))
              args)
    (newline)
    (return unspecified-value error-continuation)))







(define *the-global-environment* (cons '() '()))
(define *max-primitives* 512)

(define *primitive-vector* (make-vector *max-primitives*))





(define primitive-procedure? (test-first-element primitive-tag))
(define k-primitive-procedure? (test-first-element k-primitive-tag))






(define evaluate
  (lambda (expr)
    (eval-cps (embed-expand expr)
              *the-global-environment*
              (Clambda (x)
		init : ()
		x))))

;; (define user-print
;;   (lambda (object)
;;     (cond ((compound-procedure? object)
;; 	   (let ((lambda-exp (closure-lambda object)))
;; 	     (write (list 'compound-procedure
;; 			  (lambda-parameters object)
;; 			  (lambda-body object)
;; 			  "[procedure-env]"))))
;;           (else (write object)))))

(define user-print write)

(define primitive-id
  (lambda (exp)
    (cadr exp)))


(define apply-escape-procedure
  (lambda (procedure arguments)
    (cond ((null? arguments)
           (wrong 'apply-escape-procedure "zero argument" arguments))
          ((null? (cdr arguments))
           (return (car arguments) (escape-procedure->cont procedure)))
          (else (wrong 'apply-escape-procedure "wrong number of arguments" arguments)))))
           
;;; default way of resuming a continuation is to apply it to a value.
(define resume
  (lambda (c v)
    (c v)))

;;; If we don't do interleaving, returning a value to a continuation
;;; is just resuming it.
;;; Note: do not (define return resume) because for ACPS we redefine resume.
(define return
  (lambda (v c)
    (set! *count* (+ *count* 1))
    (if (= *count* *bound*)
	(wrong 'return
	       "Too many steps"
	       0)
	(resume c v))))

(define *bound* 10000)
(define *count* 0)

(define error-continuation uninitialised)

(define match-arity?
  (lambda (arity len)
    (cond ((and (number? arity)
                (>= arity 0))
           (= arity len))
          ((and (number? arity)
                (< arity 0))
           (>= len (- (+ 1 arity)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; r5rs business




(define return-values
  (lambda (cont vals)
    (apply cont vals)))

(define call/values
  (lambda (cont producer consumer)
    (apply-cps producer
	       '()
	       (Clambda x
		 cwv : (consumer cont)
		 (apply-cps consumer x cont)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define for-each-primitive
  (lambda (f args cont)
    (if (null? (car args))
        (return unspecified-value cont)
        (apply-cps f
		   (map car args)
		   (Clambda (discard)
		     for-each-first : (f args cont)
		     (for-each-primitive f
					 (map cdr args)
					 (Clambda (args)
					   for-each-rest : (cont)
					   (return unspecified-value cont))))))))

(define map-primitive
  (lambda (f args cont)
    (if (null? (car args))
        (return '() cont)
        (apply-cps f
		   (map car args)
		   (Clambda (val)
		     map-first : (f args cont)
		     (map-primitive f
				    (map cdr args)
				    (Clambda (args)
				      map-rest : (val cont)
				      (return (let ((val (cons val args)))
						(instrument 'cons 'map 1)
						val)
					      cont))))))))

(define append-map-primitive
  (lambda (f args cont)
    (if (null? (car args))
        (return '() cont)
        (apply-cps f
		   (map car args)
		   (Clambda (val)
		     append-map-first : (f args cont)
		     (append-map-primitive f
				    (map cdr args)
				    (Clambda (args)
				      append-map-rest : (val cont)
				      (return (append val args) cont))))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abstract continuation representation:

(define acont-tag 'acont)
(define make-acont
  (lambda (name . vals)
    (cons acont-tag (cons name vals))))
(define acont?
  (test-first-element acont-tag))

(define test-acont?
  (lambda (name)
    (lambda (x)
      (and (acont? x)
	   (eq? (cadr x) name)))))

(define resume-core
  (lambda (cont next)
    (cond ((acont-rator? cont) resume-rator)
	  ((acont-rand? cont)  resume-rand)
	  ((acont-lof-first? cont) resume-lof-first)
	  ((acont-lof-rest? cont) resume-lof-rest)
	  ((acont-cond? cont) resume-cond)
	  ((acont-seq? cont) resume-seq)
	  ((acont-set? cont) resume-set)
	  ((acont-define? cont) resume-define)
	  ((acont-init? cont) resume-init)
	  ((acont-cwv?  cont) resume-cwv)
          ((acont-map-first? cont)      resume-map-first)
          ((acont-map-rest? cont)       resume-map-rest)
          ((acont-append-map-first? cont)      resume-append-map-first)
          ((acont-append-map-rest? cont)       resume-append-map-rest)
          ((acont-for-each-first? cont) resume-for-each-first)
          ((acont-for-each-rest? cont)  resume-for-each-rest)
	  ((acont-frame?  cont) resume-frame)
	  ((acont-adjoin?  cont) resume-adjoin)
	  ((acont-extend-env? cont) resume-extend-env)
	  ((acont-load-file? cont) resume-load-file)
	  (else (next cont)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define cxr?
  (lambda l
    (lambda (v)
      (letrec ((check (lambda (l v)
			(or (null? l)
			    (and (pair? v)
				 (check (cdr l) ((car l) v)))))))
	(check l v)))))
(define and-map
  (lambda (pred l)
    (or (null? l)
	(and (pred (car l))
	     (and-map pred (cdr l))))))

(define butlast
  (lambda (l)
    (if (or (null? l)
	    (null? (cdr l)))
	'()
	(cons (car l)
	      (butlast (cdr l))))))

                                               
(define procedure-primitive?
  (lambda (x)
    (or (primitive-procedure? x)
	(k-primitive-procedure? x)
	(escape-procedure? x)
	(compound-procedure? x))))

(define alist?
  (lambda (x)
    (and (list? x)
	 (and-map pair? x))))


(define cycle?
  (lambda (S-exp path)

    (define successors
      (lambda (S-exp)
	(cond ((pair? S-exp) (list (car S-exp) (cdr S-exp)))
	      ((vector? S-exp) (vector->list S-exp))
	      (else (error 'successors
			   "Unknown composite type"
			   S-exp)))))
	       
    
    (if (and (atom? S-exp)
	     (not (vector? S-exp)))
	#f
	(let ((new-path (cons S-exp path)))
	  (or (memq S-exp path)
	      (let loop ((succ (successors S-exp)))
		(and (not (null? succ))
		     (let ((yes? (cycle? (car succ)
					 new-path)))
		       (or yes?
			   (loop (cdr succ)))))))))))

(define load-primitive
  (lambda (cont filename)
    (read-file-primitive (Clambda (l)
			   load-file : (cont)
			   (eval-cps (embed-expand (cons 'begin l))
				     *the-global-environment*
				     cont)
			   unspecified-value)
			 filename)))

(define read-file-primitive
  (lambda (cont filename)
    (let ((handler1 (lambda (escape proc message object)
		      (wrong 'read-file-primitive
			     "file does not exist: "
			     filename)))
	  (handler2 (lambda (escape proc message object)
		      (wrong 'read-file-primitive
			     "failed on"
			     filename))))
      (let ((port (try (open-input-file filename)
		       handler1)))
	(if port
	    (return (try (let loop ((l '()))
			   (let ((val (read port)))
			     (if (eof-object? val)
				 (reverse l)
				 (loop (cons val l)))))
			 handler2)
		    cont)
	    (wrong 'read-file-primitive
		   "file does not exist"
		   filename))))))

(define stats-set! 0)
(define stats-set-cxr! 0)
(define stats-cons 0)

(define *instrument-initialized* #f)

(define instrument
  (lambda (operation subject args)
    (cond ((eq? operation 'reset)
	   (if (not *instrument-initialized*) ;;; allow reset only ones. 
	       (begin
		 (set! *instrument-initialized* #t)  
		 (cond ((eq? subject 'set!)
			(set! stats-set! 0))
		       ((eq? subject 'set-cxr!)
			(set! stats-set-cxr! 0))
		       ((eq? subject 'cons)
			(set! stats-cons 0))
		       ((eq? subject '*)	
			(set! stats-cons 0)
			(set! stats-set! 0)
			(set! stats-set-cxr! 0)))))
	   unspecified-value)
	  ((eq? operation 'cons)
	   (set! stats-cons (+ args stats-cons))
	   unspecified-value)
	  ((eq? operation 'set!)
	   (set! stats-set! (+ 1 stats-set!))
	   unspecified-value)
	  ((eq? operation 'set-cxr!)
	   (set! stats-set-cxr! (+ 1 stats-set-cxr!))
	   unspecified-value)
	  ((eq? operation 'get)
	   (cond ((eq? subject 'set!) stats-set!)
		 ((eq? subject 'set-cxr!) stats-set-cxr!)
		 ((eq? subject 'cons) stats-cons)
		 ((eq? subject '*)  (list 'cons stats-cons 'set! stats-set! 'set-cxr! stats-set-cxr!)))))))
		 

(process-global-definitions)  
; end of core-cps.scm 

