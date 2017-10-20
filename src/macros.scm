; /home/lavm/lecture-notes/cm315/scheme/eval/macro.scm created by Lavm on kronos on Fri Aug  8 15:54:49 1997 
; $Id: macros.scm,v 1.3 1998/09/27 10:00:27 lavm Exp $ 

(define-macro (apply-macro arity proc args)
  (cond
   ((= arity 0) `(,proc))
   ((= arity 1) `(,proc (car args)))
   ((= arity 2) `(,proc (car args) (cadr args)))
   ((= arity 3) `(,proc (car args) (cadr args) (caddr args)))
   ((= arity 4) `(,proc (car args) (cadr args) (caddr args) (cadddr args)))
   ((= arity 5) `(,proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))
   ;((<= arity 0) `(apply ,proc args))
   ((= arity -1) `(,proc args))
   ((= arity -2) `(,proc (car args) (cdr args)))
   ((= arity -3) `(,proc (car args) (cadr args) (cddr args)))
   (else (wrong 'apply-macro
                "Unknown arity"
                (list arity proc args)))))

(define-macro (apply-macro/c arity proc cont args)
  (cond
   ((= arity 0) `(,proc cont))
   ((= arity 1) `(,proc cont (car args)))
   ((= arity 2) `(,proc cont (car args) (cadr args)))
   ((= arity 3) `(,proc cont (car args) (cadr args) (caddr args)))
   ((= arity 4) `(,proc cont (car args) (cadr args) (caddr args) (cadddr args)))
   ((= arity 5) `(,proc cont (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))
   ;((<= arity 0) `(apply ,proc cont args))
   ((= arity -1) `(,proc cont args))
   ((= arity -2) `(,proc cont (car args) (cdr args)))
   ((= arity -3) `(,proc cont (car args) (cadr args) (cddr args)))
   (else (wrong 'apply-macro
                "Unknown arity"
                (list arity proc args)))))

;(define-macro (definitial var val)
;  `(define-variable! ',var ,val the-global-environment))


;(define-macro (definitial-primitive name arity . rest)
;  (if (null? rest)
;      `(definitial ,name (make-primitive ',name ,arity (apply-macro ,arity ,name)))
;      `(definitial ,name (make-primitive ',name ,arity (apply-macro ,arity ,(car rest))))))
      

;(define-macro (definitial-k-primitive name arity . rest)
;  (if (symbol? arity)
;      `(definitial ,name (lookup-variable-value ',arity the-global-environment))
;      (let ((real-arity (if (>= arity 0)
;                           (+ arity 1)
;                           (- arity 1))))
;       (if (null? rest)
;           `(definitial ,name (make-k-primitive ',name ,real-arity (apply-macro ,real-arity ,name)))
;           `(definitial ,name (make-k-primitive ',name ,real-arity (apply-macro ,real-arity ,(car rest))))))))

(define-macro (compile-time . body)
  (begin
    (eval (cons 'begin body))
    0))

(compile-time
(define *primitive-nbr* 0)
)

(define-macro (define-primitive name arity predicate . value)

  (if (null? value)
      (set! value name)
      (set! value (car value)))

  (let ((num (let ((num *primitive-nbr*))
               (set! *primitive-nbr* (+ 1 *primitive-nbr*))
               num)))
    
    `(begin

       (define-variable! ',name (make-primitive ',name ,num) initial-env)
       
       (define-primitive-entry ,num
         (lambda (k args)
           (if (match-arity? ,arity (length args))
	       (if
		 ,(if (eq? predicate '-)
		      #t
		      (list 'apply-macro arity predicate 'args))
		 (return (apply-macro ,arity ,value args) k)
		 (wrong 'apply-primitive
			"Invalid arguments for primitive"
			',name))
               (wrong 'apply-primitive
                      "Incorrect number of arguments for primitive"
                      ',name))))))

  )


(define-macro (define-k-primitive name arity predicate value)

  (let ((num (let ((num *primitive-nbr*))
               (set! *primitive-nbr* (+ 1 *primitive-nbr*))
               num)))
    
    `(begin

       (define-variable! ',name (make-k-primitive ',name ,num) initial-env)
       
       (define-primitive-entry ,num
         (lambda (args cont)
           (if (match-arity? ,arity (length args))
               (apply-macro/c ,arity ,value cont args)
               (wrong 'apply-primitive
                      "Incorrect number of arguments for primitive"
                      ',name))))))

  )

;;; A private k-primitive is a k-primitive for which there
;;; is no binding in the global environment.
;;; It can only be used by the implementation.

(define-macro (define-private-k-primitive name arity value)

  (let ((num (let ((num *primitive-nbr*))
               (set! *primitive-nbr* (+ 1 *primitive-nbr*))
               num)))
    
    `(begin

       (define-primitive-entry ,num
         (lambda (args cont)
           (if (match-arity? ,arity (length args))
               (apply-macro/c ,arity ,value cont args)
               (wrong 'apply-primitive
                      "Incorrect number of arguments for primitive"
                      ',name))))))

  )

(define-macro (define-primitive-entry num value)
  `(vector-set! *primitive-vector* ,num ,value))


(define-macro (define-rpc-procedure name arity value)
  
  `(define-rpc-entry! ',name
     (let ((the-proc ,value))
       (lambda (args cont)
         (if (match-arity? ,arity (length args))
             (apply-macro/c ,arity the-proc cont args)
             (wrong 'apply-rpc-procedure
                    "Incorrect number of arguments for rpc procedure"
                    ',name))))
     rpc-env))


(define-macro (define-rsr-handler name arity value)

  `(define-rsr-entry! ',name
     (lambda (args cont)
       (if (match-arity? ,arity (length args))
           (apply-macro/c ,arity ,value cont args)
           (wrong 'apply-rsr-handler
                  "Incorrect number of arguments for rsr handler"
                  ',name)))
     rsr-env))
; end of macro.scm 

