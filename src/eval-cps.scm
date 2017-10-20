;;; /m2/moreau/scheme/eval-cont.scm, Edit by Moreau running on Montefiore on Mon Aug  6 17:15:45 1990
;;; /m6/moreau/scheme/eval-cont.scm, Edit by Moreau running on Ia1 on Fri Jun  2 11:27:52 1989

'(module eval-cps
        (export driver-loop
                prompt
                resume-terminate
                resume-cps
                resume-rep)
        (include "acont.scm")
        (import
         (eps "eps.scm")
         (core-cps "core-cps.scm")
         (primitives "primitives.scm")))

(include "acont.scm")

(init-global-definitions)

(define cps-init!
  (lambda ()
    (set! *the-global-environment*
          (r4rs-global-environment! *the-global-environment*))
    (make-initial-continuation)
    (make-terminate-continuation)
    (set! error-continuation terminate-continuation)))



;;; Driver loop


(define prompt "CPS-EVAL==> ")


(define initial-continuation  'any)
(define terminate-continuation  'any)

;;; I need to wrap Clambda inside a lambda because Clambda is expanded
;;; in a constructor that will be defined at the end of the file.

(define make-initial-continuation
  (lambda ()
    (set! initial-continuation
          (Clambda (result)
            rep : ()
            (user-print result)
            (driver-loop)))))

(define make-terminate-continuation
  (lambda ()
    (set! terminate-continuation
          (Clambda (result)
            terminate : ()
            (user-print result)
            (driver-loop)))))


(define driver-loop
  (lambda ()
    (set! error-continuation terminate-continuation)
    (newline)
    (display prompt)
    (let ((expr (read)))
      (if (not (eof-object? expr))
          (eval-cps (embed-expand expr)
                    *the-global-environment*
                    initial-continuation)))))

(define resume-cps
  (lambda (cont next)
    (cond ((acont-rep? cont) resume-rep)
          ((acont-terminate? cont) resume-terminate)
          (else (next cont)))))

(process-global-definitions)

(cps-init!)
