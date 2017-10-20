(load "assoc-all.scm")
(load "remove-alist-all.scm")

(define group-by-key
  (lambda (l)
    (if (null? l) '()
        (cons (cons (car (car l)) (assoc-all (car (car l)) l)) (group-by-key (remove-alist-all (car (car l)) l))))))
       

