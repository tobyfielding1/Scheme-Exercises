(define remove-alist-all
  (lambda (x l)
    (cond ((null? l) '()) 
          ((equal? x (car (car l))) (remove-alist-all x (cdr l)))
          (else (cons (car l) (remove-alist-all x (cdr l)))))))

