(define (sign expr)
  (cond
    ((equal? (car expr) '-) '-)
    ((equal? (car expr) '+) '+)
    (else '())))

(define (helper1 expr)
  (cond
    ((number? expr) 0)
    ((equal? expr 'x)1)
    ((equal? expr '-x)-1)))

    

(define (derivative expr)
  (if (not(list? expr))
          (derivative (list expr))
      (cond
        ;((equal? expr '(* x 2 x)) (derivative '(* 2 (expt x 2))))
        ((< (length expr) 2) (helper1 (car expr)))
        ((equal? (car expr) '*)
         (if (< (length expr) 4)
             (if (number? (cadr expr))
                 (list '* (cadr expr) (derivative (caddr expr)))
                 (list '* (derivative (cadr expr)) (caDdr expr)))
             (list '* (cadr expr) (derivative (cons '* (cddr expr))))))
        ((equal? (car expr) '*) (list '+ (list '* (derivative (cadr expr)) (caddr expr))
                                      (list '* (cadr expr) (derivative (caddr expr)))))
        ((and (equal? (car expr) 'expt) (equal?  (cadr expr) 'x))
         (list '* (caddr expr) (list (car expr) (cadr expr) (- (caddr expr) 1))))
        ((and (equal? (car expr) 'expt) (number? (cadr expr))) (list '* expr (list 'log (cadr expr))))
        ((equal? (car expr) 'cos) (list '* (list '- (list 'sin (cadr expr))) (derivative (cadr expr))))
        ((equal? (car expr) 'sin) (list '* (list 'cos (cadr expr)) (derivative (cadr expr))))
        ((equal? (car expr) 'exp) (list '* (list 'exp (cadr expr)) (derivative (cadr expr))))
        ((equal? (car expr) 'log) (list '/(derivative (cadr expr)) (cadr expr)))
        ((equal? (car expr) '/) (list '/ (list '- (list '* (derivative (cadr expr)) (caddr expr))
                                               (list '* (cadr expr) (derivative(caddr expr)))) (list 'expt (caddr expr) 2)))
        ((not(null? (sign expr))) (if (null? (cddr expr))
                                      (list (sign expr) (derivative (cadr expr)))
                                      (list (sign expr) (derivative (cadr expr))(derivative (caddr expr))))))))

    
  
    



