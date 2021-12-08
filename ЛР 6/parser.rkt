(define fast-break 0)

(define (find-tail xs target)
  (if (equal? (car xs) target)
      (cdr xs)
      (find-tail (cdr xs) target)))

(define (find-head xs target)
  (let loop ((xs xs) (acc '()))
    (if (null? xs)
        #f
        (if (equal? (car xs) target)
            acc
            (loop (cdr xs) (append acc (list (car xs))))))))

(define (last xs)
  (list-ref xs (- (length xs) 1)))

(define (tail-endif program)
  (let loop ((prog program) (depth 0))
    (and (not (null? prog))
        (let ((word (car prog)))
          (cond
            ((and (equal? word 'endif) (= depth 1)) (cdr prog))
            ((equal? word 'endif) (loop (cdr prog) (- depth 1)))
            ((equal? word 'if) (loop (cdr prog) (+ depth 1)))
            (else (loop (cdr prog) depth)))))))

(define (head xs n)
  (if (or (= n -1) (null? xs))
      '()
      (cons (car xs) (head (cdr xs) (- n 1)))))

(define (parse program)
  (call-with-current-continuation
   (lambda (var)
     (set! fast-break var)
     (let ((prog (vector->list program)))
       (if (equal? (car prog) 'define)
           (let ((articles (parse-articles prog)))
             (cons (head articles (- (length articles) 2))
                   (list (parse-body (last articles)))))
           (cons '() (list (parse-body prog))))))))


(define (parse-articles program)
  (let loop ((prog program))
    (if (not (null? prog))
        (let ((word (car prog)) (other (cdr prog)))
          (if (equal? word 'define)
              (if (null? other)
                  (fast-break #f)
                  (if (member (car other) '(if endif))
                      (fast-break #f)
                      (let ((head (find-head (cdr other) 'end)))
                        (if (not head)
                            (fast-break #f)
                            (cons (cons (car other) (list (parse-body head)))
                                  (loop (find-tail (cdr other) 'end)))))))
              (list prog)))
        (list prog))))
            
(define (parse-body program)
  (let loop ((prog program) (parsed '()) (saver '()))
    (if (not (null? prog))
        (let ((word (car prog)))
          (cond
            ((equal? word 'if)
             (let ((tail (tail-endif prog)))
               (if (not tail)
                   (fast-break #f)
                   (loop tail (append parsed (list(list 'if (loop (cdr prog) '() (cons 'if saver))))) saver))))
            ((equal? word 'endif)
             (if (and (not (null? saver)) (equal? (car saver) 'if))
                 parsed
                 (fast-break #f)))
            ((member word '(define end))
             (fast-break #f))
            (else (loop (cdr prog) (append parsed (list word)) saver))))
        parsed)))

(display "--------------1-----------------")
(newline)
(parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial ))
(newline)

(display "--------------2-----------------")
(newline)
(parse #(x dup 0 swap if drop -1 endif))
(newline)

(display "--------------3-----------------")
(newline)
(parse #(define abs 
          dup 0 < 
          if neg endif 
          end 
          9 abs 
          -9 abs))
(newline)

(display "--------------4-----------------")
(newline)
(parse #(define =0? dup 0 = end 
          define <0? dup 0 < end 
          define signum 
          =0? if exit endif 
          <0? if drop -1 exit endif 
          drop 
          1 
          end 
          0 signum 
          -2 signum ))
(newline)

(display "--------------5-----------------")
(newline)
(parse #(   define =0? dup 0 = end
             define =1? dup 1 = end
             define -- 1 - end
             define fib
             =0? if drop 0 exit endif
             =1? if drop 1 exit endif
             -- dup
             -- fib
             swap fib
             +
             end
             define make-fib
             dup 0 < if drop exit endif
             dup fib
             swap --
             make-fib
             end
             10 make-fib   ))
(newline)

(display "--------------6-----------------")
(newline)
(parse #(1 2 if + if dup - endif endif dup))
(newline)