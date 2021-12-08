'feature-if-else
'feature-while-loop
'feature-repeat-loop
'feature-for-loop
'feature-break-continue
'feature-nested-if


(define (def-arifm-oper oper)
  (cond
    ((equal? oper '+) '+)
    ((equal? oper '-) '-)
    ((equal? oper '*) '*)
    ((equal? oper '/) '/)))

(define (comp-oper? oper)
  (or
   (equal? oper '<)
   (equal? oper '=)
   (equal? oper '>)))

(define (make-log-oper oper stack)
  (cond
    ((and (equal? oper '<)(< (cadr stack) (car stack))) -1)
    ((and (equal? oper '>)(> (cadr stack) (car stack))) -1)
    ((and (equal? oper '=)(= (cadr stack) (car stack))) -1)
    (else 0)))

(define (replace n stack)
  (set! stack (cons n (cddr stack)))
  stack)

(define ie (interaction-environment))

(define (skeeper-count vect i)
  (if (equal? (vector-ref vect i) 'end)
      i
      (skeeper-count vect (+ i 1))))

(define (skeeper-count-if vect i)
  (if (or (equal? (vector-ref vect i) 'endif) (equal? (vector-ref vect i) 'else))
      i
      (skeeper-count-if vect (+ i 1))))

(define (skeeper-count-wend vect i)
  (if (or (equal? (vector-ref vect i) 'wend) (equal? (vector-ref vect i) 'until) (equal? (vector-ref vect i) 'next))
      i
      (skeeper-count-wend vect (+ i 1))))

(define (nested-ifer vect i)
  (let loop ((ind i)(depth 0))
    (let ((word (vector-ref vect ind)))
      (cond
        ((equal? word 'if) (loop (+ ind 1) (+ 1 depth)))
        ((and(equal? word 'endif)(= depth 1)) ind)
        ((equal? word 'endif) (loop (+ ind 1) (- depth 1)))
        (else (loop (+ ind 1) depth))))))
        
      
       
(define (interpret program stack)
  (define (helper i _stack returns_stack defs)
    (if (= i (vector-length program))
        _stack
        (let ((word (vector-ref program i))) 
          (cond
            ((number? word) (helper (+ i 1) (cons word _stack) returns_stack defs))
            ((equal? word (def-arifm-oper word))(helper (+ i 1)
                                                        (replace (eval(list word (cadr _stack) (car _stack))ie) _stack) returns_stack defs))
            ((equal? word 'neg) (helper (+ i 1) (cons (-(car _stack)) (cdr _stack)) returns_stack defs))
            ((equal? word 'mod) (helper (+ i 1) (replace (remainder (cadr _stack) (car _stack)) _stack)returns_stack defs))
            ((comp-oper? word) (helper (+ i 1) (replace (make-log-oper word _stack) _stack) returns_stack defs))
            ((equal? word 'not) (helper (+ i 1) (cons (if (= (car _stack) 0) -1 0) (cdr _stack)) returns_stack defs))
            ((equal? word 'and) (helper (+ i 1) (cons (if (and (= (car _stack) 0) (= (cadr _stack) 0)) -1 0) (cddr _stack)) returns_stack defs))
            ((equal? word 'or) (helper (+ i 1) (cons (if (or (= (car _stack) 0) (= (cadr _stack) 0)) -1 0) (cddr _stack)) returns_stack defs))
            ((equal? word 'drop) (helper (+ i 1) (cdr _stack) returns_stack defs))
            ((equal? word 'swap) (helper (+ i 1) (append (cons (cadr _stack) (list (car _stack))) (cddr _stack)) returns_stack defs))
            ((equal? word 'dup) (helper (+ i 1) (cons (car _stack) _stack) returns_stack defs))
            ((equal? word 'over) (helper (+ i 1) (cons (cadr _stack) _stack)returns_stack defs))
            ((equal? word 'rot) (helper (+ i 1) (append (list (caddr _stack) (cadr _stack) (car _stack)) (cdddr _stack)) returns_stack defs))
            ((equal? word 'depth) (length _stack))
            ((equal? word 'define) (helper (+ (skeeper-count program i) 1) _stack returns_stack
                                           (cons (list (vector-ref program (+ i 1)) (+ i 2)) defs)))
            ((or(equal? word 'end) (equal? word 'exit)) (helper (car returns_stack) _stack (cdr returns_stack) defs))
            ((equal? word 'if) (helper
                                (if (equal? (car _stack) 0)
                                    (nested-ifer program i)
                                    (+ i 1)) (cdr _stack) returns_stack defs))
            ((equal? word 'endif) (helper (+ i 1) _stack returns_stack defs))
            ((equal? word 'else) (helper (+ (skeeper-count-if program i) 1) _stack returns_stack defs))
            ((equal? word 'while)(if (zero? (car _stack))
                                     (helper (+ (skeeper-count-wend program i) 1) (cdr _stack) returns_stack defs)
                                     (helper (+ i 1) (cdr _stack) (cons i returns_stack) defs)))
            ((equal? word 'wend) (helper (car returns_stack) _stack (cdr returns_stack) defs))
            ((equal? word 'repeat)(helper (+ i 1) _stack (cons (+ i 1) returns_stack) defs))
            ((equal? word 'until) (if (zero? (car _stack))
                                      (helper (car returns_stack) (cdr _stack) (cdr returns_stack) defs)
                                      (helper (+ i 1) (cdr _stack) (cdr returns_stack) defs)))
            ((equal? word 'for) (helper (+ i 1) (cddr _stack)
                                        (append (list (cadr _stack)) (list(car _stack)) (list (+ i 1)) returns_stack) defs))
            ((equal? word 'i) (helper (+ i 1) (cons (car returns_stack) _stack) returns_stack defs))
            ((equal? word 'next) (if (< (car returns_stack) (cadr returns_stack))
                                     (helper (caddr returns_stack) _stack
                                             (cons (+ (car returns_stack) 1) (cdr returns_stack)) defs)
                                     (helper (+ 1 i) _stack (cdddr returns_stack) defs)))
            ((equal? word 'break) (helper (+ (skeeper-count-wend program i) 1) _stack
                                          (if (equal?(vector-ref program (skeeper-count-wend program i)) 'next)
                                             (cdddr returns_stack)
                                             (cdr returns_stack)) defs))
            ((equal? word 'continue) (helper (skeeper-count-wend program i) _stack returns_stack defs))
            (else(helper (cadr (assoc word defs)) _stack (cons (+ i 1) returns_stack) defs))))))
          
          
  (helper 0 stack '() '()))