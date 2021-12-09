(define arifm '(#\+ #\- #\* #\/ #\^))
(define fast-break 1)
(define call/cc call-with-current-continuation)

(define (comp_num lst)
  (let loop ((lst lst) (res '()) (c_points 0))
    (if (or (null? lst)(and (not(char-numeric? (car lst))) (not (member (car lst) '(#\. #\e)))))
        (string->number(list->string res))
        (if (equal? (car lst) #\.)
            (if (= c_points 0)
                (loop (cdr lst) (append res (list (car lst))) (+ c_points 1))
                (fast-break #f))
            (loop (cdr lst) (append res (list (car lst))) c_points)))))

(define (comp_num-skiper lst)
  (if (or (null? lst)(and (not(char-numeric? (car lst))) (not (member (car lst) '(#\. #\e)))))
      lst
      (comp_num-skiper (cdr lst))))

(define (comp_var lst res)
  (if (or (null? lst)(not(char-alphabetic? (car lst))))
      (string->symbol(list->string res))
      (comp_var (cdr lst) (append res (list (car lst))))))

(define (comp_var-skiper lst)
  (if (or (null? lst)(not(char-alphabetic? (car lst))))
      lst
      (comp_var-skiper (cdr lst))))
      


(define (tokenize str)
  (define (helper lister)
    (let loop ((lstr lister)(tokenized '()))
      (if (null? lstr)
          tokenized
          (let ((word (car lstr)))
            (cond
              ((char-whitespace? word) (loop (cdr lstr) tokenized))
              ((member word '(#\( #\))) (loop (cdr lstr)
                                              (append tokenized (list(string(car (member word '(#\( #\)))))))))
              ((member word arifm)
               (loop (cdr lstr) (append tokenized (list(string->symbol(string(car (member word arifm))))))))
              ((char-alphabetic? word)
               (loop (comp_var-skiper lstr) (append tokenized (list (comp_var lstr '())))))
              ((char-numeric? word) (loop (comp_num-skiper lstr)(append tokenized (list (comp_num lstr)))))
              (else (fast-break #f)))))))
  (call/cc
   (lambda (stack)
     (set! fast-break stack)
     (helper (string->list str)))))



(define (parse tokens)
  (define ind 0)
  (define (isIndex?) (< ind (length tokens)))
  (define (curToken) (list-ref tokens ind))
  (define (ind++) (set! ind (+ ind 1)))
  
  (define (correctBrackets?)
    (let loop ((i ind) (deapth 1))
      (and (isIndex?)
           (let ((word (list-ref tokens i)))
             (cond
               ((and (equal? word ")") (= deapth 1)))
               ((equal? word "(") (loop (+ i 1) (+ deapth 1)))
               ((equal? word ")") (loop (+ i 1) (- deapth 1)))
               (else (loop (+ i 1) deapth)))))))

  (define (expr start-index)
    (let loop ((T (term)))
      (if (and (isIndex?)(or (equal? (curToken) '+) (equal? (curToken) '-)))
          (let ((addOp (curToken)))
            (ind++)
            (if (not (isIndex?))
                (fast-break #f)
                (loop (list T addOp (term)))))
          (if (isIndex?)
              (if (and (equal? (curToken) ")") start-index)
                  (begin
                    (ind++)
                    T)
                  (fast-break #f))
              T))))

  (define (term)
    (let loop ((F (factor)))
      (if (and (isIndex?)(or (equal? (curToken) '/) (equal? (curToken) '*)))
          (let ((mullOp (curToken)))
            (ind++)
            (if (not(isIndex?))
                (fast-break #f)
                (loop (list F mullOp (factor)))))
          F)))

  (define (factor)
    (let loop ((P (power)))
      (if (and (isIndex?)(equal? (curToken) '^))
          (if (not (isIndex?))
              (fast-break #f)
              (begin
                (ind++)
                (list P '^ (factor))))
          P)))

  (define (power)
    (if (not (isIndex?)) (fast-break #f)
        (let ((curToken (curToken)))
          (ind++)
          (cond
            ((equal? curToken '-)(list '- (power)))
            ((equal? curToken "(")
             (if (correctBrackets?)
                 (expr  ind)
                 (fast-break #f)))
            ((number? curToken) curToken)
            ((symbol? curToken) curToken)
            (else (fast-break #f))))))

  (call/cc
   (lambda (var)
     (set! fast-break var)
     (if tokens
         (expr #f)
         (fast-break #f)))))

(define arifm-symbols '('* '+ '- '/))

(define (tree->scheme parserRes)
  (and parserRes
       (let loop ((expr parserRes) (res '()))
         (if (null? expr)
             res
             (if (list? (car expr))
                 (loop (cdr expr) (append res (list (loop (car expr) '()))))
                 (let ((word (car expr)))
                   (cond
                     ((or (equal? word '+) (equal? word '-)
                          (equal? word '*) (equal? word '/))
                      (loop (cdr expr) (append (list word) res)))
                     ((equal? word '^)(loop (cdr expr) (append (list 'expt) res)))
                     (else (loop (cdr expr) (append res (list word)))))))))))
            

  
      

      
    
