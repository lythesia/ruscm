; (define-syntax case
;   (syntax-rules (else)
;     ((case (key ...)
;        clauses ...)
;      (let ((atom-key (key ...)))
;        (case atom-key clauses ...)))
;     ((case key
;        (else result1 result2 ...))
;      (begin result1 result2 ...))
;     ((case key
;        ((atoms ...) result1 result2 ...))
;      (if (memv key ’(atoms ...))
;          (begin result1 result2 ...)))
;     ((case key
;        ((atoms ...) result1 result2 ...)
;        clause clauses ...)
;      (if (memv key ’(atoms ...))
;          (begin result1 result2 ...)
;          (case key clause clauses ...)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body2 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((_ ((name val) ...) body1 body2 ...)
     (let ((name #f) ...)
       (set! name val) ...
       body1
       body2 ...))))

; letrec
; (letrec
;   ((even? (lambda (n) (if (= n 0) #t (odd?  (- n 1)))))
;    (odd?  (lambda (m) (if (= m 0) #f (even? (- m 1))))))
;   (even? 5))
; named-let
; (let loop ((n 10) (acc '())) (if (= 0 n) acc (loop (- n 1) (cons n acc))))
