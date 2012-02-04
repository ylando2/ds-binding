#lang racket

(provide ds-let ds-set! ds-define ds-let2 ds-set2! ds-define2)
  
;;This is a version of arc destruction-binding in racket.
;;I want to build 3 macros to use in destructuring assigment of a list:
;;ds-let,ds-set! and ds-define
;;I have two ways to do that so I build them in two diffrent ways.
;;Now we can test them to see which one is more effecient.
;;The first way is a more functional one and the second procedural.
;;I hope that racket optimize the first way.

;;ds-define-false is an helper macro that destruct his arguments and defined them to false.
(define-syntax ds-define-false
  (syntax-rules (o)
    ((_ ()) (void))
    ((_ ((o arg val) . rest))
     (begin
       (define arg #f)
       (ds-define-false rest)))
    ((_ (arg . rest))
     (begin
       (ds-define-false arg)
       (ds-define-false rest)))
    ((_ arg)
     (define arg #f))))

(define-syntax ds-let
  (lambda (x)
    (syntax-case x ()
      ((_ arg lst body ...)
       (if (identifier? #'lst)
           #'(helper-ds-let arg lst body ...)
           #'(let ((lst2 lst))
               (helper-ds-let arg lst2 body ...)))))))

(define-syntax helper-ds-let
  (lambda (x)
    (syntax-case x (o)
    ((_ () lst body ...) #'(begin body ...)) ;empty argument list
    ((_ ((o arg val)) lst body ...)
     #'(let ((arg (if (null? lst) val (car lst)))) body ...))
    ((_ (arg) lst body ...) ;one element argument list
      (if (identifier? #'arg) 
          #'(let ((arg (car lst))) body ...)
          #'(let ((head (car lst)))
              (helper-ds-let arg head body ...))))
    ((_ ((o arg val) . rest) lst body ...)
     (if (identifier? #'rest)
         #'(let ((arg (if (null? lst) val (car lst))) (rest (if (null? lst) '() (cdr lst)))) body ...) 
         #'(let ((arg (if (null? lst) val (car lst))) (lst2 (if (null? lst) '() (cdr lst))))
             (helper-ds-let rest lst2 body ...))))   
    ((_ (arg . rest) lst body ...)
     (if (identifier? #'arg)
         (if (identifier? #'rest)
             #'(let ((arg (car lst)) (rest (cdr lst)))
                 body ...) ;arg and rest are identifier
             #'(let ((arg (car lst)) (lst2 (cdr lst)))
                 (helper-ds-let rest lst2 body ...))) ;arg is identifier but rest is not identifier
         (if (identifier? #'rest)
             #'(let ((rest (cdr lst)) (lst2 (car lst)))
                 (helper-ds-let arg lst2 body ...)) ;arg is not identifier but rest is identifier
             #'(let ((head (car lst)) (lst2 (cdr lst)))
                 (helper-ds-let arg head
                    (helper-ds-let rest lst2 body ...))))))))) ;arg and rest are not identifiers

(define-syntax ds-set!
  (syntax-rules ()
    ((_ arg lst)
     (let ((lst2 lst))
       (helper-ds-set! arg lst2)))))  

(define-syntax helper-ds-set!
  (lambda (x)
    (syntax-case x (o)
    ((_ () lst) #'(void)) ;empty argument list
    ((_ ((o arg val)) lst)
     #'(set! arg (if (null? lst) val (car lst))))
    ((_ (arg) lst body ...) ;one element argument list
      (if (identifier? #'arg) 
          #'(set! arg (car lst))
          #'(let ((head (car lst)))
              (helper-ds-set! arg head))))
    ((_ ((o arg val) . rest) lst)
     (if (identifier? #'rest)
         #'(if (null? lst)
               (begin
                 (set! arg val)
                 (set! rest '()))
               (begin
                 (set! arg (car lst))
                 (set! rest (cdr lst))))
         #'(let ((lst2 (if (null? lst) '() (cdr lst))))
             (set! arg (if (null? lst) val (car lst))) 
             (helper-ds-set! rest lst2))))   
    ((_ (arg . rest) lst)
     (if (identifier? #'arg)
         (if (identifier? #'rest)
             #'(begin
                 (set! arg (car lst)) 
                 (set! rest (cdr lst))) ;arg and rest are identifier
             #'(let ((lst2 (cdr lst)))
                 (set! arg (car lst)) 
                 (helper-ds-set! rest lst2))) ;arg is identifier but rest is not identifier
         (if (identifier? #'rest)
             #'(let ((head (car lst)))
                 (set! rest (cdr lst)) 
                 (helper-ds-set! arg head)) ;arg is not identifier but rest is identifier
             #'(let ((head (car lst)) (lst2 (cdr lst)))
                 (helper-ds-set! arg head)
                 (helper-ds-set! rest lst2)))))))) ;arg and rest are not identifiers
 
(define-syntax ds-define
  (syntax-rules ()
    ((_ arg lst) 
     (begin 
       (ds-define-false arg)
       (ds-set! arg lst)))))

;;;;;;; a second version that use less variables ;;;;;;;;;;;;;;;

(define-syntax ds-let2 
  (syntax-rules ()
    ((_ arg lst body ...)
     (let ((lst2 lst))
       (helper-ds-let2 arg lst2 body ...)))))

(define-syntax helper-ds-let2
  (lambda (x)
    (syntax-case x (o)
    ((_ () lst body ...) #'(begin body ...)) ;empty argument list
    ((_ ((o arg val)) lst body ...)
     #'(let ((arg (if (null? lst) val (car lst)))) body ...))
    ((_ (arg) lst body ...) ;one element argument list
      (if (identifier? #'arg) 
          #'(let ((arg (car lst))) body ...)
          #'(begin 
              (set! lst (car lst))
              (helper-ds-let2 arg lst body ...))))
    ((_ ((o arg val) . rest) lst body ...)
     (if (identifier? #'rest)
         #'(let ((arg (if (null? lst) val (car lst))) (rest (if (null? lst) '() (cdr lst)))) body ...) 
         #'(let ((arg (if (null? lst) val (car lst)))) 
             (when (not (null? lst)) (set! lst (cdr lst)))
             (helper-ds-let2 rest lst body ...))))   
    ((_ (arg . rest) lst body ...)
     (if (identifier? #'arg)
         (if (identifier? #'rest)
             #'(let ((arg (car lst)) (rest (cdr lst)))
                 body ...) ;arg and rest are identifier
             #'(let ((arg (car lst)))
                 (set! lst (cdr lst))
                 (helper-ds-let2 rest lst body ...))) ;arg is identifier but rest is not identifier
         (if (identifier? #'rest)
             #'(let ((rest (cdr lst)))
                 (set! lst (car lst))
                 (helper-ds-let2 arg lst body ...)) ;arg is not identifier but rest is identifier
             #'(let ((head (car lst)))
                 (set! lst (cdr lst))
                 (helper-ds-let2 arg head
                    (helper-ds-let2 rest lst body ...))))))))) ;arg and rest are not identifiers
 

(define-syntax ds-set2! 
  (syntax-rules ()
    ((_ arg lst)
     (let ((lst2 lst))
       (helper-ds-set2! arg lst2)))))

(define-syntax helper-ds-set2!
  (lambda (x)
    (syntax-case x (o)
    ((_ () lst) #'(void)) ;empty argument list
    ((_ ((o arg val)) lst)
     #'(set! arg (if (null? lst) val (car lst))))
    ((_ (arg) lst) ;one element argument list
      (if (identifier? #'arg) 
          #'(set! arg (car lst))
          #'(begin 
              (set! lst (car lst))
              (helper-ds-set2! arg lst))))
    ((_ ((o arg val) . rest) lst)
     (if (identifier? #'rest)
         #'(if (null? lst)
               (begin
                 (set! arg val)
                 (set! rest '()))
               (begin
                 (set! arg (car lst))
                 (set! rest (cdr lst))))
         #'(begin (if (null? lst) 
                      (set! arg val)
                      (begin
                        (set! arg (car lst))
                        (set! lst (cdr lst))))
                  (helper-ds-set2! rest lst))))
    ((_ (arg . rest) lst)
     (if (identifier? #'arg)
         (if (identifier? #'rest)
             #'(begin (set! arg (car lst)) (set! rest (cdr lst))) ;arg and rest are identifier
             #'(begin 
                 (set! arg (car lst))
                 (set! lst (cdr lst))
                 (helper-ds-set2! rest lst))) ;arg is identifier but rest is not identifier
         (if (identifier? #'rest)
             #'(begin 
                 (set! rest (cdr lst))
                 (set! lst (car lst))
                 (helper-ds-set2! arg lst)) ;arg is not identifier but rest is identifier
             #'(let ((head (car lst)))
                 (set! lst (cdr lst))
                 (helper-ds-set2! arg head)
                 (helper-ds-set2! rest lst)))))))) ;arg and rest are not identifiers

(define-syntax ds-define2
  (syntax-rules ()
    ((_ arg lst)
     (begin
       (ds-define-false arg)
       (ds-set2! arg lst)))))