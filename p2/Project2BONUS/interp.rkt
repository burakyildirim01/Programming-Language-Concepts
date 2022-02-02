#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
    
      (var-exp (var) (apply-env env var))
      
    
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      ;; implement zero-exp here
      (zero?-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (if (zero? num1)
                (bool-val #t)
                (bool-val #f)
            )
          )
        )
      )

       ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

  
      
      ;; implement let-exp here
      (let-exp ( id exp1 exp2)
        (value-of exp2 (extend-env id (value-of exp1 env) env))
      )

      ;; implement rope-exp here
      (rope-exp (rope) 
        (rope-val (toList (clear-qmarks rope)))
        
      
      )

      ;; implement concate-exp here
      (concate-exp (exp-rope exp-rope2) 
        (let
          (
            (str1 (fromList (expval->rope (value-of exp-rope env))))
            (str2 (fromList (expval->rope (value-of exp-rope2 env))))
          )
          (let
            (
              (combined (string-append str1 str2))
            )
            (rope-val (toList (add-qmarks combined)))
          )
        )
      )

      ;; implement fetch-exp here
      (fetch-exp (exp-rope exp1) 
        (let
          (
            (str (fromList (expval->rope (value-of exp-rope env))))
            (i (expval->num (value-of exp1 env)))
          )
          (let
            (
              (final-str (substring str i (+ 1 i)))
            )
            (rope-val (toList (add-qmarks final-str)))
          )
        )
      
      )
      
      ;; implement substring-exp here
      (substring-exp(exp-rope exp1 exp2) 
        (let
          (
            (str (fromList (expval->rope (value-of exp-rope env))))
            (from (expval->num (value-of exp1 env)))
            (to (expval->num (value-of exp2 env)))
          )
          (let
            (
              (final-str (substring str from to))
            )
            (rope-val (toList (add-qmarks final-str)))
          )
        )
      
      )
                     
                   
      
      )))


    ;; implement helper functions here for your rope-exp
         ;;Hint: functions might do the followings..
             ;; ..removes first element, last element, returns ith index of a list, function returns a substring of a given list etc.
    (define (clear-qmarks rope) (substring  rope 1 (- (string-length rope) 1)))
    (define (add-qmarks rope) (string-append "'" rope "'"))
    (define (substr rope from to) (substring  rope from to) )
    (define (toList str) (string->list str))
    (define (fromList lst) (list->string lst))
    
      
    
