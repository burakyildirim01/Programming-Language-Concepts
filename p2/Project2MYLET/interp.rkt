#lang eopl

;; interpreter for the LET language. 


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

      ;; implement const-exp here
      (const-exp (num) (num-val num))

      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

     

      ;; implement comp-exp here
      (comp-exp (exp1 how-str exp2)
        (let 
         (
           (val1 (value-of exp1 env))
           (val2 (value-of exp2 env))
         )
         (let 
            (
              (num1 (expval->num val1))
              (num2 (expval->num val2))
              (how (expval->string (value-of how-str env)))   
            )
            (cond
              ((equal? how "'greater'") (bool-val (> num1 num2)))
              ((equal? how "'less'") (bool-val (< num1 num2)))
              ((equal? how "'equal'") (bool-val (= num1 num2)))
              (else (eopl:error 'comp-exp "No good condition for ~s" how))

            )
        )
        )
      
      )

      
      ;; implement op-exp here
     (op-exp (exp1 op-str exp2)
        (let 
         (
          (num1 (expval->num (value-of exp1 env)))
          (num2 (expval->num (value-of exp2 env)))        
          (op (expval->string (value-of op-str env)))   
         )
          (cond
            ((equal? op "'add'") (num-val (+ num1 num2)))
            ((equal? op "'mult'") (num-val (* num1 num2)))
            ((equal? op "'div'") (num-val (/ num1 num2)))
            ((equal? op "'sub'") (num-val (- num1 num2)))
            (else (eopl:error 'op-exp "No good condition for ~s" op))
          )
        )
      )



      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
        (let 
          (
            (val1 (value-of cond1 env))
          )      
            (if (expval->bool val1)
              (value-of exp1 env)
              (value-of else-exp env)
            )
          )
      )

      ;; implement my-cond-exp here
      (my-cond-exp ( cond1 exp1 conds exps else1) 
        (let
          (
            (val? (expval->bool (value-of cond1 env)))
          )
          (if val?
            (value-of (my-cond-helper conds exps exp1 env) env)
            (value-of (my-cond-helper conds exps else1 env) env)
          )
        )
      )

      


      
      ;; implement str-exp here
      (str-exp (str) (str-val str))


      ;; implement bool-exp here
      (bool-exp (bool-str) 
   
          (cond 
            ((equal? bool-str "#true") (bool-val #t))
            ((equal? bool-str "#false") (bool-val #f))
            (else (eopl:error 'bool-exp "No good condition for ~s" bool-str))
          )
          


        
      )
      
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


      ;; implement let-exp here
      (let-exp ( id exp1 exp2)
        (value-of exp2 (extend-env id (value-of exp1 env) env))
      )


      (switch-exp ( search-exp values expressions else-exp)
        (let
          (
            (search-val (value-of search-exp env))
          )
          (switch-exp-helper search-val values expressions else-exp env)

          
        )
      
      )
      
      


    )
    
))

(define my-cond-helper 
  (lambda ( conds exps return env)
    (if (null? conds)
      return
      (let
      (
        (val? (expval->bool (value-of (car conds) env)))
        (exp1 (car exps) )
        (remain-conds (cdr conds))
        (remain-exps (cdr exps))
      )
      (if val?
        (my-cond-helper remain-conds remain-exps exp1 env)
        (my-cond-helper remain-conds remain-exps return env)
      
      )
    )
    )
  )
)


(define switch-exp-helper 
  (lambda (search-val values expressions else-exp env)

    (if (null? expressions)
      (value-of else-exp env)
      (let
        (
          (val (value-of (car values) env))
          (exp (car expressions) )
        )
        (if (equal? val search-val)
          (value-of exp env)
          (switch-exp-helper search-val (cdr values) (cdr expressions) else-exp env)
        )
        

    )
          
    
    )
  )

)



;(trace value-of)