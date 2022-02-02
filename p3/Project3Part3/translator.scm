(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  ;;; (require "environments.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        
        (const-exp (num) (const-exp num))
        
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        
        (var-exp (var)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of var-exp here
          (var-exp (get-symbol-with-num senv var))


        )
        
        (let-exp (var exp1 body)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of let-exp here
          (let
            (
              (senv-imp  (extend-senv var senv))
            )
           (let-exp (get-symbol-with-num senv-imp var) (translation-of exp1 senv-imp) (translation-of body senv-imp))
          )

        )
        
        (proc-exp (var body)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of proc-exp here   
          (let
            (
              (senv-imp  (extend-senv var senv))
            )
            (proc-exp (get-symbol-with-num senv-imp var)  (translation-of body senv))
          )
        )
        
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  
  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
  
   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
  
  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))
  
  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;;;;define apply-senv-number, a procedure that applies the environment
  ;;;;and finds the occurences of variable var in the environment senv
  (define apply-senv-number
    (lambda (senv var)
      (apply-senv-number-helper senv var 0)
    )
  )
  (define apply-senv-number-helper
    (lambda (senv var occ)
      (cond
        ((null? senv) occ)
        ((eqv? var (car senv)) (apply-senv-number-helper (cdr senv) var (+ occ 1)))
        (else  (apply-senv-number-helper (cdr senv) var occ))
      )
    )
  )

    
  ;; init-senv : () -> Senv
  ;; Page: 96
  

  (define get-symbol-with-num
    (lambda (senv sym)
      (symbol-num-combiner sym (apply-senv-number senv sym))
    )
  )
  

  (define symbol-num-combiner
    (lambda (sym num)
      (string->symbol (string-append (symbol->string sym) (number->string num)))
    ))  

  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'a
            (empty-senv))))))
  (define init-senv-2
    (lambda ()
      (extend-senv 'i
        (extend-senv 'i
          (extend-senv 'i
            (empty-senv))))))


  
)
