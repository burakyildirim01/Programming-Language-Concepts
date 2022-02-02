#lang eopl
(module interp (lib "eopl.ss" "eopl"))
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        (newarray-exp (exp1 exp2)
          (let 
            (
              (len (expval->num (value-of exp1 env)))
            )
              (let 
                (
                  (val (value-of exp2 env))
                )
                  (array-val (createNewArray len val) len)
              )
          )
        )
        
       (update-array-exp (exp1 exp2 exp3)
          (let 
            (
              (arr (expval->array (value-of exp1 env)))
            )
              (let 
                (
                  (i (expval->num (value-of exp2 env)))
                )
                (let 
                  (
                    (val (value-of exp3 env))
                  )
                    (arraySet arr i val)
                )
              )
          )
        )

       (read-array-exp (exp1 exp2)
          (let 
            (
              (arr (expval->array (value-of exp1 env)))
            )
              (let 
                (
                  (i (expval->num (value-of exp2 env)))
                )
                (arrayGet arr i)
              )
          )
        )

        
       (print-array-exp (exp1)
        (let
          (
            (arr (expval->array (value-of exp1 env)))
            (length (expval->arrayLength (value-of exp1 env)))
          )
          (printHelper arr length 0)
        )
       
       
       )

      ;;; STACK

      (newstack-exp ()
        (array-val  (createNewArray 1000 0) 1000)
      
      )
      (stack-push-exp (exp1 exp2)
        (let
          (
            (stk (expval->array (value-of exp1 env)))
            (val (expval->num (value-of exp2 env)))
          )
          (stackPushHelper stk val 0)
        )
      
      )

      (stack-pop-exp (exp1)
        (let
          (
            (stk (expval->array (value-of exp1 env)))
          )         
          
          (num-val (stackPopHelper stk 0 #t))
        )
      )

      (stack-top-exp (exp1)
        (let
          (
            (stk (expval->array (value-of exp1 env)))
          )
          (num-val (stackPopHelper stk 0 #f))
        )
      )

      (stack-size-exp (exp1)
      (let
        (
          (stk (expval->array (value-of exp1 env)))
        )
        (num-val (stackSizeHelper stk 0))
      )
      )

      (empty-stack?-exp (exp1)
        (let
          (
            (stk (expval->array (value-of exp1 env)))
          )
          (let
            (
              (val (arrayGet stk 0))
            )
            (if (= 0 val)
              (bool-val #t)
              (bool-val #f)
            )
          )
        )
      
      )

      (print-stack-exp (stk)
        (printHelper stk (stackSizeHelper stk 0) 0)
      
      )

      (array-comprehension-exp (body var arr-exp)
        (let
          (
            (size (expval->arrayLength (value-of arr-exp env)))
            (arr (expval->array (value-of arr-exp env)))
          )

          (array-val (comprehensionHelper body var arr size 0 env) size)
          ; (printHelper  (comprehensionHelper body var arr size 0 env) size 0 )
        )
      
      )

  )
  )
  )


  (define comprehensionHelper
    (lambda (body var arr size i env)
      (if (>= i size)
        arr      
        (let
          (
            (val (arrayGet arr i))          
          )
            (let
              (
                (new-env (extend-env var val env))
              )
                (let
                (
                  (body-val (value-of body new-env))
                )
                
                  (cases expval body-val
                    (proc-val (proc)
                        (begin
                          (arraySet arr i (apply-procedure proc val))
                          (comprehensionHelper body var arr size (+ 1 i) new-env)
                        )
                    )
                    (num-val (num)
                      (begin
                        (arraySet arr i body-val)
                        (comprehensionHelper body var arr size (+ 1 i) new-env)
                      )  
                    )
                    (else 'wrong)
                  )
                  
                
                    
                )
            )
            )          
        
      )
    )
  )
  ; (arraySet arr i body-val)
  ;                 (comprehensionHelper body var arr size (+ 1 i) env)
  

  (define stackPushHelper
    (lambda (stk val i)
      (let
        (
          (cur (arrayGet stk i))
        )
      (if (= cur 0)
        (arraySet stk i val)
        (stackPushHelper stk val (+ i 1))
      )
      
      )
    )
  )
  (define stackPopHelper
    (lambda (stk i remove?)
      (let
        (
          (next (arrayGet stk ( + i 1)))
          (popVal (arrayGet stk  i))
        )
      (if (= next 0)
        (if remove?
          (begin
            (arraySet stk i 0)
          popVal
          )
          popVal
        )
        (stackPopHelper stk (+ i 1) remove?)
      )
      
      )
    )
  )

  (define stackSizeHelper
    (lambda (stk counter)
      (let
        (
          (val (arrayGet stk counter))
        )
        (if (= 0 val)
          counter
          (stackSizeHelper stk (+ 1 counter))
        )
      )
    )
  )
  
  (define printHelper
    (lambda (arr len i)
      (if (= len (+ i 1))
        (eopl:printf
	  	    "~s\n"
		      (expval->num (arrayGet arr i))
        )
        (begin
          (eopl:printf
            "~s,"
            (expval->num (arrayGet arr i))
          )
          (printHelper arr len (+ i 1))
        )
      )
      
    )
  )
  
  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
  
  ; apply-procedure : Proc * ExpVal -> ExpVal
  
  ; uninstrumented version
    (define apply-procedure
     (lambda (proc1 arg)
       (cases proc proc1
         (procedure (bvar body saved-env)
           (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  ; (define apply-procedure
  ;   (lambda (proc1 arg)
  ;     (cases proc proc1
  ;       (procedure (var body saved-env)
	;   (let ((r arg))
	;     (let ((new-env (extend-env var r saved-env)))
	;       (when (instrument-let)
	; 	(begin
	; 	  (eopl:printf
	; 	    "entering body of proc ~s with env =~%"
	; 	    var)
	; 	  (pretty-print (env->list new-env))
  ;                 (eopl:printf "store =~%")
  ;                 (pretty-print (store->readable (get-store-as-list)))
  ;                 (eopl:printf "~%")))
  ;             (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  


  
