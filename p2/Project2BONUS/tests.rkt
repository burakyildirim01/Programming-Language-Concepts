#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans)
    
  )))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ((list? sloppy-val) (rope-val sloppy-val))
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run

 ;; rope test cases here
 ;; write at least 6 test cases (two for each: fetch ith character, concatenate, substring) 
 (fetch-simple "fetch 'burak'[0]"  ( #\' #\b #\'))
 (fetch-nested "if zero?(0) then fetch 'alkan'[ -(5,3) ] else fetch 'alkan'[ -(2,2) ]"  ( #\' #\k #\'))

 (concatenate-simple "concate 'alkan' + 'akısu' "  ( #\' #\a #\l #\k #\a #\n #\a #\k #\ı #\s #\u #\'))
 (concatenate-simple-2 "concate 'burak' + 'yıldırım' "  ( #\' #\b #\u #\r #\a #\k #\y #\ı #\l #\d #\ı #\r #\ı #\m #\'))

 (substring-simple "let x = 5 in let y = 8 in substr 'comp 301'[x,y] " (#\' #\3 #\0 #\1 #\'))
 (substring-simple "substr 'Koç Universitesi' [-(-(5,3),2),-(5,-(5,3))] " ( #\' #\K #\o #\ç #\'))

 
 )