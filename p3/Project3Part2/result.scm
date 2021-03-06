#lang racket
(require "top.scm")
(require "globalstack.scm")
(run "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)")
(define print-element (lambda (i) (pretty-print i) (display "\n*element*\n")))
(display "\n**split**\n")
(map print-element (get-expstack))
(display "\n**split**\n")
(map print-element (get-envstack))
(display "\n**split**\n")
(map print-element (get-stostack))
