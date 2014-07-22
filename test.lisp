(setq factorial (lambda (n) (if (eq n 0) 1 (* (factorial (- n 1)) n))))
(factorial 5)

(quote a)
(quote (a b c))

(atom (quote a))
(atom (quote (a b c)))
(atom (quote ()))
(atom (atom (quote a)))
(atom (quote (atom (quote a))))

(eq (quote a) (quote a))
(eq (quote a) (quote b))
(eq (quote ()) (quote ()))

(car (quote ()))
(car (quote (a)))
(car (quote (a b c)))

(cdr (quote ()))
(cdr (quote (a)))
(cdr (quote (a b c)))
