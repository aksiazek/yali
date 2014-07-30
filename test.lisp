(defun factorial (n) (if (eq n 0) 1 (* (factorial (- n 1)) n)))
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

(cons (quote a) (quote (b c)))
(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))) )
(car (cons (quote a) (quote (b c))))
(cdr (cons (quote a) (quote (b c))))

(cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))

 ; in common lisp
(atom ()) -> t
;;even though (car ()) -> nil is supposed to only work for lists
; and (cond ()) -> says clause is not a list ??

;; clojure approach taken


;; interesting clojure vs common lisp cond
(cond (quote ()) '(nil))

;; Helpers

(defun null (x) (eq x ()))
(defun listp (x) (not (atom x)))
(defun caar (e) (car (car e)))
(defun cadr (e) (car (cdr e)))
(defun cdar (e) (cdr (car e)))
(cadr (quote ((a b) (c d) e)))
(defun caddr (e) (car (cdr (cdr e))))
(defun cadar (e) (car (cdr (car e))))
(defun caddar (e) (car (cdr (cdr (car e)))))
(defun and (x y) (cond (x (cond (y t) (t nil))) (t nil)))
(and (atom (quote a)) (eq (quote a) (quote a)))
(and (atom (quote a)) (eq (quote a) (quote b)))
(defun not (x) (cond (x nil) (t t)))
(not (eq (quote a) (quote a)))
(not (eq (quote a) (quote b)))

(defun append (x y) (cond ((null x) y) (t (cons (car x) (append (cdr x) y)))))
(append (quote (a b)) (quote (c d)))
(append () (quote (c d)))

(defun assoc (x y) (cond ((eq (caar y) x) (cadar y)) (t (assoc x (cdr y)))))
(assoc (quote x) (quote ((x a) (y b))))
(assoc (quote x) (quote ((x new) (x a) (y b))))

(defun pair (x y) (cond ((and (null x) (null y)) ()) ((and (listp x) (listp y)) (cons (cons (car x) (cons (car y) ())) (pair (cdr x) (cdr y))))))
(pair (quote (x y z)) (quote (a b c)))

;; grand test

(defun evcon (c a) (cond ((eval (caar c) a) (eval (cadar c) a)) (t (evcon (cdr c) a))))
(defun evlis (m a) (cond ((null m) ()) (t (cons (eval (car m) a) (evlis (cdr m) a)))))

(defun eval (e a) (cond ((atom e) (assoc e a)) ((atom (car e)) (cond ((eq (car e) (quote quote)) (cadr e)) ((eq (car e) (quote atom)) (atom (eval (cadr e) a))) ((eq (car e) (quote eq)) (eq (eval (cadr e) a) (eval (caddr e) a))) ((eq (car e) (quote car)) (car (eval (cadr e) a))) ((eq (car e) (quote car)) (car (eval (cadr e) a))) ((eq (car e) (quote cdr)) (cdr (eval (cadr e) a))) ((eq (car e) (quote cons)) (cons (eval (cadr e) a) (eval (caddr e) a))) ((eq (car e) (quote cond)) (evcon (cdr e) a)) (t (eval (cons (assoc (car e) a) (cdr e)) a)))) ((eq (caar e) (quote label)) (eval (cons (caddar e) (cdr e)) (cons (list (cadar e) (car e)) a))) ((eq (caar e) (quote lambda)) (eval (caddar e) (append (pair (cadar e) (evlis (cdr e) a)) a)))))
;; poprawka
(defun eval (e a) (cond ((eq e t) t) ((eq e nil) nil) ((atom e) (assoc e a)) ((atom (car e)) (cond ((eq (car e) (quote quote)) (cadr e)) ((eq (car e) (quote atom)) (atom (eval (cadr e) a))) ((eq (car e) (quote eq)) (eq (eval (cadr e) a) (eval (caddr e) a))) ((eq (car e) (quote car)) (car (eval (cadr e) a))) ((eq (car e) (quote car)) (car (eval (cadr e) a))) ((eq (car e) (quote cdr)) (cdr (eval (cadr e) a))) ((eq (car e) (quote cons)) (cons (eval (cadr e) a) (eval (caddr e) a))) ((eq (car e) (quote cond)) (evcon (cdr e) a)) (t (eval (cons (assoc (car e) a) (cdr e)) a)))) ((eq (caar e) (quote label)) (eval (cons (caddar e) (cdr e)) (cons (list (cadar e) (car e)) a))) ((eq (caar e) (quote lambda)) (eval (caddar e) (append (pair (cadar e) (evlis (cdr e) a)) a)))))

(eval t (quote ()))
(eval nil (quote ((nil t))))
(eval (quote x) (quote ((x a) (y b))))
(eval (quote (eq (quote a) (quote a))) ())
(eval (quote (cons x (quote (b c)))) (quote ((x a) (y b))))
(eval (quote (cond ((atom x) (quote atom)) (t (quote list)) ))    (quote ((x a b))) )
(eval (quote (cond ((atom x) (quote atom)) (t (quote list)) ))    (quote ((x (quote (a b)) ))) )

;; consider cond without a default clause, or just not exhausting conditions -> normal behavior nil, ours error
(eval (quote (cond ((atom x) (quote atom)) (t t) ))    (quote ((x (quote (a b)) ))) )
(eval (quote (cond ((atom x) (quote atom)) ))    (quote ((x (quote (a b)) ))) )
;; better evcon
(defun evcon (c a) (cond ((and (not (null (car c))) (and (listp (car c)) (eval (caar c) a))) (eval (cadar c) a)) ((not (null (cdr c)))  (evcon (cdr c) a))   ))
;; (defun evcon (c a) (cond ((and (null (car c)) (listp (car c))) (cond ((eval (caar c) a) (eval (cadar c) a)) (t (evcon (cdr c) a))) ) ))

(eval (quote (f (quote (b c)))) (quote ((f (lambda (x) (cons (quote a) x))))))
(eval (quote ((lambda (x) (cons (quote a) x)) (quote (b c)))) (quote ((f (lambda (x) (cons (quote a) x))))))

