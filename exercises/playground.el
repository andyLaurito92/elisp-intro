;;; Create a counter that increments by 2 rather than 1

(defun plus2 (x) (+ 2 x))

(plus2 3)

(setq plus2 
      (lambda (x)
	"adds 2"
	(+ x 2)))

(funcall plus2 2)

(defalias '2+ (apply-partially '+ 2)
  "Increment argument by two")

(2+ 10)


;; defun is just a macro. This macro eventually evaluates defalias
(defun secondplus2 (x) (+ x 2))

(secondplus2 2)

