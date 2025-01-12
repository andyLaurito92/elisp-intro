;;; Macros enable you to define new control constructs

;;; A macro is defined much like a function, BUT instead of telling HOW TO COMPUTE A VALUE
;;; it tells HOW TO COMPUTE ANOTHER LISP EXPRESSION which will in turn compute the value
;;; We call this expression the expansion of the macro

;;; Macros can do this because they operate on the UNEVALUATED EXPRESSIONS FOR THE ARGUMENTS
;;; not on the ARGUMENT VALUES as functions do. They can therefore construct an expansion containing these argument expressions or parts of them.


;;; Example of macro
;;; Let's implement the C ++ operator (increment + 1)

(defun inc-fun (var)
  (setq var (1+ var)))


(defun inc-fun-with-set (var)
  (set var (1+ (symbol-value var))))

(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(setq x 3)

(inc x) ; x++

(inc-fun x) ; This doesn't work, just increments 1

x
(inc-fun-with-set 'x) ; This gets closer, but still not there.
;;; The problem with the aboe is that we are passing the symbols name 
;;; Not just the symbol as I would do with ++

;;; Why is this happening? :)
;;; Because inc-fun only adds 1 to the VALUE of x, not to x itself.
;;; This is because lisp passes arguments by copying them instead of
;;; referencing them

;;; Think how you would do it in Python. How would that look like?

;;; def myinc(x:int) -> None:
;;;     x += 1 # This would just add 1 to the value of x and nothing else, x is copied to myinc, is not a reference to the original value
;;;     # You could define global x, but then myinc would only work for variable x and no other variable, which is not what we want
;;;     x = 1 


;;; In the end, you would have the same problem than in elisp!

;;; Expansion of a macro call

;;; When elisp evaluates a macro call, it begins like an evaluation of a function. The main
;;; difference is that the arguemnts ARE NOT REDUCED TO THEIR VALUE, this means, that the macro
;;; receives the EXPRESSION passed by an user instead of THE VALUES that THOSE EXPRESSIONS REPRESENTS


;;; Question: Does the above mean that I recieve the symbols? Remember that a variable in the end is:
;;; (setq x 3) --> x -> 3 where 3 is the value and x is the symbol and -> is the relationshipt between
;;; variable & symbol

