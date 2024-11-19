;;; Function -> rule for carrying out a computation given arguments
;;; A function can return a value, perform a side effect, both or one of these


;;; Pure function -> If given the same arguments, returns always the same value and does
;;; not perform any side effect

;;; In lisp, a function doesn't need to have strictly a name
;;; It is an object which can optionaly be associated with a symbol

;;; Terms for functions:

;;; 1 - Lambda expressions

;;; 2 - Primitives/built-in functions -> those implemented in C

;;; 3 - Special form -> A primitive that is like afunction, but does not evaluate
;;; all of its arguments in the usual way. Examples: if, and, while

;;; 4 - Macro -> A construct defined in Lisp. It translates a lisp expression into
;;; another expression which is to be evaluated instead

;;; 5 - Command -> An object which can be invoked via the command-execute primitive
;;; usually due to the user typing in a key sequence bound to that command


;;; 6 - Closure -> A function object like a lambda, except that it also encloses an
;;; environment of lexical variable bindings.

;;; 7 - Autoload object -> A placeholder for a real function. If the autoload object is called
;;; Emacs loads the file and then calls the proper function


;;; A LAMBDA EXPRESSION IS A LIST that starts with symbol lambda
;;; this indicates that the list represents a function

(setq plus2 
      (lambda (x)
	"adds 2"
	(+ x 2)))

(funcall plus2 2)

(defalias '2+ (apply-partially '+ 2)
  "Increment argument by two")

(2+ 10)


(defun secondplus2 (x) (+ x 2))

(secondplus2 2)
