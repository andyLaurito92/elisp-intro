;;; Macros enable you to define new control constructs

;;; A macro is defined much like a function, BUT instead of telling HOW TO COMPUTE A VALUE
;;; it tells HOW TO COMPUTE ANOTHER LISP EXPRESSION which will in turn compute the value
;;; We call this expression the expansion of the macro

;;; Macros can do this because they operate on the UNEVALUATED EXPRESSIONS FOR THE ARGUMENTS
;;; not on the ARGUMENT VALUES as functions do. They can therefore construct an expansion containing these argument expressions or parts of them.


