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

;;; Second key difference: a MACRO RETURNS LISPS EXPRESSION, while functions return VALUES. This lisp
;;; expression is called the "exapnsion of the macro"
;;; The lisp interpreter proceeds to evaluate the expansion as soon as it comes back from the macro

;;; Note: The expansion of the macro might contain other calls to other macros

(macroexpand '(inc x)) ; (setq x (1+ x))

;;; Note: emacs tries to expand macros when loading an uncompiled Lisp file. This is not always possible, but if it is,
;;; it speeds up subsequent executions

(defmacro inc2 (var1 var2)
  (list 'progrn
   (list 'inc var1)
   (list 'inc var2)
   ))

(setq y 3)
;;; Macroexpand doesn't expand subexpressions, see the following example
(macroexpand '(inc2 x y)) ; (progrn (inc var1) (inc var2))

;;; In the above, if you want to expand inc subexpressions you need to
;;; call macroexpand-all
(macroexpand-all
 '(inc2 x y)) ;; Not working as expected


;;; Macros and Byte Compilation

;;; Question: Why don't we directly make the macro evaluate the lisp expression
;;; instead of returning it so it can be evaluated by the interpreter?
;;; Reason -> Compilation

;;; When a macro call appears in a Lisp program being compiled, the Lisp
;;; compiler calls the macro definition just as the interpreter would,
;;; and receives an expansion. Instead of evaluatin this expansion, it
;;; compiles the expansion as if it had appeared directly in the program!

;;; ==> Compiled code produces the value and side effects intended for
;;; the macro, but executes at full compile speed

;;; In order for compilation of macro calls to work, the macros must
;;; already be defined in Lisp when the calls to them are compiled


;;; Defining Macros

;;; A lisp macro object -> (macro lambda args . body)
;;; This is, a list whose car is macro and whose cdr is a function

;;; Exapnsion of the macro works by applying the function (with apply) to the
;;; list of unevaluated arguments


;;; Example of for in elisp
(defmacro for (var from init to final do &rest body)
  "Execute a simple \"for\" loop.
For example, (for i from 1 to 10 do (print i))."
  (list 'let (list (list var init))
        (cons 'while
              (cons (list '<= var final)
                    (append body (list (list 'inc var)))))))

(setq print 'message) ; To be more python familiar
(print "hola")

;;; Note that from to and do are syntaxis sugar but mandatory
(for i from 1 to 3 do
     (setq square (* i i))
     (princ (format "\n%d %d" i square)))


;;; If from, to and do are not receieved, an error is raised
(for x 2 3 do
     (message "hola"))


;;; The above for doesn't have lexical-scoping as in other languages
;;; Meaning that variables define in the body will be globally defined
square
