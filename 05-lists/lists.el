;; A list is a sequence of elements
;; List are not a primitive data type. Lists are built up from cons cells :)

;; cons cell -> data object that represents an ordered pair.

;; nil is both a symbol and an empty list

(listp nil) ; t

;; Becuase nil is a list, I can ask for its car!
(car nil) ; nil
(cdr nil) ; nil

;; Dotted notation
(setq dotted-list '(red . blue)) ; cons-cell using the dotted pair notation.
(car dotted-list)
(cdr dotted-list)

(atom dotted-list)
					;
;;; Because lists are a sequence of cons cells, we can either
;;; ask listp or consp (predicate for cons cells)
(consp dotted-list)
(consp '(1 2 3))


;;; Null returns true if object is nil
(null '(1))
(null '("red" "orange"))
(null '())

(setq mystack '(1 2 3))
(setq first-elem
      (pop mystack))
mystack
first-elem

;;; Note: A proper list is a list whose final element is nil
;;; in other words, the last cons cell has to have as cdr nil

(proper-list-p '(1 2 3)) ; Length of a list


;;; Accessing elements

(car '(1 2 3)) ; 1

(cdr '(1 2 3)) ; '(2 3)

(nth 2 '(1 2 3))

(nthcdr 2 '(1 2 3 4 5)) ; Also can be seen as skip n elements and return the rest

(defun mytake (n list)
  "Implemeneting take. Recieves an integer n and
	returns the first n elements. Nil if n is <=0"
  (let ((res '()))
    (dotimes (i n (reverse res)) ; Remember that (reverse res) is called at the end!
      (setq res (cons (nth i list) res))
     )
   )
)
(mytake 3 '(1 2 3 4 5 6 7))

;;; We need to define the error previous to be able to use it in a condition-case
(define-error 'wrong-argument-error "Wrong argument provided")
(defun mybutlast (x &optional n)
  "Returns list x with the last element, or last n elements, removed"
  (setq n (or n 1))
  (if (< n 0)
      (signal 'wrong-argument-error '("Negative numbers not allowd"))
    n)
  (mytake (- (proper-list-p x) n) x)
)

;;; Writing tests in elisp using ert. See https://www.gnu.org/software/emacs/manual/html_node/ert/Introduction.html for more details
;;; For running interactively the test, run M-x ert RET t RET

(ert-deftest testing-mybutlast ()
  "Tests on mybutlast implementation"
  (should (equal (mybutlast '(1 2 3 4)) '(1 2 3)))
  (should (equal (mybutlast '(1 2 3 4) 2) '(1 2)))
 )

(mybutlast '(1 2 3 4))
(mybutlast '(1 2 3 4) 2)

(condition-case err
    (mybutlast '(1 2 3 4) -1)
  (wrong-argument-error
   (message "You cannot pass negative arguments!: %s" err)))