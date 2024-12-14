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
  (mytake (- (length x) n) x)
)

;;; Writing tests in elisp using ert. See https://www.gnu.org/software/emacs/manual/html_node/ert/Introduction.html for more details
;;; For running interactively the test, run M-x ert RET t RET

(ert-deftest mybutlast-when-no-integer-then-lastone-is-removed ()
  "When no integer is passed then it defaults to last element"
  (should (equal (mybutlast '(1 2 3 4)) '(1 2 3)))
 )
(ert-deftest mybutlast-when-integer-then-nelements-removed ()
  "When an integer N is passed then N elements are removed from the end of the list"
  (should (equal (mybutlast '(1 2 3 4) 1) '(1 2 3)))
  (should (equal (mybutlast '(1 2 3 4) 2) '(1 2)))
  (should (equal (mybutlast '(1 2 3 4) 3) '(1)))
  )
(ert-deftest mybutlast-when-negative-value-then-error-signal ()
  "When a negative integer is passed to the function a wrong-argument-error should be thrown"
  (should-error (mybutlast '(1 2 3 4) -1)
		:type 'wrong-argument-error))

;; How to handle case errors with condition-case
(condition-case err
    (mybutlast '(1 2 3 4) -1)
  (wrong-argument-error
   (message "You cannot pass negative arguments!: %s" err)))


;;; We can build al ist by doing this:
(list 1 2 3 4)

;; For creating a board game, we can use make-list

(setq tic-tac-toe-board
      (make-list 3 (make-list 3 '())))


;; Append works only for appending sequences to
;; a list, which should be the final value. For
;; adding aobjects into a list use cons
(append '(1) '(1 2))
(cons 1 '(1 2))


(apply 'append '((1 2 3) nil (a b c) nil))

(apply 'append '((1 2 3) nil (4 5 6) (8 9 10)))

;; ensure-list object
;; Returns object as a list
(ensure-list 1)

(dolist (elem (ensure-list 1))
  (princ elem))

;; princ -> Outputs the Printed representation of object
(princ 1)

;; number-equence --> like range in python
;; The difference is that number-sequence doesn't
;; yield values as in Python. Perhaps it exists
;; another function more similar to it?

(setq odd-numbers
      (number-sequence 1 10 2))

;; This is valid as well in Python!
(number-sequence 9 4 -2)
