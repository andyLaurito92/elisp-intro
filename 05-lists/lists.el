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


;;; TODO: as an exercise, implement some of the list functions :)
