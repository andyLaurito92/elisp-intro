;;nil has three separate meanings

;;1. Symbol with the name 'nil'
nil

;;2. Logical truth value false
(eq (not t) nil)

;;3. Empty list
(eq nil '())

;; For the Lisp reader, '()' and 'nil' are identical: they stand
;; for the same object, the symbol nil

;; Good convention, use nil as false and '() as empty list

(cons foo nil) ; Emphasize value false
(setq empty-list '()) ; Emphasize empty list

;; Expressions that you can evaluate is a form
;; Evaluating a form always produce a lisp object

(car '(1 2))

;; third is a macro that stands up for
;; (car (cdr (cdr '(a b c))))
(third '(a b c))



