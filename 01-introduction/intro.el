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



(count-loop)


;; Some nomenclature:
;; - Functions -> The one we know already
;; - Command -> Functions thay may be called interactively
;; - Macros -> Process their arguments differently from functions (arguments are not evaluated)


;; Variable is a name bound to an object, which we call value. Although nearly
;; all variables can be set by the user, there are certain variables that are meant
;; to be changed by them. These type of variables are called user options


;; Some informative functions

; Returns a string describing the version of emacs that is running.
; It is useful to include this string in bug reports
(emacs-version)

; If call with a non-nill value, it inserts the text in the buffer before point and returns nil
(emacs-version t)

; Variable: Output style in current-time or nil if no info available
emacs-build-time

; Variable: String such as 29.1. If the value returned is
; a three numeric components, the vesrsion is an unreleased test version
emacs-version

emacs-build-number ; Variable: Number of times this emacs was built
