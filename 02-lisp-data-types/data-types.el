;;; DATA TYPES

;;; A data type is a set of possible objects
;;; Every object belongs to at least one type --> Types can overlap,
;;; and objects can belong to two ore more types

;;; We can ask if an object is of a Type A, but we can't ask an object's type :O

;;; Primitive types -> integer, float, cons, symbol, string, vector, hash-table, subr,
;;; byte-code function and record
;;; Special types -> buffer (related to editing)

;;; Each primitive type has a corresponding lisp function that checks wether an object
;;; is a member of that type

;;; Objects in lisp are self-typing

;;;; In most languages, the programmer must declare the data type of each variable, and
;;;; the type is known by the compiler but not represented in the data. Such type
;;;; declaration do not exist in Elisp. A Lisp variable can have any type of value, and
;;;; it remembers whatever value you store in it, type and all

;;;; PRINTED REPRESENTATION

;;; The printed representation of an object is the format of the output generated by the
;;; function prin1
;;; The read syntax of an object is the format of the input accepted by the Lisp reader
;;; (the function read) for that object. THis is not necessarily unique

;;; Some objects don't have a read syntax. These objects are printed in hash notation #<buffer example>
;;; Example:
(current-buffer)

;;; In other languages, an expression is text; It has no other form. In Lisp, an expression
;;; is primarily a Lisp object and only secondairly the text that is the object's read syntax

;;; When you evaluate an expression interactively, the lisp interpreter first reads the textual
;;; representation of it, producing a Lisp object, and then evaluates that object.
;;; Evaluation and reading are separate activities.
;;; Reading returns the Lisp object represented by the text that is read
;;; The object may or may not be evaluated later


;;; NOTE: See Comment conventions here -> https://www.gnu.org/software/emacs/manual/pdf/elisp.pdf#Comment%20Tips

;;; Summary:
;;; Three ; for sections, headers
;;; Two ; comments should be aligned to the same level of identation of the code
;;; Example
;; (prog1 (setq auto-fill-function
;; ...
;; ...
;; ;; Update mode line.
;; (force-mode-line-update)))
;;; One ; for comments in the right margin
;;;
;; (setq base-version-list          ; There was a base
;; (assoc (substring fn 0 start-vn) ; version to which
;; file-version-assoc-list))        ; this looks like
;;                                  ; a subversion.

;;; PROGRAMMING TYPES
;;; 2 big families: Those related to lisp programming and those related to editing
;;; We start with the former ones:

;;; INTEGER TYPE

;;; There are 2 kinds of integers
;;; Small integers (called fixnums) --> The range of values for a fixnum depends on the machine. The minimum range is
;;; −536,870,912 to 536,870,911  (30 bits; i.e., −229 to 229 − 1) but many machines provide a wider range
;;; Large integers (called bignums) --> Can have arbitrary precision. Operations that overflow a fixnum will return a bignum instead

;;; All numbers can be compared either using eql or = or. Fixnums can also be compared with eq

;;; To test wether a number is a fixnum or a bignum you can use the predicates fixnump and bignump

(bignump 3)

(fixnump 3)

most-negative-fixnum

; Because the operation overflows, it returns a bignum
(bignump (+ most-positive-fixnum 10))


;;; FLOATING TYPE

;;; The C data type double


;;; CHARACTER TYPE

;;; A character is nothing more than an integer. Character are represented by their character code

?A ; Read syntax for alphanumeric characters

?\( ; Bc parenthesis is a special character we need to scape it
?\\ ; Scaping character \


?\a ⇒ 7 ; control-g, C-g
?\b ⇒ 8 ; backspace, BS, C-h
?\t ⇒ 9 ; tab, TAB, C-i
?\n ⇒ 10 ; newline, C-j
?\v ⇒ 11 ; vertical tab, C-k
?\f ⇒ 12 ; formfeed character, C-l
?\r ⇒ 13 ; carriage return, RET, C-m
?\e ⇒ 27 ; escape character, ESC, C-[
?\s ⇒ 32 ; space character, SPC
?\\ ⇒ 92 ; backslash character, \
?\d ⇒ 127 ; delete character, DEL

;;; For unicode caracters just use ?\N{NAME}
?\N{LATIN SMALL LETTER A WITH GRAVE} ; equivalent to ?a, Unicode character U+00E0 

;;; CONTROL CHARACTERS

?\C-i ; Control-character syntax equivalent to Ctrl + i
?\^I ; Equivalent to above

;;; Only control characters allowed are those that exist in ASCII


;;; META CHARACTERS

?\M-A ; Meta + A



;;; SYMBOL
;;; A symbol is an object with a name. The symbol name serves as the printed representation
;;; of the symbol

;;; A symbol can serve as a variable, as a function name, or to hold a property list.
;;; A symbol whose name starts with : is called a keyword symbol

(setq asymbol 'foo)
(symbolp asymbol)



;;; SEQUENCE TYPES

;;; Sequence = ordered set of elements
;;; 2 types --> 1) Lists and 2) Arrays


;;; Lists --> Can hold elements of any type, and its length can be easily changed by adding
;;; or removing elements (pretty much like in Python)

;;; Arrays -> Fixed-length sequences (again, pretty much like Python). They are subdivided into
;;; strings, vectors, char-tables and bool-vectors

;;; Vectors --> can hold elements of any type, whereas string elements must be characters and
;;; bool-vector elements must be t or nil
;;; Char-tables are like vectors, except that they are indexed by any valid character code

;;; It is generally impossible to read the same sequence twice, since sequences are always created
;;; anew upon reading <---- WUT?

;;; CONS cell and List types
;;; cons cell is an object that consists of two slots --> car slot and cdr slot (Like a tuple)
;;; Each slot can hold any Lisp object

;;; A list is a series of cons cells, linked together so that the CDR slot of each cons cell holds
;;; either the next cons cell or the empty list. Most con cells are used as parts of lists, we
;;; refer to any structure made out of cons cells as a list structure


;;; ConsCell[CAR val1, CDR ConsCell[CAR val2, CDR ConsCell[CAR val3, nil]]] <---Representation of---> val1 -> val2 -> val3

;;; A Lisp list works as a linked list built up of cons cells

;;; Objects that are not cons cells are called ATOMS

'(A 2 "A") ; Upon reading, each object inside the parentheses become an element of the list.
'()	   ; That is, a cons cell is made for each element. The CAR slot of the cons cell holds
nil        ; The element, and its CDR slot refers to the next cons cell of the list, which holds the next element
'("A ()")  ; in the list

;;; The names CAR and CDR derive from the history of Lisp. Those names were instructions in the IBM 704 computer 

'(rose violet buttercup)

;; Representatino of the above list can be seen below

;;  --- ---      --- ---      --- ---
;; |   |   |--> |   |   |--> |   |   |--> nil
;;  --- ---      --- ---      --- ---
;;   |            |            |
;;   |            |            |
;;    --> rose     --> violet   --> buttercup
