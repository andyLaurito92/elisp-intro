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
;; rose, violet and buttercup are atoms

;; Another way of representing this would be
;;  ---------------       ----------------       -------------------
;; | car   | cdr   |     | car    | cdr   |     | car       | cdr   |
;; | rose  |   o-------->| violet |   o-------->| buttercup |  nil  |
;; |       |       |     |        |       |     |           |       |
;;  ---------------       ----------------       -------------------



;;; DOTTED PAIR NOTATION

;;; General syntax for cons cells that represents the CAR and CDR explicitly.
;;; In the syntax, (a . b) stands for a cons cell whose CAR is the object a
;;; and whose CDR is the object b

(setq myconscell '(1 . 3))
(consp myconscell)


;;; Note: The list (rose violet) is equivalent to (rose . (violet))


;;; ASSOCIATION LIST TYPES = DICTIONARIES
(setq alist-of-colors '((rose . red) (lily . white) (buttercup . yellow)))

;;; ARRAY TYPE

;;; Composed of an arbitrary number of slots for holding or referering to other Lisp objects
;;; Array is indexed while lists are not --> Accessing an element of a list is O(N)

;;; 4 types of arrays: 1) Strings, 2) Vectors, 3) bool-vectors and 4) char-tables
;;; Char-tables are sparse arrays indexed by any valid character code

;;; Strings are array of characters. Strings are constants


"foo \^Ibar" ; A tab character embedded withing a string


"My string has
new lines in it"


;;; Non-ASCII Characters in Strings ---> What about UNICODE? Doesn't seem that elisp treats strings as the unicode sandwich
;;, See https://github.com/andyLaurito92/fluent-python/blob/3a0d754eeff4ea0aefec792c72b21bfb4588edf8/chapter04-unicode-text-vs-bytes/unicode-vs-bytes.py#L10
;;; to remember about this concept :)

;;; 2 texts representations -> Multibyte & unibyte
;;; Unibyte -> store raw bytes, value between 0 and 255
;;; Multibyte -> store human-readable text. Values between 0 to 4194303
;;; Characters above 127 are non-ASCII


;;; Unicode characters are automatically treated as multibytes

;;; TEXT PROPERTIES IN STRINGS

;;; A string can hold properties for the characters it contains.

;;; #("characters" property-data...)
;;; Example

;;; #("foo bar" 0 3 (face bold) 3 4 nil 4 7 (face italic))

;;; Represents a string whose textual contents are 'foo bar',
;;; in which the first three characters have a face property
;;; with value bold, and the last three have a face property
;;; with value italic (The fourth character has no text
;;; properties, so its property list is nil)


;;; VECTOR TYPE
;;; Vector = one-dimensional array of elements of any type
;;; O(1) to access elements
(vectorp '[1 2 3])

;;; CHAR-TABLE type
;;; One dimensional array of elements of any type,
;;; indexed by character codes.
(make-char-table 'test [1])

;;; BOOL-VECTOR TYPE

;;; One dimensional array whose elements must be t or nil
;;; First argument is the length and second is the default value
;;; for each elements

(setq test (make-bool-vector 3 t)) ; Result -> #&3"^G" bc C-g is 111 (3 trues) 111 = 1 + 2 + 4 = 7
(bool-vector t nil t); Result -> #&3"^E" -> 69 --> 1000101

?\C-g
?\^G
?E ; 


;;; Printed representation is a string, except that it begins with
;;; #& followed by the length. The string constant that follows
;;; specifies the contents of the bool vector as a bitmap - each
;;; character in the string contains 8 bits, which specify the
;;; next 8 elements of the bool vector. The least significant
;;; bits of the character correspond to the lowest indices in
;;; the bool vector


;;; HASH-TABLE (alias dictionaries)

(setq testing-hash
      (make-hash-table :test 'equal))

(puthash 'aa 9 testing-hash)
(gethash 'aa testing-hash)

(setq my-hash
      #s(hash-table size 30 test equal data ("aa" 3 "bb" 9)))

(gethash "aa" my-hash)
(hash-table-count my-hash)
		 

;;; FUNCTION TYPE

;;; Lisp functions are objects. A non-compiled function in Lisp is a lambda expression
;;; This is: a list whose first element is the symbol lambda

;;; In lisp, a function has no intrinsic name. A lambda expression can be called as a
;;; function even though it has no name; to emphasize this, we also call it an anonymous
;;; function. A named function in Lisp is just a symbol with a valid function in its
;;; function cell

(setq times2
      (lambda (x)
	(* x 2))
      )

(functionp times2)
;;; You can construct or obtain a function object at run time and then call it with the
;;; primitive functions funcall and apply
(funcall times2 3)


;;; MACRO TYPE

;;; A Lisp macro is a user-defined construct that extends the Lisp language. It is represented
;;; as an object much like a function, but with different argument-passing semantics. A Lisp
;;; macro has the form of a list whose first element is the symbol macro and whose CDR is a
;;; Lisp function object, including the lambda symbol

;;; Lisp macro are usually defined with the built-in defmacro macro, but any list that begins
;;; with macro is a macro as far as Emacs is concerned
;;; LISP MACROS != KEYBOARD MACROS

;;; To talk more about this later :)


;;; PRIMITIVE FUNCTION TYPE

;;; Function defined in C, also called subrs (subroutines) or built-in functions
;;; Most primitive functions evaluate all their arguments when they are called.
;;; When a primitife function doesn't evaluate all its arguments, it's called a special form

;;; Redefinition of built-in functions is discouraged bc Lisp functions might call the new
;;; re-defined function, but other pre-compiled functions will probably call the original
;;; primitive function (this is pretty much like in Python)

(symbol-function 'car) ; Acess the function cell of the symbol

(subrp (symbol-function 'car)) ; Is this a primitive function?

;;; BYTE-CODE FUNCTION TYPE

;;; Byte-code function objects are produced by byte-compiling Lisp code. Internally, a byte
;;; code function object is much like a vector; however, the evaluator handles this data type
;;; specially when it appears in a function call.


;;; Record type
;;; A record is much like a vector, however the first element is used to hold its type as
;;; returned by type-of

(type-of times2)

;;; TYPE DESCRIPTOR

;;; Is a record which holds information about a type. Slot 1 in the record
;;; must be a symbo naming the type

;;; Example: cl-structure-class

;;; AUTOLOAD TYPE

;;; Is a list whose first element is the symbol autoload. It is stored as the function
;;; definition of a symbol, where it serves as a placeholder for the real definition
;;; The autoload object says that the real definition is found in a file of Lisp code
;;; that should be loaded when necessary. It contains the name of the file, plus some
;;; other information about the real definition


;;; Finalizer Type

;;; A finalizer object helps Lisp code clean up after objects that are no longer needed.
;;; A finalizer holds a Lisp function object. When a finalizer object becomes unreachable
;;; after a garbage collection pass, Emacs calls the finalizer's associated function object
;;; When deciding wether a finalizer is reachable, Emacs does not count references from
;;; finalizer object themselves, allowing you to use finalizers without having to worry about
;;; aciddentaly capturing references to finalized objects themselves

;;; Emacs runs a given finalizer object's exactly once

;;; (make-finalizer function)


;;; Editing types --> Types uniques to the context of EMACS

;;; Buffer Type

;;; Buffer -> object that contains text that can be edited.
;;; Most buffers hold the contents of a disk file
;;; A buffer doesn't need to be displayed in a window!

;;; Each buffer has a designated position called point; Most editing commands act on the contents of the current buffer
;;; in the neighborhood of point

;;; The content of a buffer are much like a string, but buffers are not used like strings in elisp.
;;; For example: You can insert text efficiently into an existing buffer, altering the buffer's contents,
;;; whereas inserting text into a string requires concatenating substrings and the result is an entirely new string object

;;; Personal note --> Buffers sound pretty muck like StringBuilders of Java (with the exception of the point attribute)
;;; It's a super interesting abstraction that it's used a lot

;;; Several other data structures are associated with each buffer:
;;; Local syntax tables, local keymaps, list of buffer-local variable bindings,
;;; overlays and text properties for the text in the buffer

;;; Local keymap and variable list contain entries that individually override global bindings of values.
;;; These are used to customize the behaviour of programs in different buffers, without actually changing
;;; the programs.

;;; A buffer may be indirect: It shares text of another buffer, but presents it differently

(current-buffer)


;;; Marker Type

;;; Marker denotes a position in a specific buffer. Marker has 2 components:
;;; 1) The buffer, 2) The posiiton

;;; Changes in the buffer's text automatically relocate the position value as necessary
;;; to ensure that the marker always points between the same 2 characters in the buffer

(point-marker)


;;; Window Type

;;; Portion of the screen that emacs uses to display buffers. Every live window has
;;; an associated buffer
;;; On contrast, a buffer can appear in 0, 1 or multiple windows
;;; Windows are grouped on the screen into frames; Each window belongs to 1 and only 1 frame

;;; There might be multiple windows, but only 1 is the selected window (window where cursor is)

(selected-window)


;;; Frame type

;;; Frame is the screen area that contains 1 or more emacs windows

(selected-frame)

;;; Terminal type -> device capable of displaying 1 or more emacs frames
(get-device-terminal nil)

;;; Window configuration type

;;; Stores information about the positions, sizes, and contents of the windows
;;; in a frame, so you can recreate the same arrangement of windows later

(current-window-configuration)

;;; Frame configuration type

;;; Stores information about the positions, sizes, and contents of the windows
;;; in ALL frames. This is a list whose CAR is frame-configuration and whose CDR
;;; is an alist. Each alist element describes one frame, which appears as the CAR
;;; of that element


;;; Process Type

;;; A process is a Lisp object that designates a subprocess created by the EMACS process.
;;; Programs such as shells, GDB, ftp, and compilers run in an Emacs subprocess created by
;;; the Emacs process.

;;; An Emacs subprocess takes textual input from Emacs and returns textual output to Emacs
;;; for further manipulation. Emacs can also send signals to the subprocess

(process-list)


;;; Thread Type

;;; A thread in Emacs represents a separate thread of Emacs Lisp execution.
;;; It runs its own Lisp program, has its own current buffer and can have
;;; subprocesses locked to it, this is, subprocesses whose output only this thread
;;; can accept.

(all-threads)


;;; Mutex type

(setq testing (make-mutex "testing-mutex"))
(mutexp testing)

;;; Condition Variable Type

;;; A condition variable is a device for a more complex thread syncrhonization
;;; than a mutex.
;;; A thread can wait on a condition variable, to be woken up when some other thread
;;; notifies the condiiton

(make-condition-variable testing)


;;; Stream type

;;; Object that can be used as a source or sink for characters
;;; Many different types can be used this way: markers, buffers, strings, and functions

;;; Keymap Type

;;; A keymap maps keys typed by the user to commands
;;; A keymap is a list whose CAR is the symbol keymap


;;; Overlay type

;;; Overlay specifies properties that apply to a part of a buffer usually temporarily
;;; in a different display style.

(overlay-lists)


;;; Font type

;;; 3 Types: 1) Font objects, 2) Font specs & 3) Font entities


;;; Read syntax for circular objects (Syntax for referentiating a lisp object)

;;; To represent shared or circular structures within a complex of Lisp objects, you
;;; can use the reader constructs '#n=' and '#n#'

;;; Use '#n=' before an object to label it for later reference. Subsequently you can
;;; use #n# to refer the same object in another place.

;;; Example:

(setq x '(#1=(a) b #1#)) ; This equality is at object level, is not the equal equality

(eq
 (nth 0 x)
 (nth 2 x)) ; True, elements refer to the same lisp object

(setq y '((a) b (a)))

(eq
 (nth 0 y)
 (nth 2 y)) ; Not equal, they are different lisp objects


;;; You can also create circular structures like this:

(setq circular-list
      '(#1=(a #1#))
      )

;;; Didn't work as I expect :) --> To review
(car (car circular-list))


;;; Type predicates

;;; Emacs lisp doesn't perform type checking on the arguments passed to a function
;;; It could not do so, since function arguments in Lisp do not have declared types
;;; It is therefore up to the individual function to test wether each actual argument
;;; belongs to a type that the function can use


;;; All built-in functions do check the types of their actual arguments

;;; Most common way to check the type of an object is to call a type predicate function.
;;; Emacs has a type predicate for each type, as well as some predicates for combinations
;;; of types ---> Duck typing

;;; A type predicate function takes one argument; It returns t if the argument belongs to
;;; the appropriate type, and nil otherwise
;;; Example of usage:

(setq list '())
(defun add-on (x)
  (cond ((symbolp x)
	 ;; If X is a symbol, put it on LIST
	 (setq list (cons x list)))
	((listp x)
	 ;; If X is a list, add its elements to LIST
	 (setq list (append x list)))
	(t
	 ;; Throw invalid argument if neither symbol nor list
	 (error "Invalid argument %s in add-on" x))
   )
  )

(add-on 'a)
(add-on '(1 2))
(add-on 3)

;;; To know the type of an object, use type-of
;;; Note: The value is one of the symbols bool-vector, buffer, char-table, compiled-function,
;;; condition-variable, cons, finalizer, float, font-entity, font-object,
;;; font-spec, frame, hash-table, integer, marker, mutex, overlay, process,
;;; string, subr, symbol, thread, vector, window, or window-configuration.

(type-of 3)
(type-of ())
(type-of 'a)


;;; Equality predicates

;;; eq --> tests if 2 objects are the same lisp object
;;; equal --> tests if 2 objects have equal components


(eq "abc" "abc")
(equal "abc" "abc")

;;; Comparisson between strings is canse sensitive, but does not take into account
;;; the text properties, it compares only the characters in the strings
;;; In case you want to compare properties, use equal-including-properties


;;; Note: 2 buffers are nevered equal, even if their textual contents are the same

;;, For equal, equality is defined recursively


;;; Mutability example

(let*
    ((x (list 0.5))
     (y (eval (list 'quote x))))
  (setcar x 1.5) ;; The program should not do this.
y)
