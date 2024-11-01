;;; A string in elisp is an array that contains an ordered sequence of characters

;;; Unlike in C, elisp strings are not terminated by a distinguised character code

(prin1-char (aref "hey how are you" 1)) ; We can use array functions in a string
?e ; Ascii-number of e = 101


(setq string-test "hollo how r u?")
(string-match "\w" string-test) ; This is matching the character w and not a word
(prin1-to-string (aref string-test 8))

(string-width string-test) ; Don't use length

(make-string 5 ?x) ; Returns a string made up of count repetitions of character

(setq another-test "a super large string")
(substring another-test 2 7)

(substring another-test -6 nil) ; nil here stands for the length of the string

;;; Omitting argument end is equivalent to specifying nil

(substring-no-properties another-test) ; Copy of string with all text properties removed

(concat "abc" "cde")
(concat "jojo" nil "jiji")


;; split-string string &optional separators omit-nulls trim

;; If separators is nil (or omitted), the default is the value of split-string-default-
;; separators and the function behaves as if omit-nulls were t.

;; If omit-nulls is nil (or omitted), the result contains null strings whenever there are
;; two consecutive matches for separators, or a match is adjacent to the beginning or
;; end of string. If omit-nulls is t, these null strings are omitted from the result.

;; If the optional argument trim is non-nil, it should be a regular expression to match
;; text to trim from the beginning and end of each substring. If trimming makes the
;; substring empty, it is treated as null.

(split-string " two words ")
split-string-default-separators

(split-string "Something to test this function" " ")
