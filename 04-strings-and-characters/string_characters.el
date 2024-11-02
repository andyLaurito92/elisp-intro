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

;; You can use a regex as well
(split-string "Something to test this function" "[ht]i")


(string-clean-whitespace "  let's clean this    string    !    ")
(string-trim-left "  let's clean this    string    !    ")
(string-trim-right "  let's clean this    string    !    ")


;;; Modifying strings

(setq a-testing-string "this is my string")
(aset a-testing-string 3 ?x)
(print a-testing-string)

;; string= returns t if the cahracters of the two strings match exactly
;; This function equals string-equal
(string= "a string" "a string")

;; string< returns t if the first string is greater, otherwise return nil.
;; Keep in mind that lower case letters have higher numeric value in the ASCII
;; character set than their upper case counterparts
;; digits and many punctuation characters have a lower
;; numeric value than upper case letters. An ASCII character is less than any non-ASCII
;; character; a unibyte non-ASCII character is always less than any multibyte non-ASCII
;; character
(string< "something starting" "something ending")

;; This function compares strings lexicographically, except it treats sequences of
;; numerical characters as if they comprised a base-ten number
(string< "abc" "ABC")
(string-version-lessp "abc" "ABC")
(string-version-lessp "ABC" "abc")

(string-prefix-p "hey" "hey how are you?")
(string-suffix-p "ho" "hey how are you? hoho")

(string-search "myneedle" "The haystack was so big, that myneedle was all around the place!")
(string-search "myneedle" "no needle this time")

;; See also
(describe-function 'compare-strings)
(describe-function 'string-distance)
(describe-function 'assoc-string)
