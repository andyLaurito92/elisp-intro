;;; Emacs supports 2 numeric data types -> integers and floating-point numbers


1.5e2 ; 1.5 * 10^2


;;; For non decimal integers -> #{radix}{digits}
;;; Radixes can be -->
;;; b for binary
;;; o for octal
;;; x for hex
;;; radixr for radix radix

;;; Example:

#b101100 ; Uses two's complement notation
#o54
#x2c

;;; Text characters are represented by integers. Any integer between zero
;;; and the value of (max-char), inclusive, is ocnsidered to be valid as a character

(max-char)

;;; No unicode?

;;; The range of values for bignums is limited by the amount of main memory, by machine
;;; characteristics such as the size of the word used to represent a bignum’s exponent, and by
;;; the integer-width variable.

integer-width

;;; Range of floating-point numbers is the same as the range of the C data type double on
;;; the machine you're using

(setq not-a-number (/ 0.0 0.0)) ; Returns NaN

;;; Useful functions

(isnan not-a-number)

(frexp 2.3) ; Returns (s . e) where x = s2^e


;;; Note: It is often a bad idea to check for equality in floating point arithmetic
;;; It's beter to check for approximate equality

;;; Function to check wether 2 floating point numbers approximate
;;; each other by a factor variable

(defvar fuzz-factor 1.0e-6)
(defun approx-equal (x y)
  (or (= x y)
      (< (/ (abs (- x y))
	    (max (abs x) (abs y)))
	 fuzz-factor)
      )
  )

(approx-equal 3 3.3)

;;; Returns pseudo-random integer
(random)

(random 3) ; Limit integer randomness up to 3 randomness

;;; If limit is a positive integer, the value is chosen to be nonnegative and less than limit.
;;; Otherwise, the value might be any fixnum, i.e., any integer from most-negative-
;;; fixnum through most-positive-fixnum (see Section 3.1 [Integer Basics], page 38).

;;; If limit is a string, it means to choose a new seed based on the string’s contents. This
;;; causes later calls to random to return a reproducible sequence of results.

;;; If limit is t, it means to choose a new seed as if Emacs were restarting. This causes
;;; later calls to random to return an unpredictable sequence of results.
