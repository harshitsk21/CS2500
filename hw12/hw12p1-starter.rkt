;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 12, Problem 1 ==

; In this problem you will be working with the shift cipher, also known as Caesar sipher,
; one of the simplest encryption techniques (https://en.wikipedia.org/wiki/Caesar_cipher).

; The basic idea is to shift every letter in an alphabet by a constant amount (circling
; back if it goes past the end of the alphabet). For example, shifting the alphabet by 1
; would produce...

; Start:  ABC ... XYZ
; Cipher: BCD ... YZA

; The constant can also be negative, such as -3...

; Start:  ABC ... XYZ
; Cipher: XYZ ... UVW 

; To help, you've been supplied two functions to help translate between upper-case letters
; (e.g., "A" "B" "C") and the corresponding offsets from the start of the English alphabet
; (e.g.,  0   1   2)...


; alphabet : Nat[0, 25] -> 1String
; provides the letters of the alphabet given an
; English-alphabet offset (i.e., 0=A and 25=Z)

(check-expect
 (alphabet 0)
 "A")

(check-expect
 (alphabet 25)
 "Z")

(define (alphabet n)
  (string
   (integer->char (+ 65 n))))



; alphabet-offset : 1String -> Nat[0, 25]
; provides the offset in the English alphabet (i.e., A=0, Z=25)

(check-expect
 (alphabet-offset "A")
 0)

(check-expect
 (alphabet-offset "Z")
 25)

(define (alphabet-offset letter)
  (-
   (char->integer
    (string-ref letter 0))
   65))



; (In case you are curious about the magic number 65, this conversion is based on the ASCII
; character encoding: https://en.wikipedia.org/wiki/ASCII).



; TODO #1: design the function encrypt that takes a string (you can assume it is only composed of
; upper-case letters and spaces) and a shift and produces the corresponding encrypted message,
; ignoring the spaces. You have been provided some example tests for clarity.

; Hint: the modulo function can help deal with shift numbers that are less than 0 or greater than
; 25; it basically works like remainder but works better in this context for negative numbers.


; encrypt : String Real -> String
; Given a string a shift number outputes an alphabetical shift


(check-expect
 (encrypt "ABC" 0)
 "ABC")

(check-expect
 (encrypt "ABC" 1)
 "BCD")

(check-expect
 (encrypt "XYZ" 1)
 "YZA")

(check-expect
 (encrypt "ABC" 26)
 "ABC")

(check-expect
 (encrypt "ABC" -3)
 "XYZ")

(check-expect
 (encrypt "XYZ" -3)
 "UVW")

(check-expect
 (encrypt "XY " -3)
 "UV ")

(check-expect
 (encrypt "AB C " 2)
 "CD E ")

(check-expect
 (encrypt "ABC" 50)
 "YZA")

(check-expect
 (encrypt "" 4)
 "")


(define (encrypt str num)
  (local
    [(define IND-STR (explode str))]
    (foldr string-append ""
           (map alphabet
                (map (lambda (n) (if (= n -33) n (modulo (+ num n) 26)))
                     (map alphabet-offset IND-STR))))))


; TODO #2: design the function decrypt that takes an encrypted message, and tries to decrypt it.
; It does so by considering an additional pair of inputs, presumed to be a pairing between an
; original phrase and its encrypted version. Your function must try to find the constant offset
; between this pair, and then apply it to decrypt the first input; if such an offset isn't found
; then the function should return #false. Some tests have been supplied for clarity.

; To guide your design, you have also been supplied the signature and test for a helper (find-shift);
; you must meaningfully use this function in your solution without changing its signature. This
; function should start at a shift of 0, see if encrypting using that offset produces a matching
; encrypted string: if so, return that, otherwise continue counting up until 26 (a full cycle of the
; alphabet). Don't forget accumulator and termination statements as appropriate!


(check-expect
 (decrypt
  "YUNJBN AXDWM VH PAJMN CX JW J VRWDB"
  "A"
  "J")
 "PLEASE ROUND MY GRADE TO AN A MINUS")

(check-expect
 (decrypt
  ""
  "DEF"
  "ABC")
 "")

(check-expect
 (decrypt
  " "
  "ABC"
  "DEF")
 " ")


(check-expect
 (decrypt
  "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD"
  "DEF"
  "ABC")
 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")

(check-expect
 (decrypt
  "ABC"
  "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
  "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD")
 "DEF")

(check-expect
 (decrypt
  "JOHN"
  "ABC"
  "ABD")
 #false)

(check-expect
 (decrypt
  "PHONE"
  "ABC DEF"
  "KWB")
 #false)



; decrypt : String String String -> [Maybe String]
; Decrypts a supplied string according to shift between two other strings
; if no shift is detected outputs false

(define (decrypt s0 s1 s2)
  (cond
    [(boolean? (find-shift s2 s1)) #false]
    [(number? (find-shift s2 s1)) (encrypt s0 (find-shift s2 s1))]))


; find-shift : String String -> [Maybe Nat[0, 25]]
; given two strings, determines the alphabetical shift between the two,
; outputs false if no shift is detected

(check-expect
 (find-shift
  "ABC" "DEF")
 3)

(check-expect
 (find-shift
  "ABC" "ABD")
 #false)

(check-expect
 (find-shift
  "ABC" "ABD")
 #false)

(check-expect
 (find-shift
  "XYZ" "ABC")
 3)

(check-expect
 (find-shift
  "EFG" "ABCD")
 #false)

(check-expect
 (find-shift
  " " " ")
 0)

(check-expect
 (find-shift
  "ABC" "ABC")
 0)

(check-expect
 (find-shift
  "" "ABC")
 #false)

(check-expect
 (find-shift
  "" "")
 0)



(define (find-shift og encr)
  (local [; find-shift/acc : String String Nat -> [Maybe Nat[0, 25]]
          ; given two strings, determines the alphabetical shift between the two,
          ; outputs false if no shift is detected
          ; Accumilator: current shift in the alphabet 
          (define (find-shift/acc og encr num)
            (if (string=? encr (encrypt og num))
                num (if (= 26 (add1 num)) #false (find-shift/acc og encr (add1 num)))))]
    (find-shift/acc og encr 0)))
  
      
    
  
   





