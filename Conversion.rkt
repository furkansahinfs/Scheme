#lang scheme


;;decimal to binary
(define (decimaltobinary x)
  (letrec ((recursion (lambda (new x)
                   (cond ((zero? x) new)
                         (else (recursion (cons (remainder x 2) new)
                                     (quotient x 2)))))))
    (recursion '() x)))

(decimaltobinary 165)




;;decimal to octal
(define decimaltooctal
  (lambda (x)
    (cond ((zero? x) 0)
          ((zero? (quotient x 8)) x)
          (else (+ (* 10 (decimaltooctal (quotient x 8))) (remainder x 8))))))

(decimaltooctal 36)




;;binary to decimal
(define (binarytodecimal x)
    (if (zero? x)
        x
        (+ (* 2 (binarytodecimal (quotient x 10))) (modulo x 10))))


(binarytodecimal 1010101)




;;binary to octal
(define (binarytooctal x)
   (decimaltooctal (binarytodecimal x))) 

(binarytooctal 1010)



;; get digits of number ( helper function )
(define (getDigits x basenumber)
  (if (>= x basenumber)
	(append 
	  (getDigits (floor (/ x basenumber)) basenumber) 
	  (list (remainder x basenumber)))
	(cons x '())))

  
;;octal to binary
(define (octaltobinary x)
  (for/vector ([i (getDigits x 10)]) (decimaltobinary i)))

(octaltobinary 25)
       

  
;;octal to decimal
(define (octaltodecimal x)
    (if (zero? x)
        x
        (+ (modulo x 10) (* 8 (octaltodecimal (quotient x 10))))))


(octaltodecimal 370)




      

;; calculates value of char ( helper function )
(define (hexchartointeger c)
  (cond [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
        [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
        [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
        ))


;;hexadecimal to decimal
(define (hexadecimaltodecimal s)
   (for/sum ([i (in-range (string-length s))]) (* (expt 16 (- (- (string-length s) i) 1)) (hexchartointeger (string-ref s i)))))

       
(hexadecimaltodecimal "2Ab")



;;hexadecimal to binary
(define (hexadecimaltobinary s)
  (decimaltobinary(hexadecimaltodecimal s)))

(hexadecimaltobinary "2Ab")


;;hexadecimal to octal
(define (hexadecimaltotooctal s)
  (decimaltooctal(hexadecimaltodecimal s)))

(hexadecimaltotooctal "24")





;; calculates values of char ( helper function )
(define (hexcharcode x)
   (cond [(< x 10) (+ x (char->integer #\0))]
        [else  (+ x (- (char->integer #\A) 10))]))
      
(define (integertohexchar x)
  (integer->char (hexcharcode x)))


;; decimal to hexadecimal
(define (decimaltohexadecimal x)
  (list->string
   (let loop ([recval null] [x x])
     (if (= x 0)
         recval
         (loop (cons (integertohexchar (bitwise-and x 15)) recval)
               (arithmetic-shift x -4))))))

(decimaltohexadecimal 165)


;; binary to hexadecimal
(define (binarytohexadecimal x)
  (decimaltohexadecimal (binarytodecimal x)))

(binarytohexadecimal 1010101)

;; octal to hexadecimal
(define (octaltohexadecimal x)
  (decimaltohexadecimal (octaltodecimal x)))

(octaltohexadecimal 1001)


