; Functions in Scala without using built in functions
; By: Christian Magpantay
; Code Referenced: The Scheme Programming Language, 4th Edition
; Author: R. Kent Dybvig
; https://www.dyclassroom.com/programming/how-to-find-modulus-without-using-modulus-operator
; https://repl.it/repls/ModestVerticalTrace

; A function (binomial N k) that returns the binomial coefficients C(N, k), defined recursively as: 
; C(N,0) = 1, C(N, N) = 1, and, for 0<k < N, C(N, k) = C(N-1, k) + C(N - 1, k -1).
;
; using recursion
; take the params N k, set N to i and k to j
; base case, if j is 0, return 1    ( C(N,0)=1 )
; otherwise, set conds before continuing
;   cond if i = j, return 1           ( C(N, N) = 1 )
;   cond if j > i, return error       ( for 0<k < N )
;   else 
;       C(N, k) = C(N-1, k) + C(N - 1, k -1)
(define binomial
    (lambda (N k)
        (let ([i N] [j k])
            (if (zero? j)
                1
                (cond
                    [(= i j) 1]
                    [(> j i) "error"]
                    [else (+ ( binomial (- i 1) j ) 
                            ( binomial (- i 1)(- j 1) ) )]
                )
            )
        )
    )
)

; A function (mod N M) that returns the modulus remainder when dividing N by M.
;
; divide N by M and remove the remainder which gives us the quotient
; multiply the number above M which gives us the product
; subtract that N which gives us modulo
; return answer
(define (mod N M)
    (lambda N M)
        (let ( [ans (- N (* (truncate (/ N M)) M) )] )
            ans
    )
)

; A function (binaryToDecimal b) that takes a binary number and returns its decimal value. 
; (binaryToDecimal 1101) returns 13.
;
; using recusion
; base case, if b is zero, return b
; in the first half
;   divide b by 10, the remainder is the digit we need to start
;   divide b by 10 again, but round the number to give us ONLY the whole number 
;                           without the remainder
;   subtract the whole number from the remainder
;       resulting in the digit needed to convert
;
; the second half in which we add from the number above
;   add the number to the call of the binarytoDecimal function again
;   adding in the multiplication of 2 every time as the 
;           function is called, so is the power of 2 is increased
;   round the number final number
;   resulting number will be converted from binary
(define binaryToDecimal
    (lambda (b)
        (if (zero? b)
            b
            (round (+ (* (- (/ b 10) (round (/ b 10))) 10) (* 2 (binaryToDecimal (round (/ b 10))))) )
        )
    )
)

; A function (addBinary binaryList) that takes a list of binary numbers and returns their decimal sum. 
; (addBinary '(1101 111 10 101)) returns 27.
;
; uses binaryToDecimal function above
; let the sum or accumaltor be 0
; base case, if the list is null, return sum
; add sum with calling binaryToDecimal by passing in (car) function
;   and also add the rest of the list, recalling addBinary with (cdr) function
(define addBinary
    (lambda (binaryList)
        (let ([sum 0])
            (if (null? binaryList)
                sum
                (+ (+ sum (binaryToDecimal (car binaryList))) (addBinary (cdr binaryList)) )
            )
        )
    )
)

; A function (min list) that returns the smallest value in a simple list of integers.
;
; base case; if the rest of the list is null but the first element is present
;   return the first element
; otherwise,
;   check if the first element is smaller than the rest of the list
;       if true, return first element
;       otherwise, keep searching to return min element in list
(define min
    (lambda (list) 
        (if (null? (cdr list)) 
            (car list) 
            (if (< (car list) (min (cdr list)) ) 
                (car list)
                (min (cdr list)) 
            )
        )
    )
)

; A function (myRemove num list) that removes all occurrences of the integer num from a 
; simple list of integers, returning list with num removed. myRemove should return the 
; original list if atm is not found.
; 
; let rList be the list that removes the num
; base, if list is null, return rList
; otherwise, 
;   if num = car list
;   call myRemove to skip
;   otherwise,
;       append the element of car and use cons by
;       recalling the myRemove function with cdr of list to add 
;       all the other elements when num is not equal to car of list
; return removed all of the desired number list
(define myRemove
    (lambda (num list)
        (let ([rList '()])
            (if (null? list) 
                rList
                (if (= num (car list))
                    (myRemove num (cdr list))
                    (cons (append rList (car list)) (myRemove num (cdr list))
                )
            )
        )
    )
)

; A function (selectionSort list) that returns a simple list of integers in 
; ascending order using a recursive selection sort algorithm. Hint: use your min function.
;
; using recursion
; base-case, if the list is null, return empty list
; otherwise,
;   find the min of the list using min function
;   move it to the front
;   the call selectionSort function with the list
;   of the min removed from the list using the myRemove function
;   continually add the min of the new list of the removed min
;   until empty
;   return list in ascending order
(define selectionSort
    (lambda (list)
        (if (null? list)
            '()
            (cons (min list) (selectionSort (myRemove (min list) list) ) )
        )
    )
)
