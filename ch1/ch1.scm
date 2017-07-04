(* 2 3)

(define (cube x) (* x x x) )

(cube 8)

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)) ) ) )

(expt 3 4)

(define (expt_ b n)
  (expt_iter b n 1) )

(define (expt_iter b n product)
  (if (= n 0)
    product
    (expt_iter b (- n 1) (* product b) )))

(expt_ 3 5)  

(define (even? n)
  (= (remainder n 2) 0))

(define (fast_expt b n) 
	(cond ( (= n 0)  1)
      ((even? n) (square (fast_expt b (/ n 2)) ))
      (else (* b (fast_expt b (- n 1)) ) )
  )
)

(fast_expt 3 5)


(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ( (= n 0) a)
		( (even? n) (fast-expt-iter (* b b) (/ n 2) a) )
    ( else (fast-expt-iter b  (- n 1)  (* a b)) )  
	)
)

(fast-expt 2 11)
(fast-expt 31 2)

(define (*_ a b)
  (*_iter a b 0))

(define (*_iter a b n)
  (cond 
	  ( ( = b 0) n)
    ( else (*_iter a (- b 1) (+ n a) ))
  )
)

(*_ 3 7)


(define (fast_multi a b)
	(fast_multi_iter a b 0))


(define (fast_multi_iter a b n)
  (cond 
    ( ( = a 0) n)
    ( ( = a 1) (+ n b) )
    ( ( > a b ) (fast_multi_iter ( - a b) b (+ (fast-expt b 2) n) ) )
    ( else (fast_multi_iter (- b a) a (+ (fast-expt a 2) n) ) )
  )
)




(fast_multi 31 23)



;Testing for primality
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ( (> (square test-divisor) n ) n)
    ( (divides? test-divisor n ) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 12)
(prime? 7)


;Fermat test version
(define (expmod base expnum m)
  (cond ((= expnum 0) 1)
        ((even? expnum)
         (remainder (square (expmod base (/ expnum 2) m)) m ))
        (else
         (remainder (* base (expmod base (- expnum 1) m)) m))))



(define (fermat-test-once n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test-once n) (fast-prime? n (- times 1)))
        (else false)))


(fast-prime? 193 10)


;Exec1.2-runtime
(runtime)
(newline)

(define (time-prime-test n)
	(newline)
  (display n)
  (start-prime-test n (runtime)))
  
(display (runtime))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
  ;(
    ;(report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(time-prime-test 10000009)


;(define (search-for-primes first last)
 ; (display n)
  ;(start-prime-test n    





;;Higher order procedures
(define (sum term a next b)
  (if (> a b)
      0
     (+ (term a)
        (sum term (next a) next b))))


(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define cube x (x * x * x ))

(sum-cubes 1 10)

(define (identity x) x)

(define (sums a b)
  (sum identity a inc b))

(sums 1 10)



(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.00)) add-dx b)
    dx))

(integral cube 0 1 0.01)


(define (integral_ f a b n)
  (define (next x) (+ x  (/ n (- b a)) ))
  (define (next2 x) (next (next x)))
  (* (/ (- b a) n)
  (+ (* 4 (sum f (next a) next2 b))
    (* 2 (sum f (next2 a) next2 b)))))

(integral_ cube 0 1.0 100.0)
 

(/ (- 1 0) 100.0)
 




