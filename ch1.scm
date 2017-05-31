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


