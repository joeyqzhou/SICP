import Text.Printf
import Debug.Trace


--use Debug.Trace.trace for debuging.
--trace :: String -> a ->a
--trace a b; print a, return b
fixed_point :: (Floating a, Ord a,Show a ) => (a->a) -> a ->a
fixed_point f guess = try guess 
    where 
    --try :: (Num a) => a -> a
    try guess = trace (show guess)  (if abs (guess - f guess) < 1.0e-4 
                    then do guess
                    else do
                        try  (f guess)
										)


search :: (Floating a,Ord a) => (a->a) -> a -> a -> a
search f neg pos 
    | abs ( neg - pos) < 1e-4 = (neg + pos)/2
    | f (neg+pos)/2 > 0 = search f neg ((neg+pos)/2)
    | f (neg+pos)/2 < 0 = search f ((neg+pos)/2) pos


half_interval_method :: (Floating a, Ord a) => (a->a) -> a -> a -> a
half_interval_method f x y  
		 | (f x) > 0 && (f y)<0 = search f x y
     | (f x) < 0 && (f y)>0 = search f y x
     | otherwise = 0.0

    

--1.35
--fixed_point (\x -> (1 + 1.0/x)) 0.0 
