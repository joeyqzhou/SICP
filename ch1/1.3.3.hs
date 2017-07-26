module ch1_three where

import Text.Printf
import Debug.Trace


--use Debug.Trace.trace for debuging.
--trace :: String -> a ->a
--trace a b; print a, return b
fixed_point :: (Fractional a, Ord a,Show a ) => (a->a) -> a ->a
fixed_point f guess = try guess 
    where 
    --try :: (Num a) => a -> a
    try guess = trace (show guess)  (if abs (guess - f guess) < 1.0e-4 
                    then do guess
                    else do
                        try  (f guess)
										)


search :: (Fractional a,Ord a) => (a->a) -> a -> a -> a
search f neg pos 
    | abs ( neg - pos) < 1e-4 = (neg + pos)/2
    | f (neg+pos)/2 > 0 = search f neg ((neg+pos)/2)
    | f (neg+pos)/2 < 0 = search f ((neg+pos)/2) pos


half_interval_method :: (Fractional a, Ord a) => (a->a) -> a -> a -> a
half_interval_method f x y  
		 | (f x) > 0 && (f y)<0 = search f x y
     | (f x) < 0 && (f y)>0 = search f y x
     | otherwise = 0.0

--1.35
--fixed_point (\x -> (1 + 1.0/x)) 0.0 

--1.37
--iter version
cont_frac :: (Fractional a, Integral b,Show a,Show b) => a -> a ->b -> a
cont_frac x y z = frac_from x y z 1 
    where
    frac_from d n k val
        | k<=0 = trace (show "a") val
        -- | otherwise = (frac_from d n (k-1) d/(n+val) )
        | otherwise = trace (show k ++ " " ++ show val ++ " " ++ show d ++ " " ++ (show n) ++ " " ++ show  (d/(n+val))  ) (frac_from d n (k-1) (d/(n+val)) )

--recursive version
cont_frac' :: (Fractional a, Integral b,Show a,Show b) => a -> a ->b -> a
cont_frac' x y z  
    | z==0 = 1
    | otherwise = x/(y + (cont_frac' x y (z-1)))

--1.38 not right
-- ??
--cont_frac'' (\x ->1.0) tmp 10000
tmp :: (Integral b, Fractional a) => b->a
tmp i = if ((mod i 3)==1) then (realToFrac ((quot i 3)+1))*2.0 else 1.0

t i = if ((mod i 3)==2) then (realToFrac (i+1)/1.5) else 1.0

cont_frac'' :: (Fractional a, Integral b,Show a,Show b) => (b -> a) -> (b->a)  ->b -> a
cont_frac'' f g z
    | z == 0 = 1
    | otherwise = (f z)/((g z) + (cont_frac'' f g (z-1)))

--- \i -> if ((mod i 3)==1) then ((quot i 3)+1)*2 else 1

