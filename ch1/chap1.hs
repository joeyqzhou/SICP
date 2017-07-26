import Text.Printf
import Debug.Trace

cube :: (Num a,Ord a) => a -> a
cube x = x * x * x 


{--function delcaration must have parenthese
sumf :: (Num a,Ord a) => (a->a) ->a -> (a->a) ->a -> a
sumf f x next y =
  if x > y
    then 0
    else (f x) + sumf f (next x) next y
--}
sum_cubes :: (Num a, Ord a) => a -> a -> a 
sum_cubes x y = sumf cube x (+1) y

sum_int :: (Num a, Ord a) => a -> a -> a
sum_int x y = sumf (+0) x (+1) y


integral :: (Num a, Ord a) => (a->a) -> a -> a -> a -> a
integral f x y dx = ( sumf f x (+dx) y ) * dx



--execirse 1.31
--mergefunction
--recursive version 
mergef :: (Num a,Ord a) => (a->a->a) ->a  -> (a->a) ->a -> (a->a) ->a -> a
mergef merge init f x next y =
  if x > y
    then init
    else merge (f x)  (mergef  merge init f (next x) next y)

--sumf :: (Num a,Ord a) => (a->a) ->a -> (a->a) ->a -> a
--sumf  = mergef (+) 0
--productf ::  (Num a,Ord a) => (a->a) ->a -> (a->a) ->a -> a
--productf = mergef (*) 1

-- (productf id 2 (+2) 100)*(productf id 4 (+2) 102) / (productf id 3 (+2) 101) /  (productf id 3 (+2) 101)  * 4
-- iterative version
mergef' :: (Num a,Ord a) =>  (a->a->a) ->a  -> (a->a) ->a -> (a->a) ->a -> a
mergef' merge value f x next y =
   if x > y
       then value
       else mergef' merge (merge value (f x)) f (next x) next y


sumf :: (Num a,Ord a) => (a->a) ->a -> (a->a) ->a -> a
sumf  = mergef' (+) 0
productf ::  (Num a,Ord a) => (a->a) ->a -> (a->a) ->a -> a
productf = mergef' (*) 1




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


--create procedures whose returned values are procedires
average_damp :: (Fractional a, Ord a,Show a) =>  (a->a) -> (a->a)
average_damp f = \x -> (x + f x) / 2.0

sqrt' :: (Fractional a, Ord a,Show a) => a -> a
sqrt' x = fixed_point  (average_damp (\y -> x/y)) 1.0

cube_root :: (Fractional a, Ord a,Show a) => a -> a
cube_root x = fixed_point  (average_damp (\y -> x/ ( y * y))) 1.0

--calculate derivate of any function
deriv ::(Fractional a, Ord a,Show a) => (a->a) -> (a->a)
deriv g = \x -> ( (g (x + dx) - g(x) ) / dx )
     where dx = 0.00001

newton_trans :: (Fractional a, Ord a,Show a) =>  (a->a) -> (a->a)
newton_trans g = \x -> (x - ( g x) /( (deriv g) x))

newton_method ::  (Fractional a, Ord a,Show a) => (a->a) -> a -> a
newton_method g guess = (fixed_point (newton_trans g) guess)

sqrt'' :: (Fractional a, Ord a,Show a) => a -> a
sqrt'' x = newton_method (\y -> (y*y-x)) 1.0

fixed_point_of_transform :: (Fractional a, Ord a,Show a) => (a->a) -> ( (a->a) -> (a->a)) ->a ->a
fixed_point_of_transform g trans guess = fixed_point (trans g) guess

sqrt''' :: (Fractional a, Ord a,Show a) => a -> a
sqrt''' x = fixed_point_of_transform (\y -> (y*y - x)) newton_method 1.0



square :: (Fractional a, Ord a,Show a,Integral b) => a->a
square x = x * x
--repeated square 2 10
repeated :: (Fractional a, Ord a,Show a,Integral b) => (a->a) -> b -> a -> a
repeated f n x
    | n == 0 = x
    | otherwise = repeated f (n-1) (f x)

--smooth cube 3
smooth :: (Fractional a, Ord a,Show a) => (a->a) -> a -> a
smooth f x = ( (f x) + (f (x - dx)) + (f (x + dx)) ) / 3.0
    where dx = 1e-5

 
