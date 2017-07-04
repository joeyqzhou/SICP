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

