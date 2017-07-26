data Rational' = Rational' {numer :: Int,
                          denom :: Int}  deriving Show

add_rat :: Rational' -> Rational' -> Rational'
add_rat x y = Rational' (numers `quot` g) ( denoms `quot` g)
    where
    numers = ( (numer x) * (denom y) + (denom x) * (numer x) )  
    denoms = ( (denom x) * (denom y) )
    g = gcd numers denoms


sub_rat :: Rational' -> Rational' -> Rational'
sub_rat x y = Rational'  (numers `quot` g) ( denoms `quot` g)
    where
    numers = ( (numer x) * (denom y) - (denom x) * (numer x) )  
    denoms = ( (denom x) * (denom y) )
    g = gcd numers denoms

mul_rat :: Rational' -> Rational' -> Rational'
mul_rat x y = Rational' (numers `quot` g) ( denoms `quot` g)
    where
    numers = (numer x) * (numer y)
    denoms = ( (denom x) * (denom y) )
    g = gcd numers denoms


div_rat :: Rational' -> Rational' -> Rational'
div_rat x y =  Rational' (numers `quot` g) ( denoms `quot` g) 
    where
    numers =  (numer x) * (denom y) 
    denoms =  (denom x) * (numer y)
    g = gcd numers denoms


---Lis
car :: [a] -> Maybe a
car (x:xs) = Just x
car [] = Nothing

cdr :: [a] -> Maybe [a]
cdr (x:xs) = Just xs
cdr [] = Nothing

list_ref :: [a] -> Int -> a
list_ref (x:xs) n 
    | n==0 = x
    | otherwise = list_ref xs (n-1)

list_len :: [a] -> Int
list_len [] = 0
list_len (x:xs) = 1 + (list_len xs)

last_pair :: [a] -> Maybe a
last_pair [] = Nothing
last_pair (x:[]) = Just x
last_pair (x:xs) = last_pair xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


--2.34
--reverse version
horner_rulu :: (Num a) => [a] -> a -> a
horner_rulu [] _ = 0
--horner_rulu (x:[]) y = x 
--horner_rulu (x:z:xs) y = horner_rulu ((x*y+z):xs) y










