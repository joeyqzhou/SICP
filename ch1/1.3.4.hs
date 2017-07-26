import ch1_3

--create procedures whose returned values are procedires
average_damp :: (Fractional a) =>  (a->a) -> (a->a)
average_damp f = \x -> (x + f x) / 2.0

sqrt' :: (Fractional a) => a -> a
sqrt' x = fixed_point . average_damp (\y -> x/y) 1.0
