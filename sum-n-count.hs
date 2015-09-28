sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x > 0 = (sum 0 x, col 0 x)
			  | x < 0 = (sum 0 (-x), col 0 (-x))
			  | x == 0 = (0, 1)
	where
		col acc 0 = toInteger acc
		col acc n = col (acc + 1) (quot n 10)
		sum count 0 = toInteger count
		sum count i = sum (count + mod i 10) (quot i 10)