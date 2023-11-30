factorial :: Integer -> Integer
factorial n = product [1..n]

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

big :: Integer
big = factorial 50