-- Using recursion for fibonacci calculation
fibonacci :: (Num a, Ord a) => a -> a
fibonacci num
    | num <= 1 = num
    | otherwise = fibonacci (num - 1) + fibonacci (num - 2)

fastFib :: Integer -> Integer
fastFib num
    | num >= 0 = fst (fib num)

fib :: Integer -> (Integer, Integer)
fib 0 = (0, 1)
fib n =
    let (a, b) = fib (n `div` 2)
        c = a * (b * 2 - a)
        d = a * a + b * b
    in if n `mod` 2 == 0
        then (c, d)
        else (d, c + d)

-- Using recursion to find maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maximum' xs