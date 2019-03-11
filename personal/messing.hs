qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where
        ys = [a | a <- xs, a<= x]
        zs = [b | b <- xs, b > x]

factorial n = product [1..n]

average ns = sum ns `div` length ns