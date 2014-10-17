-- Exercise 1
n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

-- Exercise 2
last_1 xs = drop (length xs - 1) xs
last_2 xs = head (drop (length xs - 1) xs)
last_3 xs = tail (reverse xs)
last_4 xs = reverse (head xs)
last_5 xs = xs !! (length xs - 1)
last_6 xs = head (drop (length xs) xs)
last_7 xs = head (reverse xs)
last_8 xs = reverse xs !! (length xs - 1)

-- Exercise 3
init_1 xs = tail (reverse xs)
init_2 xs = reverse (head (reverse xs))
init_3 xs = reverse (tail xs)
init_4 xs = take (length xs) xs
init_5 xs = reverse (tail (reverse xs))
init_6 xs = take (length xs - 1) (tail xs)
init_7 xs = drop (length xs - 1) xs

-- Exercise 6
product_1 [] = 1
--product_1 [x, xs] = x * product_1 xs

product_2 [] = 0
product_2 (x : xs) = x * product_2 xs

product_3 [] = 1
product_3 (x : (y : (z : xs))) = x * y * z * product_3 xs

product_4 [] = 1
product_4 (x : xs) = x * product_4 xs

-- Exercise 7
qsort_1 [] = []
qsort_1 (x : xs) = qsort_1 larger ++ [x] ++ qsort_1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort_2 [] = []
qsort_2 (x : xs) = reverse (qsort_2 smaller ++ [x] ++ qsort_2 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort_3 [] = []
qsort_3 xs = qsort_3 larger ++ qsort_3 smaller ++ [x]
  where x = minimum xs
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort_4 [] = []
qsort_4 (x : xs)
  = reverse (qsort_4 smaller) ++ [x] ++ reverse (qsort_4 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort_5 [] = []
qsort_5 (x : xs) = qsort_5 larger ++ [x] ++ qsort_5 smaller
  where larger = [a | a <- xs, a > x || a == x]
        smaller = [b | b <- xs, b < x]

qsort_6 [] = []
qsort_6 (x : xs) = qsort_6 larger ++ [x] ++ qsort_6 smaller
  where smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]

qsort_7 [] = []
qsort_7 (x : xs)
  = reverse
      (reverse (qsort_7 smaller) ++ [x] ++ reverse (qsort_7 larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort_8 [] = []
qsort_8 xs = x : qsort_8 larger ++ qsort_8 smaller
  where x = maximum xs
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x]
