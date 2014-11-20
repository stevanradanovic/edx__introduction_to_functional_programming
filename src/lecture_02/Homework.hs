-- Exercise 5
second xs = head (tail xs)

-- Exercise 6
swap (x, y) = (y, x)

-- Exercise 7
pair x y = (x, y)

-- Exercise 8
double x = x * 2

-- Exercise 9
palindrome xs = reverse xs == xs

-- Exercuse 10
twice f x = f (f x)

-- Exercise 11
same x = x
same_old x = x + 1 - 1

-- Exercise 18
f xs = take 3 (reverse xs)
