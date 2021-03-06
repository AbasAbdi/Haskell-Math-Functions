--Brief outline

{-  This module contains four functions:
	1) The first return a list of all prime numbers up to a given limit. 
	2) The second encrypts text using Caesar’s cipher and returns the encrypted text. 
	3) The third implements the factorial function and returns the factorial of a given number.
	4) The fourth merges two sorted lists and returns one sorted list -}
module A1_21422705
(primes_21422705,
caesarcipher_21422705,
factorial_21422705,
merge_21422705
) where

-- Function 1

{- Defining a function which takes 
	an Integer and returns a list of Integers. -}
factors :: Int -> [Int]

{- It uses a generator to set the values for y using a list of ordered Integers
	starting with 1 and ending with the given Integer x. Then it takes the mod
	of x & y and if it equals zero it returns the value of y. -}
	
factors x = [y | y <- [1..x], x `mod` y == 0]

{- Defining a function which takes 
	an Integer and returns a Boolean. -}
prime :: Int -> Bool

{- It uses the factors function to return a value using the given Integer x. 
	Then it checks for equality between what's returned with a list containing 
	the number 1 and the given Integer x. If there is equality prime returns True,
	otherwise False. -}
prime x = factors x == [1,x]

{- Defining the function which takes 
	an Integer and returns a list of Integers. -}
primes_21422705 :: Int -> [Int]

{- It uses a generator to set the values for y using a list of ordered Integers
	starting with 2 and ending with the given Integer value x. Then it uses the 
	prime function to return a Boolean using the value of y. If the value of the
	Boolean is True then primes_21422705 returns the value of y. -}
primes_21422705 x = [y | y <- [2..x], prime y]

-- Function 2

{-- Defined a datatype that is a list of Characters. --}
ts :: [Char]
ts = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 "

{- Defining a function CharZIP which takes an Integer and returns a list of tuples which contain Characters -}
charZIP_21422705 :: Int -> [(Char, Char)]
charZIP_21422705 n = zip ts (drop n ts ++ take n ts)

{- Defining a function which takes a list of Characters 
	and an Integer then returns a list of Characters. -}
caesarcipher_21422705 :: [Char] -> Int -> [Char]
caesarcipher_21422705 cs n = [z| i <- cs, (a,z) <- charZIP_21422705 n, i==a]

-- Function 3

{- Defining a recursive function which takes 
	an Integer and returns an Integer. -}
factorial_21422705 :: Int -> Int

{- This is our base case. The factorial of zero 
	is assigned the value of 1. -}
factorial_21422705 0 = 1

{- This part is the recursive case. It takes the given integer value x and 
	multiplies it by the factorial of the integer value x minus 1. -}
factorial_21422705 x = x * factorial_21422705 (x-1)

-- Function 4

{- Defining a recursive function which takes 
	two ordered lists and returns an ordered list. -}
merge_21422705 :: Ord a => [a] -> [a] -> [a]

{- This is our base case. If an empty list is merged with a non-empty list 
	the output is the non-empty list. -}
merge_21422705 [] x = x
merge_21422705 x [] = x

{- The first element of the first list is compared with the first element of 
	the second list and the one that is lower is placed in the new list. -}
merge_21422705 (x:xs) (y:ys) | x > y     = y : merge_21422705 (x:xs) ys
merge_21422705 (x:xs) (y:ys) | otherwise = x : merge_21422705 xs (y:ys)