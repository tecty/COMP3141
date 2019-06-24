import Test.QuickCheck

divides:: Integer -> Integer -> Bool
m `divides` n  = (n `mod` m == 0)

isPrime:: Integer -> Bool
isPrime n 
    | n <= 1  = False 
    | n == 2  = True
    | otherwise = not $ any (`divides` n ) [2 .. (n-1)]

propMersenne :: Integer -> Property
propMersenne n = (isPrime n ==> isPrime(2^n -1))