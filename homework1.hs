-- Last digit in a number (ie. the ones digit)
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Remaining digits after last is removed
remainingDigits :: Integer -> Integer
remainingDigits n = n `div` 10

-- Turn an integer int a list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (lastDigit n) : toDigitsRev (remainingDigits n)

-- Reverses list, not neeeded
rev :: [Integer] -> [Integer]
rev [] = []
rev (x:[]) = [x]
rev (x:xs) = (rev xs) ++ [x]

-- toDigits in order, not needed
toDigits :: Integer -> [Integer]
toDigits n = (rev (toDigitsRev n))

-- Double every other number starting with the second one
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (y * 2) : (doubleEveryOther xs)

-- Sum all digits... eg. (sumDigit 16) == 7
sumDigit :: Integer -> Integer
sumDigit x = (x `div` 10) + (x `mod` 10) 

-- sumDigit applied to all elements in list and summed up
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = (sumDigit x) 
sumDigits (x:xs) = (sumDigit x) + (sumDigits xs)

-- validate if a valid credit card number is given
validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigitsRev n))) `mod` 10) == 0

main = do
    print (toDigitsRev 1234567)
    print (toDigits 1234567)
    print (toDigits 0)
    print (toDigits (-17))
    print (doubleEveryOther (toDigitsRev 1234))
    print (sumDigits [16,7,12,5])
    print (validate 4012888888881881)
    print (validate 4012888888881882)
