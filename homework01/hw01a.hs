-- CIS 194: Intro to Haskell, homework #1a
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

import System.Environment

-- Simple assertion function for unit testing
assert :: String -> Bool -> IO ()
assert message False = fail ( "Test failed: " ++ message )
assert message _ = putStrLn ( "Test passed: " ++ message )


-- Exercise 1
toDigits :: Integer -> [ Integer ]
toDigits n = reverseList ( toDigitsRev n )

reverseList :: [ Integer ] -> [ Integer ]
reverseList (x:xs) = reverseList xs ++ [x]
reverseList as = as

toDigitsRev :: Integer -> [ Integer ]
toDigitsRev n
    | ( n <= 0 ) = []
    | otherwise = ( n `mod` 10 ) : toDigitsRev ( n `div` 10 )


-- Exercise 2
listLength :: [Integer] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (h:[]) = [h]
doubleEveryOther ( h:a:t )
    -- If has even length, double head, process the rest
    | ( listLength ( h:a:t ) ) `mod` 2 == 0 = ( h * 2 ) : a : doubleEveryOther t
    -- Else if has odd length, don't double head and process the rest (even length portion)
    | otherwise = h : doubleEveryOther ( a : t )


-- Exercise 3
sumList :: [Integer] -> Integer
sumList [] = 0
sumList ( h:t ) = h + sumList t

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits ( h:t ) = ( sumList ( toDigits h ) ) + ( sumDigits t )


-- Exercise 4
validateHash :: Integer -> Integer
validateHash x = sumDigits ( doubleEveryOther ( toDigits x ) )

validate :: Integer -> Bool
validate x = ( ( validateHash x ) `mod` 10 ) == 0


-- Unit testing
testAll :: IO ()
testAll = do
    -- Exercise 1
    assert "toDigits works on multi-digit integers" $toDigits 1234 == [1,2,3,4]
    assert "toDigitsRev works on multi-digit integers" $toDigitsRev 1234 == [4,3,2,1]
    assert "toDigits gives empty list on 0" $toDigits 0 == []
    assert "toDigits gives empty list on negative" $toDigits ( -17 ) == []

    -- Exercise 2
    assert "listLength works on empty lists" $listLength [] == 0
    assert "listLength works on single element lists" $listLength [8] == 1
    assert "listLength works on large lists" $listLength [12, 13, 0, 1, 1, 3] == 6
    assert "doubleEveryOther works empty lists" $doubleEveryOther [] == []
    assert "doubleEveryOther works single element lists" $doubleEveryOther [6] == [6]
    assert "doubleEveryOther works two element lists" $doubleEveryOther [10, 5] == [20, 5]
    assert "doubleEveryOther works on even lengthed lists" $doubleEveryOther [8,7,6,5] == [16,7,12,5]
    assert "doubleEveryOther works on odd lengthed lists" $doubleEveryOther [1,2,3] == [1,4,3]

    -- Exercise 3
    assert "sumDigits works with empty list" $sumDigits [] == 0
    assert "sumDigits works with single element, single digit list" $sumDigits [5] == 5
    assert "sumDigits works with single element, multi-digit list" $sumDigits [123] == 6
    assert "sumDigits works with large list, single digit" $sumDigits [5, 6, 8, 9, 1, 0, 9] == 38
    assert "sumDigits works with large list, multi digit" $sumDigits [50, 16, 10, 0, 31, 123, 9] == 32

    -- Exercise 4
    assert "validate works with valid short CC" $validate 1010101010 == True
    assert "validate works with invalid short CC" $validate 9 == False
    assert "validate works with valid CC" $validate 4012888888881881 == True
    assert "validate fails with invalid CC" $validate 4012888888881882 == False


main = do
    testAll
    putStrLn "All tests passed!"
