-- CIS 194: Intro to Haskell, homework #1b
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

import System.Environment

-- Simple assertion function for unit testing
assert :: String -> Bool -> IO ()
assert message False = fail ( "Test failed: " ++ message )
assert message _ = putStrLn ( "Test passed: " ++ message )


-- Exercise 5
type Peg = String
type Move = ( Peg, Peg )

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 sourcePeg destinationPeg temporaryPeg = ( sourcePeg, destinationPeg ) : []
hanoi n sourcePeg destinationPeg temporaryPeg =
    ( hanoi ( n - 1 ) sourcePeg temporaryPeg destinationPeg ) -- move n − 1 discs from a to c using b as temporary storage
    ++ ( hanoi 1 sourcePeg destinationPeg temporaryPeg ) -- move the top disc from a to b
    ++ ( hanoi ( n - 1 ) temporaryPeg destinationPeg sourcePeg ) -- move n − 1 discs from c to b using a as temporary storage


-- Unit testing
testAll :: IO ()
testAll = do
    assert "hanoi solves for 1 pegs" $hanoi 1 "a" "b" "c" == [ ("a","b") ]
    assert "hanoi solves for 2 pegs" $hanoi 2 "a" "b" "c" == [ ("a","c"), ("a","b"), ("c","b") ]
    assert "hanoi solves for 3 pegs" $hanoi 3 "a" "b" "c" == [ ("a","b"), ("a","c"), ("b","c"), ("a", "b"), ("c","a"), ("c","b"), ("a","b") ]


main = do
    testAll
    putStrLn "All tests passed!"
