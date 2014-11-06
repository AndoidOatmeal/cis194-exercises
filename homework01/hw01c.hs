-- CIS 194: Intro to Haskell, homework #1b
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

import System.Environment

-- Simple assertion function for unit testing
assert :: ( Show a, Eq a ) => String -> a -> a -> IO ()
assert message given expected
    | ( given == expected ) = putStrLn ( "Test passed: " ++ message )
    | ( given /= expected ) = fail ( "Test failed: " ++ message ++ ". Given: " ++ show given ++ " Expected: " ++ show expected )


-- Exercise 6
type Peg = String
type Move = ( Peg, Peg )

hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi 0 sourcePeg destinationPeg tempPeg1 tempPeg2 = []
hanoi 1 sourcePeg destinationPeg tempPeg1 tempPeg2 = ( sourcePeg, destinationPeg ) : []
hanoi n sourcePeg destinationPeg tempPeg1 tempPeg2 =
    ( hanoi ( n - 1 ) sourcePeg tempPeg2 destinationPeg tempPeg1 )
    ++ ( hanoi ( n - 2 ) sourcePeg tempPeg1 destinationPeg tempPeg2 )
    ++ ( hanoi 1 sourcePeg destinationPeg tempPeg1 tempPeg2 )
    ++ ( hanoi ( n - 2 ) tempPeg1 destinationPeg sourcePeg tempPeg2 )
    ++ ( hanoi ( n - 1 ) tempPeg2 destinationPeg sourcePeg tempPeg1 )

-- Unit testing
testAll :: IO ()
testAll = do
    assert "hanoi solves for 1 disks" (hanoi 1 "a" "b" "c" "d" ) [ ("a","b") ]
    assert "hanoi solves for 2 disks" (hanoi 2 "a" "b" "c" "d" ) [ ("a","d"), ("a","b"), ("d","b") ]
    assert "hanoi solves for 3 disks" (hanoi 3 "a" "b" "c" "d" ) [ ("a","d"), ("a","c"), ("a","b"), ("c","b"), ("d","b") ]
    assert "hanoi solves for 4 disks" (hanoi 4 "a" "b" "c" "d" ) [ ("a","d"), ("a","c"), ("a","b"), ("c","b"), ("d","b") ]


main = do
    testAll
    putStrLn "All tests passed!"
