-- CIS 194: Intro to Haskell, homework #2
-- http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where

import Log


-- Exercise 1
parseMessageContents :: String -> LogMessage

parseMessage :: String -> LogMessage
parseMessage logString = case logString of
    ( 'I ':s ) -> LogMessage Info (-1) "stub"

-- Simple assertion function for unit testing
assert :: ( Show a, Eq a ) => String -> a -> a -> IO ()
assert message given expected
    | ( given == expected ) = putStrLn ( "Test passed: " ++ message )
    | otherwise = fail ( "Test failed: " ++ message ++ ". Given: " ++ show given ++ " Expected: " ++ show expected )


-- Unit testing
testAll :: IO ()
testAll = do
    -- Exercise 1
    assert "Parse simple info log" ( parseMessage "I 29 la la la" ) $LogMessage Info 29 "la la la"


main = do
    testAll
    putStrLn "All tests passed!"
