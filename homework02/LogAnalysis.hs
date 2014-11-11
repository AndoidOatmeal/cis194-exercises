-- CIS 194: Intro to Haskell, homework #2
-- http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where

import Log
import Text.Read


-- Simple assertion function for unit testing
assert :: ( Show a, Eq a ) => String -> a -> a -> IO ()
assert message given expected
    | ( given == expected ) = putStrLn ( "Test passed: " ++ message )
    | otherwise = fail ( "Test failed: " ++ message ++ ". Given: " ++ show given ++ " Expected: " ++ show expected )


-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage logString = case words logString of
    ( "I" : timeStamp : content ) -> LogMessage Info ( read timeStamp ) $unwords content
    ( "W" : timeStamp : content ) -> LogMessage Warning ( read timeStamp ) $unwords content
    ( "E" : errorLevel : timeStamp : content ) -> LogMessage ( Error ( read errorLevel ) ) ( read timeStamp ) $unwords content
    other -> Unknown logString

-- Unit testing
testAll :: IO ()
testAll = do
    -- Exercise 1
    assert "Parse simple info log" ( parseMessage "I 29 la la la" ) $LogMessage Info 29 "la la la"
    assert "Parse simple warning log" ( parseMessage "W 52 warning!" ) $LogMessage Warning 52 "warning!"
    assert "Parse simple error log" ( parseMessage "E 94 25 there is an error!" ) $LogMessage ( Error 94 ) 25 "there is an error!"
    assert "Parse simple invalid log" ( parseMessage "this is not a known log type" ) $Unknown "this is not a known log type"
    assert "Parse an malformed info log" ( parseMessage "I la la la" ) $Unknown "I la la la"


main = do
    testAll
    putStrLn "All tests passed!"
