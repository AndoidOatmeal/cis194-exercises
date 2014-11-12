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
    | otherwise = fail ( "Test failed: " ++ message ++ "\n\nGiven:    " ++ show given ++ "\n\nExpected: " ++ show expected )


-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage logString = case words logString of
    ( "I" : timeStamp : content ) -> LogMessage Info ( read timeStamp ) $unwords content
    ( "W" : timeStamp : content ) -> LogMessage Warning ( read timeStamp ) $unwords content
    ( "E" : errorLevel : timeStamp : content ) -> LogMessage ( Error ( read errorLevel ) ) ( read timeStamp ) $unwords content
    other -> Unknown logString

parse :: String -> [ LogMessage ]
parse s = map parseMessage ( lines s )

-- Unit testing
testAll :: IO ()
testAll = do
    -- Exercise 1
    assert "Parse simple info log" ( parseMessage "I 29 la la la" ) $LogMessage Info 29 "la la la"
    assert "Parse simple warning log" ( parseMessage "W 52 warning!" ) $LogMessage Warning 52 "warning!"
    assert "Parse simple error log" ( parseMessage "E 94 25 there is an error!" ) $LogMessage ( Error 94 ) 25 "there is an error!"
    assert "Parse simple invalid log" ( parseMessage "this is not a known log type" ) $Unknown "this is not a known log type"
    ( testParse parse 10 "error.log" )  >>= \ l -> assert "Parse 10 lines of error.log should yield 10 log messages" ( length l ) 10
    ( testParse parse 10 "error.log" )  >>= \ l -> assert "Parse first 10 lines of error.log" l [
        ( LogMessage Info 5053 "pci_id: con ing!" ),
        ( LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)" ),
        ( LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled" ),
        ( LogMessage Info 4076 "verse.'" ),
        ( LogMessage Info 4764 "He trusts to you to set them free," ),
        ( LogMessage Info 858 "your pocket?' he went on, turning to Alice." ),
        ( LogMessage Info 898 "would be offended again." ),
        ( LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)" ),
        ( LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And" ),
        ( LogMessage Info 3899 "hastily." ) ]


main = do
    testAll
    putStrLn "All tests passed!"
