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


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert ( Unknown _ ) t = t
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog@( LogMessage _ newTimeStamp _ ) ( Node leftTree nodeLog@( LogMessage _ nodeTimeStamp _ ) rightTree )
    | ( nodeTimeStamp < newTimeStamp ) = Node leftTree nodeLog ( insert newLog rightTree )
    | ( nodeTimeStamp > newTimeStamp ) = Node ( insert newLog leftTree ) nodeLog rightTree


-- Exercise 3
build :: [ LogMessage ] -> MessageTree
build [] = Leaf
build ( h : t ) = insert h ( build t )


-- Exercise 4
inOrder :: MessageTree -> [ LogMessage ]
inOrder Leaf = []
inOrder ( Node leftTree log rightTree ) = ( inOrder leftTree ) ++ [ log ] ++ ( inOrder rightTree )


-- Exercise 5
filterErrors :: [ LogMessage ] -> [ LogMessage ]
filterErrors [] = []
filterErrors ( h@( LogMessage ( Error severity ) _ _ ) : t )
    | ( severity >= 50 ) = h : ( filterErrors t )
    | otherwise = filterErrors t
filterErrors ( h : t ) = filterErrors t

messageOfLog :: LogMessage -> String
messageOfLog ( LogMessage _ _ content ) = content
messageOfLog ( Unknown content ) = content

whatWentWrong :: [ LogMessage ] -> [ String ]
whatWentWrong logs = map messageOfLog $ inOrder $ build $ filterErrors logs


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

    -- Exercise 2
    let infoLog34 = LogMessage Info 34 "info log"
    let oneNodeTree = Node Leaf infoLog34 Leaf
    assert "Insert an info log message into an empty tree" ( insert infoLog34 Leaf ) oneNodeTree

    let unknownLog = Unknown "unknown log type"
    assert "Unknown log shouldn't insert into tree" ( insert unknownLog oneNodeTree ) oneNodeTree

    let infoLog88 = LogMessage Info 88 "another info log"
    let twoNodeTreeRight = Node Leaf infoLog34 ( Node Leaf infoLog88 Leaf )
    assert "Insert an info log message right into an single node tree" ( insert infoLog88 oneNodeTree ) twoNodeTreeRight

    let warningLog4 = LogMessage Warning 4 "warning log"
    let twoNodeTreeLeft = Node ( Node Leaf warningLog4 Leaf ) infoLog34 Leaf
    assert "Insert an info log message left into an single node tree" ( insert warningLog4 oneNodeTree ) twoNodeTreeLeft

    -- Exercise 3
    assert "Build a one log list into a tree" ( build [ infoLog34 ] ) oneNodeTree
    assert "Build an empty tree from no logs" ( build [] ) Leaf
    let threeNodeTree = Node ( Node Leaf warningLog4 Leaf ) infoLog34 ( Node Leaf infoLog88 Leaf )
    assert "Build a tree from various logs"
        ( build [ warningLog4, infoLog88, infoLog34 ] )
        threeNodeTree

    -- Exercise 4
    let errorLog150 = LogMessage ( Error 25 ) 150 "super error!"
    let oneNodeErrorTree = Node Leaf errorLog150 Leaf
    assert "Order a one-node message tree" ( inOrder oneNodeErrorTree ) [ errorLog150 ]
    assert "Order an empty message tree" ( inOrder Leaf ) []
    assert "Order a three node tree" ( inOrder threeNodeTree ) [ warningLog4, infoLog34, infoLog88 ]
    let unknownLog1 = Unknown "some unknown"
    let unknownLog2 = Unknown "another unknown"
    assert "Sort logs and remove unknowns"
        ( inOrder ( build [ errorLog150, unknownLog1, infoLog34, unknownLog2, infoLog88, warningLog4 ] ) )
        [ warningLog4, infoLog34, infoLog88, errorLog150 ]

    -- Exercise 5
    ( testWhatWentWrong parse whatWentWrong "error.log" ) >>= \ s ->
        assert "Sort error logs with severity greater than 50" s
            [
                "Mustardwatch opened, please close for proper functioning!",
                "All backup mustardwatches are busy",
                "Depletion of mustard stores detected!",
                "Hard drive failure: insufficient mustard",
                "All backup mustardwatches are busy",
                "Twenty seconds remaining until out-of-mustard condition",
                "Ten seconds remaining until out-of-mustard condition",
                "Empty mustard reservoir! Attempting to recover...",
                "Recovery failed! Initiating shutdown sequence"
            ]

main = do
    testAll
    putStrLn "All tests passed!"
