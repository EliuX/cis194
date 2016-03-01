{-# OPTIONS_GHC -Wall #-}
module HomeWork2.LogAnalysis (parseMessage, parse) where 

import  HomeWork2.Log

parseMessage :: String -> LogMessage
parseMessage x = parseMessageTokens (words x)       

parseMessageTokens :: [String] -> LogMessage
parseMessageTokens ("E":level:timestamp:messageTokens) = LogMessage (Error (toInt level)) (toInt timestamp) (unwords messageTokens)
parseMessageTokens ("I":timestamp:messageTokens) = LogMessage Info (toInt timestamp) (unwords messageTokens)
parseMessageTokens nonvalidTokens = Unknown (unwords nonvalidTokens)


parse :: String -> [LogMessage] 
parse text = [parseMessage line | line <- lines text ]


-- converts a Numeric String to an Int
toInt :: String -> Int
toInt x = read x :: Int

--- Functions for Ordering

-- inserts a new LogMessage into an existing MessageTree, producing a new MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree = case tree of
                   Leaf -> Node Leaf msg Leaf
                   _    -> insert msg (whereMsgGoes msg tree)

-- Returns the node where a LogMessage should go : left or right?---
whereMsgGoes :: LogMessage -> MessageTree -> MessageTree
whereMsgGoes msg tree           
      | dateMsgToAdd < dateCurrentMsg = leftNode
      | otherwise = rightNode
      where (LogMessage _ dateMsgToAdd _) = msg
            (Node leftNode currentMsg rightNode ) = tree
            (LogMessage _ dateCurrentMsg _) = currentMsg

--builds a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build (msg:[])   = insert msg Leaf
build ([])       = Leaf
build (msg:rest) = insert msg (build rest) 

-- takes a sorted MessageTree and produces a list of all the LogMessages it contain
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
            Leaf -> []
            _    -> (inOrder leftNode) ++ currentMsg:(inOrder rightNode)            
            where (Node leftNode currentMsg rightNode ) = tree
