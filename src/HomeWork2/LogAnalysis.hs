{-# OPTIONS_GHC -Wall #-}
module HomeWork2.LogAnalysis (parseMessage, parse, build, inOrder) where 

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

-- inserts a new LogMessage into an existing MessageTree, producing a new MessageTree
insert :: LogMessage -> MessageTree -> MessageTree 
insert msg branch@(Node ln nodeMsg rn) = if(getMsgDate msg < getTreeDate branch) 
                                         then Node (insert msg ln) nodeMsg rn 
                                         else Node ln nodeMsg (insert msg rn)  
insert msg _                           = Node Leaf msg Leaf     


getMsgDate  :: LogMessage -> TimeStamp
getMsgDate (LogMessage _ dateMsgToAdd _) = dateMsgToAdd
getMsgDate _                             = 0

getTreeDate  :: MessageTree -> TimeStamp
getTreeDate (Node _ currentMsg _) = getMsgDate currentMsg
getTreeDate _                     = 0


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
