module HW2 (build, insert, inOrder, parseMessage, parse, whatWentWrong) where

import Log
import Data.String (words, unwords)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' l @ ("I":xs) = fromMaybe (Unknown $ unwords l) $ parseInfo xs
parseMessage' l @ ("W":xs) = fromMaybe (Unknown $ unwords l) $ parseWarning xs
parseMessage' l @ ("E":xs) = fromMaybe (Unknown $ unwords l) $ parseError xs
parseMessage' a @ _ = Unknown $ unwords a

parseInfo :: [String] -> Maybe LogMessage
parseInfo (t:xs) = createInfo <$> readMaybe t
  where createInfo x = LogMessage Info x (unwords xs)
parseInfo _ = Nothing

parseWarning :: [String] -> Maybe LogMessage
parseWarning (t:xs) = createWarning <$> readMaybe t
  where createWarning x = LogMessage Warning x (unwords xs)
parseWarning _ = Nothing

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe a b = a >>= (\a' -> (\b' -> (a', b')) <$> b)
-- lol
-- zipMaybe = (. flip ((<$>) . (,))) . (>>=)

parseError :: [String] -> Maybe LogMessage
parseError (e:xs) = createError <$> zipMaybe maybeError maybeTimestamp
  where (t:xs') = xs
        maybeError = readMaybe e :: Maybe Int
        maybeTimestamp = readMaybe t :: Maybe Int
        createError (a, b) = LogMessage (Error a) b (unwords xs')
parseError _ = Nothing

parse :: String -> [LogMessage]
parse = (parseMessage <$>) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert l Leaf = Node Leaf l Leaf
insert msg @ (LogMessage _ t1 _) (Node left node @ (LogMessage _ t2 _) right)
  | t1 >= t2 = Node left node $ insert msg right
  | t1 < t2 = Node (insert msg left) node right

build :: [LogMessage] -> MessageTree
build = foldl insert' Leaf
  where insert' = flip insert

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node m1 l m2) = inOrder m1 ++ [l] ++ inOrder m2

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = getMessage <$> (inOrder . build) errors
  where errors = filter isError xs

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s
getMessage (Unknown s) = s

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False
