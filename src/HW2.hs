module HW2 where

import Log
import Data.List
import Data.String

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":xs) = parseInfo xs
parseMessage' ("E":xs) = parseError xs
parseMessage' a @ _ = Unknown $ unwords a

-- I 147 mice in the air, I’m afraid, but you might catch a bat, and

parseInfo :: [String] -> LogMessage
parseInfo (t:xs) = LogMessage Info (read t) (unwords xs)
parseInfo a @ _ = Unknown $ unwords a

parseError :: [String] -> LogMessage
parseError (e:xs) = LogMessage (Error $ read e) (read t) (unwords xs')
  where (t:xs') = xs
parseError a @ _ = Unknown $ unwords a
