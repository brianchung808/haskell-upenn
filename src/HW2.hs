module HW2 (parseMessage) where

import Log
import Data.String (words, unwords)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' l @ ("I":xs) = fromMaybe (Unknown $ unwords l) $ parseInfo xs
parseMessage' ("E":xs) = parseError xs
parseMessage' a @ _ = Unknown $ unwords a

-- I 147 mice in the air, I’m afraid, but you might catch a bat, and

parseInfo :: [String] -> Maybe LogMessage
parseInfo (t:xs) = (\x -> LogMessage Info x (unwords xs)) <$> readMaybe t
parseInfo _ = Nothing
-- parseInfo (t:xs) =  fmap (\x -> (LogMessage Info x (unwords xs))) $ readMaybe t

parseError :: [String] -> LogMessage
parseError (e:xs) = LogMessage (Error $ read e) (read t) (unwords xs')
  where (t:xs') = xs
parseError a @ _ = Unknown $ unwords a
