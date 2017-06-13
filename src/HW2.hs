module HW2 (parseMessage) where

import Log
import Data.String (words, unwords)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' l @ ("I":xs) = fromMaybe (Unknown $ unwords l) $ parseInfo xs
parseMessage' l @ ("E":xs) = fromMaybe (Unknown $ unwords l) $ parseError xs
parseMessage' a @ _ = Unknown $ unwords a

parseInfo :: [String] -> Maybe LogMessage
parseInfo (t:xs) = (\x -> LogMessage Info x (unwords xs)) <$> readMaybe t
parseInfo _ = Nothing
-- parseInfo (t:xs) =  fmap (\x -> (LogMessage Info x (unwords xs))) $ readMaybe t

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe a b = a >>= (\a' -> (\b' -> (a', b')) <$> b)

parseError :: [String] -> Maybe LogMessage
parseError (e:xs) = (\(a, b) -> LogMessage (Error a) b (unwords xs')) <$> zipMaybe (readMaybe e :: Maybe Int) (readMaybe t :: Maybe Int)
  where (t:xs') = xs
parseError _ = Nothing
