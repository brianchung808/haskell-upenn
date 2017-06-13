module HW2 (parseMessage, parse) where

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
parseInfo (t:xs) = (\x -> LogMessage Info x (unwords xs)) <$> readMaybe t
parseInfo _ = Nothing
-- parseInfo (t:xs) =  fmap (\x -> (LogMessage Info x (unwords xs))) $ readMaybe t

parseWarning :: [String] -> Maybe LogMessage
parseWarning (t:xs) = (\x -> LogMessage Warning x (unwords xs)) <$> readMaybe t
parseWarning _ = Nothing

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe a b = a >>= (\a' -> (\b' -> (a', b')) <$> b)

parseError :: [String] -> Maybe LogMessage
parseError (e:xs) =  toError <$> zipMaybe maybeError maybeTimestamp
  where (t:xs') = xs
        maybeError = readMaybe e :: Maybe Int
        maybeTimestamp = readMaybe t :: Maybe Int
        toError (a, b) = LogMessage (Error a) b (unwords xs')
parseError _ = Nothing

parse :: String -> [LogMessage]
parse xs = parseMessage <$> lines xs
