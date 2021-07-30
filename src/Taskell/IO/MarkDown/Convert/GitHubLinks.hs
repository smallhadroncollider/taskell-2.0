{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.GitHubLinks
    ( generateLinks
    ) where

import RIO
import qualified RIO.Char as C
import qualified RIO.Seq as Seq
import qualified RIO.Text as T

import qualified Taskell.Data.Types.Task as Task

-- the rules?
-- 1) strip
-- 2) lower-case everything
-- 3) space becomes - (not merged)
-- 4) non word characters are ignored
-- 5) duplicates get -# added on end (where # is an integer, starting at 1)
--
replace :: Char -> Char
replace ' ' = '-'
replace x = x

isWordChar :: Char -> Bool
isWordChar chr = C.isAsciiLower chr || C.isDigit chr || chr == '-' || chr == '_'

format :: Text -> Text
format = T.concat . T.split (not . isWordChar) . T.map replace . T.toLower . T.strip

-- duplicates
postfix :: Int -> Text -> Text
postfix 0 url = url
postfix i url = url <> "-" <> tshow i

enumerate ::
       (Seq Text, Seq (Task.TaskID, Text))
    -> (Task.TaskID, Text)
    -> (Seq Text, Seq (Task.TaskID, Text))
enumerate (prev, acc) (tID, url) = (prev Seq.|> url, acc Seq.|> (tID, postfix count url))
  where
    count = length . Seq.filter (== url) $ prev

dedup :: Seq (Task.TaskID, Text) -> Seq (Task.TaskID, Text)
dedup = snd . foldl' enumerate ([], [])

-- generate
generateLinks :: Seq (Task.TaskID, Text) -> Seq (Task.TaskID, Text)
generateLinks s = dedup $ second format <$> s
