module Taskell.Error where

import RIO

newtype Error =
    Error Text
    deriving (Show, Eq)

type EitherError a = Either Error a

txt :: Text -> Error
txt = Error

e :: Text -> EitherError a
e text = Left (txt text)

mEither :: Text -> Maybe a -> EitherError a
mEither text = maybe (e text) pure
