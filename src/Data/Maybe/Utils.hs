module Data.Maybe.Utils
  ( maybeToEither
  )
  where

maybeToEither :: e -> Maybe r -> Either e r
maybeToEither e Nothing  = Left e
maybeToEither _ (Just r) = Right r
