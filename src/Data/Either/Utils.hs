module Data.Either.Utils
  ( eitherToMaybe
  )
  where

eitherToMaybe :: Either e r -> Maybe r
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right e) = Just e
