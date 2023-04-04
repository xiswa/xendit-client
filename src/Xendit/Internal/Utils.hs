{- | A utility module used for generating Aeson instances via TH
-}
module Xendit.Internal.Utils (
    camelToSnake
  , camelWithPrefToSnake
  , xenditOptions
  ) where

import Data.Aeson.TH (Options, defaultOptions, omitNothingFields)
import Data.Char (isUpper, toLower)

camelToSnake :: String -> String
camelToSnake = foldr op [] 
  where
    op c z 
      | isUpper c = '_':toLower c:z
      | otherwise = c:z

removePrefix :: String -> String -> String
removePrefix pref = drop (length pref)

camelWithPrefToSnake :: String -> String -> String
camelWithPrefToSnake pref = tail . camelToSnake . removePrefix pref

-- | VERY IMPORTANT to set this
-- because Xendit does not accept field with nulls
xenditOptions :: Options
xenditOptions = defaultOptions { omitNothingFields = True }
