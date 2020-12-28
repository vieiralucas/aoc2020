module Day04 where

import Prelude
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Data.Maybe (Maybe(..))
import Data.Array (catMaybes, concatMap, length)
import Data.String.Pattern (Pattern(..))
import Data.String (split)
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..))
import Data.Map (fromFoldable, Map, lookup)

data Passport
  = Passport
    { byr :: String
    , iyr :: String
    , eyr :: String
    , hgt :: String
    , hcl :: String
    , ecl :: String
    , pid :: String
    , cid :: Maybe String
    }

keyValue :: String -> Maybe (Tuple String String)
keyValue str = case split (Pattern ":") str of
  [ key, value ] -> Just $ Tuple key value
  _ -> Nothing

parsePassport :: String -> Maybe Passport
parsePassport str = do
  byr <- lookup "byr" keyValues
  iyr <- lookup "iyr" keyValues
  eyr <- lookup "eyr" keyValues
  hgt <- lookup "hgt" keyValues
  hcl <- lookup "hcl" keyValues
  ecl <- lookup "ecl" keyValues
  pid <- lookup "pid" keyValues
  let
    cid = lookup "cid" keyValues
  Just
    $ Passport
        { byr
        , iyr
        , eyr
        , hgt
        , hcl
        , ecl
        , pid
        , cid
        }
  where
  keyValues :: Map String String
  keyValues =
    fromFoldable
      <<< catMaybes
      <<< map keyValue
      <<< concatMap words
      <<< lines
      $ str

isValid :: Passport -> Boolean
isValid passport = false

fetchInput :: Effect (Array Passport)
fetchInput =
  catMaybes
    <<< map parsePassport
    <<< split (Pattern "\n\n")
    <$> readTextFile UTF8 "data/day04/input.txt"

part1 :: Effect Int
part1 = length <$> fetchInput

part2 :: Effect Unit
part2 = pure $ unit
