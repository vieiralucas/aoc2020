module Day04 where

import Prelude
import Data.Array (catMaybes, concatMap, length, filter)
import Data.Int (fromString)
import Data.Map (fromFoldable, Map, lookup)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Char.Unicode (isDigit)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

type Passport
  = { byr :: String
    , iyr :: String
    , eyr :: String
    , hgt :: String
    , hcl :: String
    , ecl :: String
    , pid :: String
    , cid :: Maybe String
    }

keyValue :: String -> Maybe (Tuple String String)
keyValue str = case S.split (Pattern ":") str of
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
    $ { byr
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

validateInt :: Int -> Int -> String -> Boolean
validateInt min max str =
  fromMaybe false $ between min max
    <$> fromString str

validateHeight :: String -> Boolean
validateHeight str =
  let
    { after, before } = S.splitAt (S.length str - 2) str
  in
    case after of
      "cm" -> validateInt 150 193 before
      "in" -> validateInt 59 76 before
      _ -> false

validateHexColor :: String -> Boolean
validateHexColor str = case reg of
  Left _ -> false
  Right r -> test r str
  where
  reg = regex "^#([0-9A-F]|[0-9a-f]){6}$" noFlags

validateEyeColor :: String -> Boolean
validateEyeColor = flip Set.member $ validEyeColors
  where
  validEyeColors =
    Set.fromFoldable
      [ "amb"
      , "blu"
      , "brn"
      , "gry"
      , "grn"
      , "hzl"
      , "oth"
      ]

validatePid :: String -> Boolean
validatePid =
  ((==) 9)
    <<< length
    <<< filter isDigit
    <<< toCharArray

isValid :: Passport -> Boolean
isValid passport = byr && iyr && eyr && hgt && hcl && ecl && pid
  where
  byr = validateInt 1920 2002 $ passport.byr

  iyr = validateInt 2010 2020 $ passport.iyr

  eyr = validateInt 2020 2030 $ passport.eyr

  hgt = validateHeight passport.hgt

  hcl = validateHexColor passport.hcl

  ecl = validateEyeColor passport.ecl

  pid = validatePid passport.pid

fetchInput :: Effect (Array Passport)
fetchInput =
  catMaybes
    <<< map parsePassport
    <<< S.split (Pattern "\n\n")
    <$> readTextFile UTF8 "data/day04/input.txt"

part1 :: Effect Int
part1 = length <$> fetchInput

part2 :: Effect Int
part2 = length <<< filter isValid <$> fetchInput
