module Day02 where

import Prelude
import Data.Array (catMaybes, filter, (!!))
import Data.Foldable (length)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, trim)
import Data.String.CodeUnits (toCharArray, charAt)
import Data.String.Utils (words, lines)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

data Rule
  = Rule Int Int Char

parseRule :: String -> Maybe Rule
parseRule s = do
  min <- fromString =<< parts !! 0
  max <- fromString =<< parts !! 1
  char <- charAt 0 =<< parts !! 2
  pure $ Rule min max char
  where
  parts = words <<< replaceAll (Pattern "-") (Replacement " ") $ s

parseLine :: String -> Maybe (Tuple Rule String)
parseLine raw = do
  rawRule <- parts !! 0
  password <- parts !! 1
  rule <- parseRule rawRule
  pure $ Tuple rule password
  where
  parts = split (Pattern ": ") raw

charCount :: Char -> String -> Int
charCount c = length <<< filter ((==) c) <<< toCharArray

validatePassPart1 :: Rule -> String -> Boolean
validatePassPart1 (Rule min max char) password = count >= min && count <= max
  where
  count = charCount char password

validatePassPart2 :: Rule -> String -> Boolean
validatePassPart2 (Rule min max char) password = (c1Valid || c2Valid) && not bothValid
  where
  c1Valid = fromMaybe false $ (==) char <$> charAt (min - 1) password

  c2Valid = fromMaybe false $ (==) char <$> charAt (max - 1) password

  bothValid = c1Valid && c2Valid

fetchInput :: Effect (Array (Tuple Rule String))
fetchInput =
  catMaybes
    <<< map parseLine
    <<< lines
    <<< trim
    <$> readTextFile UTF8 "data/day02/input.txt"

solve :: (Rule -> String -> Boolean) -> Effect Int
solve f = length <<< filter (uncurry f) <$> fetchInput

part1 :: Effect Int
part1 = solve validatePassPart1

part2 :: Effect Int
part2 = solve validatePassPart2
