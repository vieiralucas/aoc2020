module Day06 where

import Prelude
import Data.String (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.String.CodeUnits (toCharArray)
import Data.Foldable (sum, foldl)
import Data.Array (concat, uncons)
import Data.Set (fromFoldable, size, intersection)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

fetchInput :: Effect (Array (Array String))
fetchInput =
  map (lines <<< trim)
    <<< split (Pattern "\n\n")
    <$> readTextFile UTF8 "data/day06/input.txt"

part1 :: Effect Int
part1 = sum <<< map countGroupAns <$> fetchInput
  where
  countGroupAns :: Array String -> Int
  countGroupAns =
    size
      <<< fromFoldable
      <<< concat
      <<< map (toCharArray <<< trim)

part2 :: Effect Int
part2 = sum <<< map countGroupAns <$> fetchInput
  where
  countGroupAns :: Array String -> Int
  countGroupAns ans = case uncons sets of
    Just { head, tail } -> size $ foldl intersection head tail
    Nothing -> 0
    where
    sets = map (fromFoldable <<< toCharArray <<< trim) ans
