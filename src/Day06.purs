module Day06 where

import Prelude
import Data.String (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.String.CodeUnits (toCharArray)
import Data.Foldable (sum)
import Data.Array (concat)
import Data.Set (fromFoldable, size)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

fetchInput :: Effect (Array (Array String))
fetchInput =
  map lines
    <<< split (Pattern "\n\n")
    <$> readTextFile UTF8 "data/day06/input.txt"

countGroupAns :: Array String -> Int
countGroupAns =
  size
    <<< fromFoldable
    <<< concat
    <<< map (toCharArray <<< trim)

part1 :: Effect Int
part1 = sum <<< map countGroupAns <$> fetchInput
