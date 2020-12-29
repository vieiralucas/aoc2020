module Day05 where

import Prelude
import Data.Array (mapWithIndex, range, reverse, concat, (\\))
import Data.Set (fromFoldable, member)
import Data.Foldable (sum, maximum, find)
import Data.Int (pow)
import Data.Maybe (Maybe)
import Data.String (splitAt)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

type Seat
  = { row :: Int, col :: Int }

seatId :: Seat -> Int
seatId ({ row, col }) = row * 8 + col

fromBin :: Char -> String -> Int
fromBin zeroChar =
  sum
    <<< mapWithIndex (\i c -> if c == zeroChar then 0 else pow 2 i)
    <<< reverse
    <<< toCharArray

seatFromString :: String -> Seat
seatFromString str =
  let
    { before, after } = splitAt 7 str

    row = fromBin 'F' before

    col = fromBin 'L' after
  in
    { row, col }

fetchInput :: Effect (Array String)
fetchInput = lines <$> readTextFile UTF8 "data/day05/input.txt"

allSeats :: Array Seat
allSeats =
  concat
    $ map
        ( \row ->
            map (\col -> { row, col }) cols
        )
        rows
  where
  rows = range 0 127

  cols = range 0 7

part2 :: Effect (Maybe Int)
part2 = do
  inputSeats <- map (seatId <<< seatFromString) <$> fetchInput
  let
    diff = fromFoldable $ (map seatId allSeats) \\ inputSeats
  pure
    $ find
        ( \sId ->
            not (member (sId + 1) diff)
              && not (member (sId - 1) diff)
        )
        diff

part1 :: Effect (Maybe Int)
part1 =
  maximum
    <<< map (seatId <<< seatFromString)
    <$> fetchInput
