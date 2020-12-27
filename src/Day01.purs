module Day01 where

import Prelude

import Effect (Effect)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Encoding (Encoding(UTF8))
import Data.String as S
import Data.Array as A
import Data.Set as Set
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

lines :: String -> Array String
lines = S.split (S.Pattern "\n")

getNumbers :: FilePath -> Effect (Array Int)
getNumbers filepath = do
  input <- readTextFile UTF8 filepath
  pure $ A.catMaybes <<< map fromString <<< lines <<< S.trim $ input


part1 :: Effect (Maybe Int)
part1 = do
  ns <- getNumbers "data/day01/input.txt"
  let set = Set.fromFoldable ns
  let complement = A.find (\n -> Set.member (2020 - n) set) ns
  pure $ (\c -> c * (2020 - c)) <$> complement

allPairs :: forall a. Ord a => Array a -> Array (Tuple a a)
allPairs a = A.concat $
  A.mapWithIndex (\i n1 ->
    A.mapMaybe (\n2 ->
      if n1 == n2 then
        Nothing
      else
        Just $ Tuple n1 n2
    )
    (A.drop (i + 1) a)
  ) a


part2 :: Effect (Maybe Int)
part2 = do
  ns <- getNumbers "data/day01/input.txt"
  let set = Set.fromFoldable ns
  let complement = A.find (\(Tuple n1 n2) -> Set.member (2020 - n1 - n2) set) $ allPairs ns
  pure $ (\(Tuple n1 n2) -> n1 * n2 * (2020 - n1 - n2)) <$> complement
