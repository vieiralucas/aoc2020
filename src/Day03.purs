module Day03 where

import Prelude
import Data.Array (concat, mapWithIndex, catMaybes, (!!))
import Data.Set (Set, fromFoldable, member)
import Data.Foldable (length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)


data Pos = Pos { x :: Int, y :: Int }

posX :: Pos -> Int
posX (Pos { x }) = x

posY :: Pos -> Int
posY (Pos { y }) = y

pos :: Int -> Int -> Pos
pos x y = Pos { x, y }

derive instance eqPos :: Eq Pos
derive instance ordPos :: Ord Pos

instance showPos :: Show Pos where
  show (Pos { x, y }) = "Pos { x: " <> show x <> ", y: " <> show y <> " }"

instance addPos :: Semiring Pos where
  add p1 p2 = pos (posX p1 + posX p2) (posY p1 + posY p2)
  zero = pos 0 0
  mul p1 p2 = Pos { x: posX p1 * posX p2, y: posY p1 * posY p2 }
  one = pos 1 1

data TreeMap = TreeMap { width :: Int, height :: Int, trees :: Set Pos }

treeMapHeight :: TreeMap -> Int
treeMapHeight (TreeMap { height }) = height
   
derive instance eqTreeMap :: Eq TreeMap
derive instance ordTreeMap :: Ord TreeMap

instance showTreeMap :: Show TreeMap where
  show (TreeMap { width, height, trees}) =
    "TreeMap { width: " <> show width
    <> ", height: " <> show height
    <> ", trees: " <> show trees <> " }"

hasTree :: Pos -> TreeMap -> Boolean
hasTree (Pos { x, y }) (TreeMap { width, trees }) =
  member (pos x' y) trees
  where
    x' = x `mod` width

walk :: Pos -> Pos -> Int -> TreeMap -> Int
walk position direction count treeMap =
  if posY position < treeMapHeight treeMap then
    walk (position + direction) direction count' treeMap
  else
    count'
  where
    count' =
      if hasTree position treeMap then
        count + 1
      else
        count

parseLine :: Int -> String -> Array Pos
parseLine y = catMaybes <<< mapWithIndex mapper <<< toCharArray
  where
    mapper x c =
      if c == '#' then
        Just (pos x y)
      else
        Nothing

fetchInput :: Effect TreeMap
fetchInput = do
  input <- lines <<< trim <$> readTextFile UTF8 "data/day03/input.txt"
  let height = length input
  let width = fromMaybe 0 $ length <<< toCharArray <$> input !! 0
  let trees = fromFoldable <<< concat <<< mapWithIndex parseLine $ input
  pure $ TreeMap { width, height, trees }


part1 :: Effect Int
part1 = walk zero (pos 3 1) 0 <$> fetchInput
