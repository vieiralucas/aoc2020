module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Day01 as D01
import Day02 as D02
import Day03 as D03
import Day04 as D04
import Day05 as D05
import Day06 as D06

main :: Effect Unit
main = do
  log "Day 01:"
  d1p1 <- D01.part1
  log $ "part1: " <> show d1p1
  d1p2 <- D01.part2
  log $ "part2: " <> show d1p2
  log "-"
  log "Day 02:"
  d2p1 <- D02.part1
  log $ "part1: " <> show d2p1
  d2p2 <- D02.part2
  log $ "part2: " <> show d2p2
  log "-"
  log "Day 03:"
  d3p1 <- D03.part1
  log $ "part1: " <> show d3p1
  d3p2 <- D03.part2
  log $ "part2: " <> show d3p2
  log "-"
  log "Day 04:"
  d4p1 <- D04.part1
  log $ "part1: " <> show d4p1
  d4p2 <- D04.part2
  log $ "part2: " <> show d4p2
  log "-"
  log "Day 05:"
  d5p1 <- D05.part1
  log $ "part1: " <> show d5p1
  d5p2 <- D05.part2
  log $ "part2: " <> show d5p2
  log "-"
  log "Day 06:"
  d6p1 <- D06.part1
  log $ "part1: " <> show d6p1
  d6p2 <- D06.part2
  log $ "part2: " <> show d6p2
