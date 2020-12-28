module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Day01 as D01
import Day02 as D02
import Day03 as D03

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
