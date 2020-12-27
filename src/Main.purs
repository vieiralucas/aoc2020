module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Day01 as D01

main :: Effect Unit
main = do
  log "Day 01:"
  d1p1 <- D01.part1
  log $ "part1: " <> show d1p1
  d1p2 <- D01.part2
  log $ "part2: " <> show d1p2
