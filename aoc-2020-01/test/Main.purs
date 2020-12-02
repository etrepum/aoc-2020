module Test.Main where

import Prelude

import Effect (Effect)
import Main (mulPair, findPair)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert ((mulPair <$> findPair 2020 [1721, 979, 366, 299, 675, 1456]) == pure 514579)
