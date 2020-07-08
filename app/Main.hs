module Main where

import qualified Data.Either as E
import qualified ZooParserWriter as Z0
import qualified ZooParserFT as Z1
import qualified ZooParserFT2 as Z2
import Domain

explain :: E.Either [ValidationIssue] (Zoo, [ValidationIssue]) -> String
explain = E.either onLeft onRight
  where
    onLeft errors = "Validation errors: " ++ show errors
    onRight (zoo, warnings) = show zoo ++ ", warnings: " ++ show warnings

main :: IO ()
main = do
  putStrLn $ explain $ Z0.parseZoo ["cat"]
  putStrLn $ explain $ Z1.parseZoo ["cat", "giraffe"]
  putStrLn $ explain $ Z2.parseZoo config ["dog", "zebra"]
  where config = Z2.Config {
    Z2.giraffeWarning = "Too splotchy",
    Z2.zebraWarning = "Too monochrome"
  }
