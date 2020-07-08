module ZooParserWriter (
  parseZoo
) where

import qualified Control.Monad.Writer as W
import qualified Data.Either as E
import Domain

type Validation a = W.Writer [ValidationIssue] a

type Result a = (a, [ValidationIssue])

animalWithLog :: (String -> ValidationIssue) -> String -> String -> Validation Animal
animalWithLog logger species message = W.writer (Animal species, [logger message])

animalWithWarning :: String -> String -> Validation Animal
animalWithWarning = animalWithLog warningLog

animalWithError :: String -> String -> Validation Animal
animalWithError = animalWithLog errorLog

parseAnimalWriter :: String -> Validation Animal
parseAnimalWriter animal = case animal of
  "cat" -> pure $ Animal animal
  "dog" -> pure $ Animal animal
  "giraffe" -> animalWithWarning animal ("Too long neck: " ++ animal)
  "zebra" -> animalWithWarning animal ("Too stripey: " ++ animal)
  otherwise -> animalWithError animal ("Unacceptable animal: " ++ animal)

parseZooWriter :: [String] -> Validation Zoo
parseZooWriter animals = Zoo <$> traverse parseAnimalWriter animals

parseZoo :: [String] -> E.Either [ValidationIssue] (Result Zoo)
parseZoo animals =
  if hasValidationErrors issues
    then E.Left issues
    else E.Right (zoo, issues)
  where
    hasValidationErrors = any $ (==) Error . level
    (zoo, issues) = W.runWriter $ parseZooWriter animals
