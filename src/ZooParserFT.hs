{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module ZooParserFT (
  parseZoo
) where

import qualified Control.Monad.Writer as W
import qualified Control.Monad.Writer.Class as MW
import qualified Data.Either as E
import Domain

type Validation m = MW.MonadWriter [ValidationIssue] m

type Result a = (a, [ValidationIssue])

animalWithLog :: Validation m => (String -> ValidationIssue) -> String -> String -> m Animal
animalWithLog logger species message = MW.writer (Animal species, [logger message])

animalWithWarning :: Validation m => String -> String -> m Animal
animalWithWarning = animalWithLog warningLog

animalWithError :: Validation m => String -> String -> m Animal
animalWithError = animalWithLog errorLog

parseAnimalM :: Validation m => String -> m Animal
parseAnimalM animal = case animal of
  "cat" -> pure $ Animal animal
  "dog" -> pure $ Animal animal
  "giraffe" -> animalWithWarning animal ("Too long neck: " ++ animal)
  "zebra" -> animalWithWarning animal ("Too stripey: " ++ animal)
  otherwise -> animalWithError animal ("Unacceptable animal: " ++ animal)

parseZooM :: Validation m => [String] -> m Zoo
parseZooM animals = Zoo <$> traverse parseAnimalM animals

parseZoo :: [String] -> E.Either [ValidationIssue] (Result Zoo)
parseZoo animals =
  if hasValidationErrors issues
    then E.Left issues
    else E.Right (zoo, issues)
  where
    hasValidationErrors = any $ (==) Error . level
    (zoo, issues) = W.runWriter $ parseZooM animals
