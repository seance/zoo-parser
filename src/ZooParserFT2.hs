{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module ZooParserFT2 (
  Config(..),
  parseZoo
) where

import qualified Control.Monad.Writer.Class as MW
import qualified Control.Monad.Reader.Class as MR
import qualified Data.Either as E
import qualified Control.Monad.RWS as RWS
import Domain

data Config = Config {
  giraffeWarning :: String,
  zebraWarning :: String
}

type Validation m = MW.MonadWriter [ValidationIssue] m

type Configured m = MR.MonadReader Config m

type Result a = (a, [ValidationIssue])

animalWithLog :: Validation m => (String -> ValidationIssue) -> String -> String -> m Animal
animalWithLog logger species message = MW.writer (Animal species, [logger message])

animalWithWarning :: Validation m => String -> String -> m Animal
animalWithWarning = animalWithLog warningLog

animalWithError :: Validation m => String -> String -> m Animal
animalWithError = animalWithLog errorLog

parseAnimalM :: (Validation m, Configured m) => String -> m Animal
parseAnimalM animal = case animal of
  "cat" -> pure $ Animal animal
  "dog" -> pure $ Animal animal
  "giraffe" -> do
    warning <- MR.asks giraffeWarning
    animalWithWarning animal (warning ++ ": " ++ animal)
  "zebra" -> do
    warning <- MR.asks zebraWarning
    animalWithWarning animal (warning ++ ": " ++ animal)
  otherwise -> animalWithError animal ("Unacceptable animal: " ++ animal)

parseZooM :: (Validation m, Configured m) => [String] -> m Zoo
parseZooM animals = Zoo <$> traverse parseAnimalM animals

parseZoo :: Config -> [String] -> E.Either [ValidationIssue] (Result Zoo)
parseZoo config animals =
  if hasValidationErrors issues
    then E.Left issues
    else E.Right (zoo, issues)
  where
    hasValidationErrors = any $ (==) Error . level
    (zoo, (), issues) = RWS.runRWS (parseZooM animals) config ()
