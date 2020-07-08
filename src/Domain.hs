module Domain where

data Zoo = Zoo { animals :: [Animal] } deriving (Show)

data Animal = Animal { species :: String } deriving (Show)

data IssueLevel = Warning | Error deriving (Show, Eq)

data ValidationIssue = ValidationIssue {
  level :: IssueLevel,
  message :: String
} deriving (Show)

warningLog :: String -> ValidationIssue
warningLog message = ValidationIssue {
  level = Warning,
  message = message
}

errorLog :: String -> ValidationIssue
errorLog message = ValidationIssue {
  level = Error,
  message = message
}
