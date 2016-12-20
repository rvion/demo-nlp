{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Text.Megaparsec hiding (space)
import Text.Megaparsec.String (Parser)
import Control.Applicative as Alt
import Data.Function ((&))
import Data.Functor.Identity (Identity)

txt1, txt2 :: String
txt1 = "le chat est joli et le train est joli"
txt2 = "la souris est dans la place"

test :: String -> IO ()
test input = do
  let parsing = runParser
        (parseEt <|>
         parseOu <|>
         parseSentence)
        "fakeFile" input
  case parsing of
    Left e -> putStrLn (parseErrorPretty e)
    Right a -> print a

data Sentence
  = Et Sentence Sentence
  | Ou Sentence Sentence
  | Sentence1
    { subject :: String
    , verb :: String
    , complement :: Maybe String
    , extra :: Maybe Sentence
    }
  deriving (Show)

parseEt :: Parser Sentence
parseEt = do
  f1 <- parseSentence
  spaces
  string "et"
  spaces
  f2 <- parseSentence
  return (Et f1 f2)

parseOu :: Parser Sentence
parseOu = do
  f1 <- parseSentence
  spaces
  string "ou"
  spaces
  f2 <- parseSentence
  return (Ou f1 f2)

parseSentence :: Parser Sentence
parseSentence = do
  subject <- oneAmongst possibleSubjects
  spaces
  verb <- possibleVerbs
    & filter (validFor subject)
    & oneAmongst
  spaces
  complement <- optional (oneAmongst possibleComplement)
  extra <- optional (parseSentence)
  return (Sentence1 {subject, verb, complement, extra})

validFor :: String -> String -> Bool
validFor "le chat" _ = True
validFor "le train" "est" = True
validFor _ _ = False

possibleSubjects :: [String]
possibleSubjects =
  [ "le chat"
  , "le train"
  ]

possibleVerbs :: [String]
possibleVerbs =
  [ "est"
  , "fonce"]

possibleComplement :: [String]
possibleComplement =
  [ "joli"
  ]

oneAmongst :: [String] -> Parser String
oneAmongst l = foldl (<|>) Alt.empty (map string l)

spaces = many space
space = char ' '
