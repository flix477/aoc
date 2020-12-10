{-# LANGUAGE TemplateHaskell #-}
-- Who would win, this long boi or a  t h i c c  regex

module Main where

import Control.Lens hiding (noneOf)
import Control.Lens.TH
import Data.Functor
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec


data Units = Centimeters | Inches deriving (Show, Eq)
type Measurement = (Int, Units)


data Passport = Passport
  { passportBirthYear :: Int
  , passportIssueYear :: Int
  , passportExpirationYear :: Int
  , passportHeight :: Measurement
  , passportHairColor :: String
  , passportEyeColor :: String
  , passportId :: String
  , passportCountryId :: Maybe String
  } deriving (Show)


data PassportBuilder = PassportBuilder
  { _builderBirthYear :: Maybe Int
  , _builderIssueYear :: Maybe Int
  , _builderExpirationYear :: Maybe Int
  , _builderHeight :: Maybe Measurement
  , _builderHairColor :: Maybe String
  , _builderEyeColor :: Maybe String
  , _builderId :: Maybe String
  , _builderCountryId :: Maybe String
  } deriving (Show)
$(makeLenses ''PassportBuilder)


main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ count isValid <$> parse parsePassports "" input
  where count f = length . filter f


parsePassports :: GenParser Char st [Passport]
parsePassports = catMaybes <$> manyTill (parsePassport <|> skip) eof
  where parsePassport = build' <$> try parseProps
        skip = manyTill anyChar endOrPropsSep $> Nothing


propsSep = try (string "\n\n" $> ())
propSep = oneOf " \n"
end = try (optional (char '\n') >> eof)
endOrPropsSep = end <|> propsSep
parseLabel = many1 $ noneOf ": \n"
colors = ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]
setJust lens = set lens . Just


parseProps :: GenParser Char st (PassportBuilder -> PassportBuilder)
parseProps = do
  prop <- parseProp
  remaining <- (endOrPropsSep $> id) <|> (propSep >> parseProps)
  pure $ prop . remaining


parseProp :: GenParser Char st (PassportBuilder -> PassportBuilder)
parseProp = do
  label <- parseLabel
  char ':'
  parseValue label


parseValue :: String -> GenParser Char st (PassportBuilder -> PassportBuilder)
parseValue "byr" = setJust builderBirthYear <$> parseYear
parseValue "iyr" = setJust builderIssueYear <$> parseYear
parseValue "eyr" = setJust builderExpirationYear <$> parseYear
parseValue "hgt" = setJust builderHeight <$> parseMeasurement
parseValue "hcl" = setJust builderHairColor <$> (char '#' >> count 6 hexDigit)
parseValue "ecl" = setJust builderEyeColor <$> choice (try . string <$> colors)
parseValue "pid" = setJust builderId <$> count 9 digit
parseValue "cid" = setJust builderCountryId <$> parseLabel
parseValue _ = fail "invalid key"


parseYear :: GenParser Char st Int
parseYear = read <$> count 4 digit


parseMeasurement :: GenParser Char st Measurement
parseMeasurement = do
  value <- read <$> many1 digit
  units <- (string "cm" $> Centimeters) <|> (string "in" $> Inches)
  pure (value, units)


isValid :: Passport -> Bool
isValid p
   = birthYear <? (1920, 2002)
  && issueYear <? (2010, 2020)
  && expirationYear <? (2020, 2030)
  && (
     ( heightUnits == Centimeters && height <? (150, 193))
    || (heightUnits == Inches && height <? (59, 76))
     )
  where birthYear = passportBirthYear p
        issueYear = passportIssueYear p
        expirationYear = passportExpirationYear p
        (height, heightUnits) = passportHeight p


builder :: PassportBuilder
builder = PassportBuilder
  { _builderBirthYear = Nothing
  , _builderIssueYear = Nothing
  , _builderExpirationYear = Nothing
  , _builderHeight = Nothing
  , _builderHairColor = Nothing
  , _builderEyeColor = Nothing
  , _builderId = Nothing
  , _builderCountryId = Nothing
  }


build :: PassportBuilder -> Maybe Passport
build b = do
  birthYear <- _builderBirthYear b
  issueYear <- _builderIssueYear b
  expirationYear <- _builderExpirationYear b
  height <- _builderHeight b
  hairColor <- _builderHairColor b
  eyeColor <- _builderEyeColor b
  pid <- _builderId b
  pure $ Passport
    { passportBirthYear = birthYear
    , passportIssueYear = issueYear
    , passportExpirationYear = expirationYear
    , passportHeight = height
    , passportHairColor = hairColor
    , passportEyeColor = eyeColor
    , passportId = pid
    , passportCountryId = _builderCountryId b
    }


build' :: (PassportBuilder -> PassportBuilder) -> Maybe Passport
build' f = build $ f builder


(<?) :: Ord a => a -> (a, a) -> Bool
(<?) a (lower, upper) = a <= upper && a >= lower
