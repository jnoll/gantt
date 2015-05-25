{-# LANGUAGE DeriveDataTypeable #-}
module Gantt  (parseGantt, Period(..), Gantt(..), ChartLine(..), Day(..))
where
import Control.Monad (when)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import System.Console.CmdArgs -- hack: defines important defaults
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)


instance Default Day where
    def = fromGregorian 1970 1 1

defaultDay :: Day
defaultDay = def

data Period = Monthly | Quarterly | Yearly
 deriving (Data, Typeable, Show, Eq)

instance Default Period where
    def = Monthly

data Gantt = Gantt {
      start :: Day
    , dur :: Int
    , period :: Period
    , entries :: [ChartLine]
    , msg     :: String
    -- Command line only options.
    , font    :: String

    , standalone :: Bool
    , verbose :: Bool
    , file    :: FilePath
    , template :: FilePath
    , chartopts :: String
} deriving (Data, Typeable, Show)

data ConfigLine = Start Day 
                | Dur Int
                | Period Period
 deriving (Data, Typeable, Show)

data ChartLine = Group String Int Int
               | Task  String Int Int
               | Milestone String Int
 deriving (Data, Typeable, Show)


parseGantt :: Gantt -> String -> Either ParseError Gantt
parseGantt cfg c = runParser gantt cfg (file cfg) c 


gantt :: GenParser Char Gantt Gantt
gantt = do
  config
  es <- chart
  g <- getState
  return $  g { entries = es }

-- Configuration -----------------------------------------

config :: GenParser Char Gantt [ConfigLine]
config = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " config;" })
  manyTill configline (lookAhead (try chartline))


configline :: GenParser Char Gantt ConfigLine
configline = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " configline;" })
  l <- try startDate <|> try duration <|> try reportBy
  skipMany blankline
  return l

startDate :: GenParser Char Gantt ConfigLine
startDate = do 
  string "start:"
  spaces
  v <- aString
  t <- parseTimeM True defaultTimeLocale "%Y-%m-%d" v
  cfg <- getState

  when ((start cfg) == def) $ updateState (\cfg -> cfg { start = t })
  newline
  return $ Start t


duration :: GenParser Char Gantt ConfigLine
duration = do
  string "dur:"
  spaces
  v <- int
  cfg <- getState
  when ((dur cfg) == def) $ setState cfg { dur = v }
  newline
  return $ Dur v

reportBy :: GenParser Char Gantt ConfigLine
reportBy = do 
  string "period:"
  spaces
  p <- reportPeriod
  cfg <- getState
  when ((period cfg) == def) $ setState cfg { period = p }
  return $ Period p

reportPeriod :: GenParser Char Gantt Period
reportPeriod = do
  try monthly <|> try quarterly <|> try yearly
  
monthly :: GenParser Char Gantt Period
monthly = do
  string "monthly"
  return Monthly
          
quarterly :: GenParser Char Gantt Period
quarterly = do
  string "quarterly"
  return Quarterly

yearly :: GenParser Char Gantt Period
yearly = do 
  string "yearly"
  return Yearly

-- Chart entries -----------------------------------------
chart :: GenParser Char Gantt [ChartLine]
chart = many chartline
  
chartline :: GenParser Char Gantt ChartLine
chartline = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " chartline: " })
  l <- try group <|> try task <|> try milestone
  skipMany blankline
  return l

group :: GenParser Char Gantt ChartLine
group = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " group;" })
  string "G"
  space
  spaces
  nm <- quotedString
  space
  spaces
  st <- int
  space
  spaces
  end <- int
  newline
  return $ Group nm st end

task :: GenParser Char Gantt ChartLine
task = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " task;" })
  string "T"
  space
  spaces
  nm <- quotedString
  space
  spaces
  st <- int
  space
  spaces
  end <- int
  newline
  return $ Task nm st end

milestone :: GenParser Char Gantt ChartLine
milestone = do
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " milestone;" })
  string "M"
  space
  spaces
  nm <- quotedString
  space
  spaces
  due <- int
  newline
  return $ Milestone nm due

quotedString :: GenParser Char Gantt String
quotedString = do char '"'
                  content <- many quotedChar
                  char '"' <?> "quote at end of cell"
                  return content

quotedChar =
    noneOf "\"" <|> try (string "\"\"" >> return '"')

aString :: GenParser Char Gantt String
aString = do
  spaces
  many (noneOf " \t\n\r")
  
blankline :: GenParser Char Gantt ()
blankline = try (do { manyTill (oneOf " \t") (newline) ; return () })
