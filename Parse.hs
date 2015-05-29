{-# LANGUAGE DeriveDataTypeable #-}
module Parse  (parseGantt, Period(..), Gantt(..), ChartLine(..), Day(..))
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
               | SlippedTask String Int Int Int Int
               | Milestone String Int
 deriving (Data, Typeable, Show)


parseGantt :: Gantt -> String -> Either ParseError Gantt
parseGantt cfg c = runParser gantt cfg (file cfg) c 


gantt :: GenParser Char Gantt Gantt
gantt = 
  config >>
  chart >>= (\es ->
  getState >>= (\g -> 
  (return $  g { entries = es }) ))

-- Configuration -----------------------------------------

config :: GenParser Char Gantt [ConfigLine]
config = updateState (\cfg -> cfg {msg = (msg cfg) ++ " config;" }) >>
         manyTill configline (lookAhead (try chartline))

configline :: GenParser Char Gantt ConfigLine
configline = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " configline;" }) >>
  (try startDate <|> try duration <|> try reportBy) >>= (\l -> 
  skipMany blankline >>
  return l)

startDate :: GenParser Char Gantt ConfigLine
startDate = 
  string "start:" >>
  spaces >>
  aString >>= (\v -> 
  parseTimeM True defaultTimeLocale "%Y-%m-%d" v >>= (\t -> 
  getState >>= (\cfg ->
  (when ((start cfg) == def) $ updateState (\cfg -> cfg { start = t })) >>
  newline >>
  (return $ Start t) )))


duration :: GenParser Char Gantt ConfigLine
duration = 
  string "dur:" >>
  spaces >>
  int >>= (\v -> 
  getState >>= (\cfg ->
  (when ((dur cfg) == def) $ setState cfg { dur = v }) >>
  newline >>
  (return $ Dur v) ))

reportBy :: GenParser Char Gantt ConfigLine
reportBy = 
  string "period:" >>
  spaces >>
  reportPeriod >>= (\p -> 
  getState >>= (\cfg -> 
  (when ((period cfg) == def) $ setState cfg { period = p }) >>
  (return $ Period p)))

reportPeriod :: GenParser Char Gantt Period
reportPeriod =  try monthly <|> try quarterly <|> try yearly
  
monthly :: GenParser Char Gantt Period
monthly = string "monthly" >> return Monthly
          
quarterly :: GenParser Char Gantt Period
quarterly = string "quarterly" >> return Quarterly

yearly :: GenParser Char Gantt Period
yearly =  string "yearly" >> return Yearly

-- Chart entries -----------------------------------------
chart :: GenParser Char Gantt [ChartLine]
chart = many chartline
  
chartline :: GenParser Char Gantt ChartLine
chartline = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " chartline: " }) >>
  (try group <|> try task <|> try milestone) >>= (\l ->
  skipMany blankline >> return l)

group :: GenParser Char Gantt ChartLine
group = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " group;" }) >>
  string "G" >>
  space >>
  spaces >>
  quotedString >>= (\nm -> 
  space >>
  spaces >> 
  int >>= (\st -> 
  space >>
  spaces >>
  int >>= (\end -> 
  newline >>
  (return $ Group nm st end) )))

task :: GenParser Char Gantt ChartLine
task = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " task;" }) >>
  string "T" >> 
  space >> 
  spaces >> 
  quotedString >>= (\nm -> 
  range >>= (\(st, end) ->
  try (newline >> (return $ Task nm st end) ) <|>  slippedTask  nm st end))

slippedTask :: String -> Int -> Int -> GenParser Char Gantt ChartLine
slippedTask nm st end = range >>= (\(st', end') -> return $  SlippedTask nm st end st' end')

milestone :: GenParser Char Gantt ChartLine
milestone = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " milestone;" }) >>
  string "M" >>
  space >>
  spaces >> 
  quotedString >>= (\nm ->
  space >>
  spaces >>
  int >>= (\due -> 
  newline >>
  (return $ Milestone nm due)))


range :: GenParser Char Gantt (Int, Int)
range = 
  space >> spaces >>
  int >>= (\st ->
  spaces >>
  int >>= (\end -> 
  return (st, end) ))

quotedString :: GenParser Char Gantt String
quotedString =
  char '"' >>
  many quotedChar >>= (\content -> 
  (char '"' <?> "quote at end of cell") >>
  return content)

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

aString :: GenParser Char Gantt String
aString =  spaces >> many (noneOf " \t\n\r")
  
blankline :: GenParser Char Gantt ()
blankline = try (manyTill (oneOf " \t") (newline) >> return ())
