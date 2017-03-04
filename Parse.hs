{-# LANGUAGE DeriveDataTypeable #-}
module Parse  (parseGantt, Period(..), Gantt(..), ChartLine(..), ChartType(..), Day(..), defaultGantt, defaultDay, parseDate)
where
import Control.Monad (when)
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Number (int)
import System.Console.CmdArgs -- hack: defines important defaults
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Format (buildTime, parseTime)
import Data.Time.Calendar (Day, fromGregorian)


instance Default Day where
    def = fromGregorian 1970 1 1

defaultDay :: Day
defaultDay = def

data Period = Daily | Weekly | Monthly | Quarterly | Yearly | DefaultPeriod
 deriving (Data, Typeable, Show, Eq)

instance Default Period where
    def = DefaultPeriod

data ChartType = GanttChart | Markdown
 deriving (Data, Typeable, Show, Eq)

instance Default ChartType where
    def = GanttChart
          
data Gantt = Gantt {
      start :: Day
    , dur :: Int
    , windowStart :: Day
    , windowDur :: Int
    , inSize :: Period
    , outSize :: Period
    , entries :: [ChartLine]
    , msg     :: String
    , today :: Day
    -- Command line only options.
    , font    :: String
    , labelWidth :: Int
    , standalone :: Bool
    , markToday :: Bool 
--    , outfile :: FilePath
    , verbose :: Bool
    , file    :: FilePath
    , template :: FilePath
    , chartopts :: String
    , charttype :: ChartType
} deriving (Data, Typeable, Show)

defaultGantt = Gantt {
      start = def
    , dur = def
    , windowStart = def
    , windowDur = def
    , inSize = def
    , outSize = def
    , entries = []
    , msg = def
    , today = def
    -- Command line only options.
    , font  = def
    , labelWidth = 15
    , standalone = True
    , markToday = True
--    , outfile :: FilePath
    , verbose = False
--    , file    :: FilePath
    , template = def
    , chartopts = def
    , charttype = def
    , file = def
} 

data ConfigLine = Start Day 
                | Dur Int
                | Today Day
                | Period Period
 deriving (Data, Typeable, Show)

data ChartLine = Group String Int Int
               | SlippedGroup String Int Int Int Int
               | Task String Int Int
               | SlippedTask String Int Int Int Int
               | Milestone String Int
               | SlippedMilestone String Int Int
               | Deliverable String Int
               | SlippedDeliverable String Int Int
 deriving (Data, Typeable, Show)


parseGantt :: Gantt -> String -> Either ParseError Gantt
parseGantt cfg c = runParser gantt cfg (file cfg) c 


gantt :: GenParser Char Gantt Gantt
gantt = 
  config >>
  chart >>= 
  (\es -> getState >>= 
  (\g -> (return $  g { entries = es 
                      }) ))
          
-- Configuration -----------------------------------------

config :: GenParser Char Gantt [ConfigLine]
config = updateState (\cfg -> cfg {msg = (msg cfg) ++ " config;" }) >>
         manyTill configline (lookAhead (try chartline))

configline :: GenParser Char Gantt ConfigLine
configline = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " configline;" }) >>
  skipMany (try comment <|> blankline ) >>
  (try startDate <|> try duration <|> try reportPeriodSize <|> try reportBy <|> try todayLine) >>= (\l -> 
  skipMany (try comment <|> blankline ) >>
  return l)

startDate :: GenParser Char Gantt ConfigLine
startDate = 
  string "start:" >>
  spaces >>
  aString >>= (\v -> let t = (fromMaybe  (buildTime defaultTimeLocale []) $ parseTime defaultTimeLocale "%Y-%m-%d" v) in 
                     getState >>= (\cfg ->
                                   (when ((start cfg) == def) $ updateState (\cfg -> cfg { start = t })) >>
                                   newline >>
                                   (return $ Start t) ))


duration :: GenParser Char Gantt ConfigLine
duration = 
  string "dur:" >>
  spaces >>
  int >>= (\v -> 
  getState >>= (\cfg ->
  (when ((dur cfg) == def) $ setState cfg { dur = v }) >>
  newline >>
  (return $ Dur v) ))

parseDate :: String -> Day
parseDate v = (fromMaybe  (buildTime defaultTimeLocale []) $ parseTime defaultTimeLocale "%Y-%m-%d" v)

todayLine :: GenParser Char Gantt ConfigLine
todayLine = 
  string "today:" >>
  spaces >>
  aString >>= (\v -> let t = parseDate v in 
                     getState >>= (\cfg ->
                                   (when ((today cfg) == def) $ updateState (\cfg -> cfg { today = t })) >>
                                   newline >>
                                   (return $ Today t) ))

reportBy :: GenParser Char Gantt ConfigLine
reportBy = 
  (try (string "period:") <|> try (string "report:")) >>
  spaces >>
  reportPeriod >>= (\p -> 
  getState >>= (\cfg -> 
  (when ((outSize cfg) == def) $ setState cfg { outSize = p }) >>
  (return $ Period p)))

reportPeriod :: GenParser Char Gantt Period
reportPeriod =  try daily <|> try weekly <|> try monthly <|> try quarterly <|> try yearly
  
daily :: GenParser Char Gantt Period
daily = string "daily" >> return Daily
  
weekly :: GenParser Char Gantt Period
weekly = string "weekly" >> return Weekly

monthly :: GenParser Char Gantt Period
monthly = string "monthly" >> return Monthly
          
quarterly :: GenParser Char Gantt Period
quarterly = string "quarterly" >> return Quarterly

yearly :: GenParser Char Gantt Period
yearly =  string "yearly" >> return Yearly

reportPeriodSize :: GenParser Char Gantt ConfigLine
reportPeriodSize = 
  string "size:" >>
  spaces >>
  periodsize >>= (\p -> 
  getState >>= (\cfg -> 
  (when ((inSize cfg) == def) $ setState cfg { inSize = p }) >>
  (return $ Period p)))

periodsize :: GenParser Char Gantt Period
periodsize =  try days <|> try weeks <|> try months <|> try quarters <|> try years

days :: GenParser Char Gantt Period
days = string "days" >> return Daily

weeks :: GenParser Char Gantt Period
weeks = string "weeks" >> return Weekly

months :: GenParser Char Gantt Period
months = string "months" >> return Monthly
          
quarters :: GenParser Char Gantt Period
quarters = string "quarters" >> return Quarterly

years :: GenParser Char Gantt Period
years =  string "years" >> return Yearly


-- Chart entries -----------------------------------------
chart :: GenParser Char Gantt [ChartLine]
chart = many chartline
  
chartline :: GenParser Char Gantt ChartLine
chartline = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " chartline: " }) >>
  (try group <|> try task <|> try milestone <|> try deliverable) >>= (\l ->
  skipMany (try comment <|> blankline ) >> return l)

group :: GenParser Char Gantt ChartLine
group = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " group;" }) >>
  string "G" >>
  space >>
  spaces >>
  quotedString >>= (\nm -> 
  range >>= (\(st, end) ->
  try (newline >> (return $ Group nm st end) ) <|>  slippedGroup  nm st end))

slippedGroup :: String -> Int -> Int -> GenParser Char Gantt ChartLine
slippedGroup nm st end = range >>= (\(st', end') -> return $  SlippedGroup nm st end st' end')

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
  try (newline >> (return $ Milestone nm due) ) <|>  slippedMilestone  nm due))

slippedMilestone :: String -> Int -> GenParser Char Gantt ChartLine
slippedMilestone nm due = space >> spaces >> int >>= (\(due') -> return $  SlippedMilestone nm due due')

deliverable :: GenParser Char Gantt ChartLine
deliverable = 
  updateState (\cfg -> cfg {msg = (msg cfg) ++ " deliverable;" }) >>
  string "D" >>
  space >>
  spaces >> 
  quotedString >>= (\nm ->
  space >>
  spaces >>
  int >>= (\due -> 
  try (newline >> (return $ Deliverable nm due) ) <|>  slippedDeliverable  nm due))

slippedDeliverable :: String -> Int -> GenParser Char Gantt ChartLine
slippedDeliverable nm due = space >> spaces >> int >>= (\(due') -> return $  SlippedDeliverable nm due due')



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

comment :: GenParser Char Gantt ()
comment = spaces >> char '#' >> manyTill anyChar newline >> return ()

-- | Needs @foldl'@ from Data.List and 
-- @digitToInt@ from Data.Char.
-- from: http://stackoverflow.com/questions/10726085/how-do-i-get-parsec-to-let-me-call-read-int/10726784#10726784
--positiveNatural :: Stream s m Char => ParsecT s u m Int
int = many1 digit >>= (\s -> return $ foldl' (\a i -> a * 10 + digitToInt i) 0 s)
