{-# LANGUAGE DeriveDataTypeable #-}
-- following for 'here' docs
{-# LANGUAGE QuasiQuotes #-} 
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.Reader
import Data.Data (constrFields, toConstr, gmapQ, cast)
import Data.List
import Data.String.Here (i)
import Data.String.Utils (replace)
import Data.Time.Calendar (addDays, diffDays, addGregorianMonthsClip, addGregorianMonthsRollOver, addGregorianYearsRollOver, fromGregorian, toGregorian, gregorianMonthLength,)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Parse
import Paths_gantt (getDataFileName)
import System.Console.CmdArgs
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, takeBaseName, (</>), (<.>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process (system)
import Text.Printf (printf)
import Text.StringTemplate as ST

replChar :: Char -> Char
replChar ' ' = '-'
replChar '.' = '-'
replChar c = c
itemName :: String -> String
itemName s = map replChar s


data Clipped = StartClipped | EndClipped | BothClipped | NeitherClipped | UhOh

formatLink :: String -> Day -> Day -> Day -> Day -> String
formatLink label s e s' e' | s < s' && e < e' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} %
                                                    \\ganttlink[link type=slipend]{${label}}{${label}r} \\ganttnewline |] 
                           | s < s' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} \\ganttnewline |]
                           | e < e' = [i| \\ganttlink[link type=slipend]{${label}}{${label}r}  \\ganttnewline |] 
                           | True   = [i| \\ganttnewline |]

formatGroup' :: String -> String -> String -> Day -> Day -> Clipped -> Bool -> String
formatGroup' nm label fill st en cl endline = 
    let s = (formatTime defaultTimeLocale "%F" st)
        e = (formatTime defaultTimeLocale "%Y-%m-%d" en)
        nl = if endline then "\\ganttnewline" else ""
        peaks = case cl of                 
                  StartClipped -> " group left peak height = 0,  group left peak width = 0, "
                  EndClipped -> " group right peak height = 0,  group right peak width = 0, "
                  BothClipped -> " group left peak height = 0,  group left peak width = 0, group right peak height = 0,  group right peak width = 0, "
                  NeitherClipped -> ""
                  UhOh -> "group left peak height = 1,  group left peak width = 10, group right peak height = 1,  group right peak width = 10, "
    in [i| \\ganttgroup[name=${label}, ${peaks} group/.append style={draw=black,fill=${fill}}]{${nm}}{${s}}{${e}} ${nl} |]

formatGroup :: String -> String -> Maybe (Day, Day, Clipped) -> Reader Gantt String
formatGroup nm fill r= 
    case r of 
      Nothing -> return $ formatOutOfRange nm
      Just (st, en, cl) -> return $ formatGroup' nm (itemName nm) fill st en cl True


formatBar' :: String -> String -> String -> Day -> Day -> Bool -> String
formatBar' nm lbl fill st en endline = 
    let s = (formatTime defaultTimeLocale "%F" st)
        e = (formatTime defaultTimeLocale "%Y-%m-%d" en)
        nl = if endline then "\\ganttnewline" else ""
    in [i| \\ganttbar[name=${lbl}, bar/.append style={draw=black, fill=${fill}}]{${nm}}{${s}}{${e}} ${nl} |]

formatBar ::  String -> String -> Maybe (Day, Day, Clipped) -> Reader Gantt String
formatBar nm fill r = 
    case r of 
      Nothing -> return $ formatOutOfRange nm
      Just (st, en, _) -> return $ formatBar' nm (itemName nm) fill st en True
    

formatOutOfRange :: String -> String
formatOutOfRange label = printf "%% %s out of range\n" label

formatMilestone' :: String -> String -> String -> Day -> Bool -> String
formatMilestone' n label fill end_day endline = 
    let due = (formatTime defaultTimeLocale "%F" end_day) 
        nl = if endline then "\\ganttnewline" else ""
    in [i| \\ganttmilestone[name=${label}, milestone/.append style={draw=black, fill=${fill}}]{${n}}{${due}} ${nl} |]

formatMilestone :: String -> String -> Maybe Day -> Reader Gantt String
formatMilestone nm fill r = 
    case r of Just due -> return $ formatMilestone' nm (itemName nm) fill due True
              Nothing -> return $ formatOutOfRange nm

formatEntry :: ChartLine -> Reader Gantt String

formatEntry (Group n s e) = 
  let color = "cyan" in 
  dayRange s e >>= formatGroup n color

formatEntry (SlippedGroup nm st end st' end') = 
    let slipColor = if end < end' then "red" else "green"
        origColor = "black"
        label = itemName nm
    in dayRange st end >>= 
                   (\o -> case o of
                          Just (s, e, c) -> dayRange st' end' >>= 
                                         (\n -> case n of 
                                                  Just (s', e', c') -> -- both in range
                                                      return $ (
                                                             formatGroup' nm label origColor s e c False ++
                                                             formatGroup' nm (label ++ "r") slipColor s' e' c' False ++
                                                             formatLink label s e s' e')

                                                  Nothing -> -- new out of range
                                                      return $ (formatGroup' nm label origColor s e c True) ++ formatOutOfRange (nm ++ "(old)"))

                          Nothing -> dayRange st' end' >>=
                                     (\n -> case n of 
                                                  Just (s', e', c') ->  -- new in range
                                                      return $ formatOutOfRange (nm ++ "(old)") ++ formatGroup' nm (label ++ "r") slipColor s' e' c' True
                                                  Nothing -> -- both out of range
                                                      return $ formatOutOfRange (nm ++ "(both)"))
                 )

formatEntry (Task n s e) = 
    let color = "blue" in
    dayRange s e >>= formatBar n color 

formatEntry (SlippedTask nm st end st' end') = do
    let label = itemName nm 
    o <- dayRange st end
    n <- dayRange st' end'
    let slipColor = if end < end' then "red" else "green"
        origColor = "black"
    case o of Just (s, e, _) -> return $ 
                       case n of Just (s', e', _) -> (formatBar' nm label origColor s e False) ++ 
                                                     (formatBar' nm (label ++ "r") slipColor s' e' False) ++ 
                                                     (formatLink label s e s' e')
                                 Nothing -> (formatBar' nm label origColor s e True) ++ formatOutOfRange (nm ++ " (new)")
              Nothing -> return $ formatOutOfRange (nm  ++ " (orig)") ++
                       case n of Just (s', e', _) -> (formatBar' nm (label ++ "r") slipColor s' e' True)
                                 Nothing -> formatOutOfRange (nm ++ " (new)")

formatEntry (Milestone n due) = 
    msInRange due >>= formatMilestone n "blue"

formatEntry (SlippedMilestone nm d d') = do
    let label = itemName nm 
        slipColor = if d < d' then "red" else "green"
        origColor = "black"

    o <- msInRange d
    n <- msInRange d'
    case o of Just due -> return $ case n of Just due' -> (formatMilestone' nm label origColor due False) ++
                                                  (formatMilestone' nm (label ++ "r") slipColor due' False) ++ 
                                                  (printf "\\ganttlink[link type=slipms]{%s}{%sr} \\ganttnewline\n" label label )
                                             Nothing ->  (formatMilestone' nm label origColor due True) ++ 
                                                formatOutOfRange (nm ++ " (new)")
              Nothing -> return $ formatOutOfRange (nm ++ " (old)") ++
                                         case n of Just due' -> (formatMilestone' nm (label ++ "r") slipColor due' True)
                                                   Nothing ->  formatOutOfRange (nm ++ " (new)")

-- This is a hack to get different color diamonds.
formatEntry (Deliverable n d) = 
    let color = "gray" in 
    msInRange d >>= formatMilestone n color

formatEntry (SlippedDeliverable n due due') = formatEntry (SlippedMilestone n due due')


formatGantt :: Reader Gantt String
formatGantt = asks (\g -> entries g) >>= (mapM formatEntry) >>= (\ls -> return $ intercalate "\n" ls )



-- Print a line of years of a certain width.    
formatCalendarYears :: Day -> Day -> Int -> String
formatCalendarYears start end size = 
    let (st_yr, st_mo, _) = toGregorian start
        (end_yr, end_mo, _) = toGregorian end 
    in
      (intercalate "\n" $ map (\y -> [i| \\gantttitle{${y}}{${size}} |] ) $ [st_yr..end_yr]) ++ "\\ganttnewline "

formatCalendar :: Period -> Day -> Day -> String
--formatCalendar Quarterly start end = formatCalendarYears start end 4
formatCalendar Quarterly start end = [i|
 \\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar, title label font=\\tiny]{${start}}{${end}}{year, month=letter} \\ganttnewline 
 |]
formatCalendar Yearly start end = formatCalendarYears start end 1
formatCalendar Daily start end = [i|
 \\gantttitlecalendar{year, month=name, day, week, weekday, weekday=letter} \\ganttnewline 
 |]
formatCalendar Weekly start end = [i|
 \\gantttitlecalendar*[time slot format=isodate-yearmonth, title label font=\\tiny]{${start}}{${end}}{year, month=shortname} \\ganttnewline 
 |]
formatCalendar Monthly start end = [i|
 \\gantttitlecalendar*[time slot format=isodate-yearmonth, title label font=\\tiny]{${start}}{${end}}{year, month=shortname} \\ganttnewline 
 |]
-- default is a monthly compressed calendar.
formatCalendar _ start end = [i|
 \\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar, title label font=\\tiny]{${start}}{${end}}{year, month=shortname} \\ganttnewline 
 |]
-- \\gantttitlelist{1,...,$numPeriods$}{1} \\ganttnewline 

-- 1 for Monday, 7 for Sunday
dayOfWeek :: Day -> Int
dayOfWeek d = let (_, _, n) = toWeekDate d in n

grid :: Day -> (String, String)
grid d = let offset = (-) 7 $ dayOfWeek d  
             color = "white"
             style = "solid" in 
            case offset of
            0 -> ([i||], [i|,*6{${color}, ${style}}|])
            7 -> ([i|*6{${color}, ${style}},|], [i||])
            otherwise -> ([i|*${offset - 1}{${color}, ${style}},|], [i|,*${7 - offset}{${color}, ${style}}|])
                         
formatGrid :: Gantt ->  String
formatGrid g = case (outSize g) of
                 Daily -> let (preg, postg) = grid (windowStart g) 
                          in [i|%%%% formatGrid Daily 
  vgrid={${preg}*1{blue, solid}${postg}},
  milestone height=.75,
  milestone top shift=.125,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 Weekly -> let (preg, postg) = grid (windowStart g) 
                           in [i|%%%% formatGrid Weekly
  vgrid={${preg}*1{blue, solid}${postg}},
  x unit=1pt,
  milestone height=.75,
  milestone top shift=.125,
  milestone left shift=-2,
  milestone right shift=2,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 Monthly -> let (preg, postg) = grid (windowStart g) 
                            in [i|%%%% formatGrid Monthly
  vgrid={${preg}*{6}{white},{green, dotted},*{6}{white},{green, dotted},*{6}{white},{blue, solid}${postg}},
  x unit=.5pt,                  % try to get months to reflect actual dates
  milestone height=.75,
  milestone top shift=.125,
  milestone left shift=-4,
  milestone right shift=4,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 Quarterly -> [i|%%%% formatGrid Quarterly
  compress calendar, 
  vgrid={*2{white},*1{blue, solid}},
  x unit=.67em,
  milestone height=.75,
  milestone top shift=.125,
%% doesn't work  milestone left shift=-.5,
%% doesn't work  milestone right shift=.5,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 otherwise -> [i|%%%% formatGrid default
  compress calendar, 
  vgrid={*2{green, dashed},*1{blue, solid}},
  milestone height=.75,
  milestone top shift=.125,
  milestone label node/.append style={left=-.5em, align=left, text width=9em}, 
  %%%% /formatGrid|]


formatToday :: Day -> String
formatToday d = let ds = formatTime defaultTimeLocale "%F" d in 
                if d == def then "%%%% formatToday: today is def(ault)" 
                else [i| today=${ds}, today rule/.style={draw=green, ultra thick}, |]

-- calcPeriods is for calendar 'window' view, so use window start
calcPeriods :: Int -> Reader Gantt Int
calcPeriods dur = do
  g <- ask
  end_day <- endToDay dur
  let p = inSize g
      st_day = windowStart g
  return $ case p of
      Daily     -> fromIntegral $ (+) 1 $ diffDays end_day st_day
      Weekly    -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 7
      Quarterly -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365 * 4
      Yearly    -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365
      otherwise -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365 * 12

-- calcEnd is for calendar 'window' view, so used window start
calcEnd :: Day -> Reader Gantt Int
calcEnd day = do
  g <- ask
  return $ case (inSize g) of
             Daily  -> fromIntegral $ (+) 1 $  diffDays day (windowStart g)
             Weekly -> round $ (fromIntegral (diffDays day (windowStart g))) / 7
             Quarterly -> let (y, m, _) = toGregorian day
                              (st_y, _, _) = toGregorian (windowStart g)
                          in ceiling $ (fromIntegral ((((fromIntegral y) - (fromIntegral st_y)) * 12) + m)) / 3
             Yearly -> let (y, m, _) = toGregorian day
                           (st_y, _, _) = toGregorian (windowStart g)
                       in (fromIntegral y) - (fromIntegral st_y)
             otherwise -> let (y, m, _) = toGregorian day
                              (st_y, st_m, _) = toGregorian (windowStart g)
                          in (((fromIntegral y) - (fromIntegral st_y)) * 12) + (m - st_m + 1)
                                      
calcStart :: Day -> Reader Gantt Int
calcStart day = do
  g <- ask
  e <- calcEnd day 
  let p = inSize g
      s = windowStart g
  return $ case p of
             Daily -> e 
             Weekly -> (+) 1 $ round  $ (fromIntegral $ diffDays day s) / 7
             otherwise -> e


-- convert a chart start offset into a Day.  The start period is
-- actually the *end* of the previous period.  
startToDay :: Int -> Reader Gantt Day
startToDay offset = do
  g <- ask
  return $ let offset' = toInteger (offset) 
               st = start g
           in
           case (inSize g) of
             Daily -> addDays offset' st
             Weekly -> addDays (toInteger (offset' * 7) + 0) st
             Quarterly -> addGregorianMonthsClip (offset' * 3) st
             Yearly -> addGregorianMonthsClip (offset' * 12) st
             otherwise -> addGregorianMonthsClip offset' st

endOfMonth :: Day -> Day
endOfMonth day = let (y, m, _) = toGregorian day 
                 in fromGregorian y m $ gregorianMonthLength y m -- move to end of month; months w. less than 31 days get correct last day.

offsetToDay :: Day -> Integer -> Period -> Day
offsetToDay st offset p = case p of
             Daily -> addDays offset st -- no adjustment necessary?
             Weekly -> addDays ((offset * 7) + 6) st
             Quarterly -> endOfMonth $ addGregorianMonthsClip (toInteger (offset * 3) - 1) st
             Yearly -> endOfMonth $ addGregorianMonthsClip (toInteger (offset * 12) - 1) st
             otherwise -> endOfMonth $ addGregorianMonthsClip offset st -- Monthly is default

-- Convert a chart end offset into a Day.  The calculated date has to
-- be at the *end* of the period (for example, 28 Feb not 1 Feb).
endToDay :: Int -> Reader Gantt Day
endToDay offset = do
  g <- ask
  let p = inSize g
      st = start g
      offset' = toInteger (offset - 1) 
  return $ offsetToDay st offset' p

before :: Day -> Day -> Bool
before a b = if diffDays a b <= 0 then True else False

after :: Day -> Day -> Bool
after a b = if diffDays a b > 0 then True else False


computeRange :: Day -> Day -> Day -> Day -> (Day, Day, Clipped)
computeRange s e start end 
    | (before s start) && (after e end)  = (start, end, BothClipped)
    | (before s start) && (before e end) = (start, e, StartClipped)
    | (after s start) && (after e end)   = (s, end, EndClipped)
    | (after s start) && (before e end)  = (s, e, NeitherClipped)
    | otherwise                          = (s, e, UhOh) -- XXX should never happen

dayRange :: Int -> Int -> Reader Gantt (Maybe (Day, Day, Clipped))
dayRange s e = do
  g <- ask
  let start = windowStart g
      end = offsetToDay start (toInteger ((windowDur g) - 1)) (inSize g)
  s' <- startToDay s
  e' <- endToDay e
  let r = if before e' start || after s' end then Nothing
          else Just $ computeRange s' e' start end
  return r

msInRange :: Int -> Reader Gantt (Maybe Day)
msInRange d = do
  g <- ask
  let start = windowStart g
      end = offsetToDay start (toInteger ((windowDur g) - 1)) (inSize g)
  due <- endToDay d
  return $ if after due start && before due end then Just due else Nothing

printGantt :: Gantt -> ST.StringTemplate String -> Handle -> IO ()
printGantt g tmpl h = do
  when (verbose g) $ do
    putStrLn "--- gantt ---"
    putStrLn $ show $ g
  let end = dur g :: Int
  let st = 1 :: Int
--  let end_date = runReader (endToDay $ addDays (windowDur g) (windowStart g)) g
  let end_date = offsetToDay (windowStart g) (toInteger $ (windowDur g) - 1) (inSize g)
  let body = runReader formatGantt g 
  when (verbose g) $ do
    putStrLn "--- body ---"
    putStrLn $ body
    putStrLn "--- ------ ---"
  hPutStrLn h $ ST.toString 
                $ (ST.setManyAttrib $ filter (\(k, v) -> length v > 0) $ showEm g)
                $ (ST.setAttribute "vgrid" $ formatGrid g)
                $ (ST.setAttribute "calendar" $ formatCalendar (outSize g) (windowStart g) end_date)
                $ (ST.setAttribute "numPeriods" $ runReader (calcPeriods $ windowDur g) g)
                $ (ST.setAttribute "todayDate" $ formatToday (today g))
                $ ST.setAttribute "end" (formatTime defaultTimeLocale "%F" $ end_date) -- end date, for calendar lines in monthly
                $ ST.setAttribute "body" body -- actual chart elements
                  tmpl 
-- Command line parsing and processing --------------------------------------------------------------------------

-- Convert a record to a list.
showEm :: (Data a) => a -> [(String, String)]
showEm x = zip (constrFields $ toConstr x) (gmapQ (showIt [showPeriod, showBool, showDouble, showInt, showDate, showStr]) x)

-- Using a list of "showing" functions, find a representation of d.
showIt :: (Data a) => [(a -> Either String a)] -> a -> String
showIt ss d = either id (\_ -> "XXX Nope XXX") (foldl (>>=) (return d) ss)

-- Show various primitive types.
showInt, showDouble, showPeriod, showBool, showStr, showDef :: (Data a) => a -> Either String a
showInt    d = maybe (Right d) (Left . show) (cast d :: Maybe Int)
showDouble d = maybe (Right d) (Left . show) (cast d :: Maybe Double)
showPeriod d = maybe (Right d) (Left . show) (cast d :: Maybe Period)
showBool   d = maybe (Right d) (Left . show) (cast d :: Maybe Bool)
showDate   d = maybe (Right d) (Left . show) (cast d :: Maybe Day)
showStr    d = maybe (Right d) (Left)        (cast d :: Maybe String)
showDef    d = maybe (Right d) (Left . show) (cast d :: Maybe String)



-- help, summary, and program are for command line argument parsing.  
data Options = Options {
      opt_start :: String
    , opt_dur :: Int
    , opt_windowStart :: String
    , opt_windowDur :: Int
    , opt_inSize :: Period
    , opt_outSize :: Period
    -- Command line only options.
    , opt_font    :: String
    , opt_standalone :: Bool
    , opt_markToday :: Bool 
    , opt_outfile :: FilePath
    , opt_verbose :: Bool
    , opt_file    :: FilePath
    , opt_template :: FilePath
    , opt_chartopts :: String
} deriving (Data, Typeable, Show)

defaultOptions :: Options
defaultOptions = Options { 
                 opt_start  = def            &= help "Project start date" &= explicit &= name "start"
               , opt_dur    = def            &= help "Project duration (periods)" &= explicit &= name "dur" 
               , opt_windowStart  = def         &= help "Output 'window' start date (yyyy-mm-dd)" &= explicit &= name "winst"
               , opt_windowDur = def         &= help "Output 'window' duration (periods)"   &= explicit &= name "windur"
               , opt_inSize = def            &= help "Input period size (default: monthly)"  &= explicit &= name "insize"
               , opt_outSize = enum [ DefaultPeriod
                               , Monthly &= help "Output report Monthly (default)"
                               , Daily   &= help "Output report Daily"
                               , Weekly  &= help "Output report Weekly"
                               , Quarterly &= help "Output report Quarterly"
                               , Yearly  &= help "Output report Yearly"
                               ] 
               -- Command line only options.
               , opt_font = def              &= help "Typeface for printed chart" &= explicit &= name "font"
               , opt_standalone = True       &= help "Generate standlone latex file" &= explicit &= name "standalone"
               , opt_markToday = False       &= help "Show today's date as 'today'" &= explicit &= name "today"
               , opt_outfile = "stdout"      &= help "Output file" &= name "outfile" 
               , opt_verbose = False         &= help "Print diagnostics as well" &= explicit &= name "verbose"
               , opt_file   = "test.gantt"   &= args &= typFile 
               , opt_template = def          &= help "Template for standalone output" &= explicit &= name "template"
               , opt_chartopts = def         &= help "Options for \\pgfganttchart" &= explicit &= name "chartopts"
               }
       &= summary "Gantt chart v0.1, (C) 2016 John Noll"
       &= program "main"


makePDF :: Gantt -> String -> FilePath -> IO ()
makePDF g tmpl outfile = getCurrentDirectory >>= (\cwd ->
                               withSystemTempDirectory "ganttpdf"  (\d ->
                               setCurrentDirectory d >>
                               let texFile = (takeBaseName outfile) <.> "tex" in
                               openFile texFile WriteMode >>= (\h -> 
                               printGantt g (ST.newSTMP tmpl) h >> 
                               hClose h >>
                               (system $ "pdflatex " ++ texFile ++ " > /dev/null" ) >>
                               setCurrentDirectory cwd >>
                               (system $ "cp " ++ (d </> (takeBaseName texFile) <.> "pdf") ++ " " ++ outfile) >> 
                               return () )))

getTemplate :: FilePath -> IO String
getTemplate ""  = (getDataFileName $ "templates" </> "gantt.st") >>= (\d -> readFile d)
getTemplate p   = readFile p

ifDef :: (Eq a, Default a)  => a -> a -> a
ifDef x y = if x == def then y else x

main :: IO ()
main = do
  args <- cmdArgs defaultOptions
  todays_date <- getCurrentTime >>= return . utctDay 

  when (opt_verbose args) $ do
      putStrLn "--- args ---"
      putStrLn $ show $ args

  t <- getTemplate (opt_template args)
  c <- readFile (opt_file args)


  let cfg = defaultGantt 

  case parseGantt cfg {      
             start = if (opt_start args) /= def then parseDate (opt_start args)
                     else (start cfg)
           , dur   = ifDef (opt_dur args)   (dur cfg)
           , windowStart = if (opt_windowStart args) /= def then parseDate (opt_windowStart args)
                        else (windowStart cfg)
           , windowDur = ifDef (opt_windowDur args) (windowDur cfg)
           , inSize = ifDef (opt_inSize args) (inSize cfg)
           , outSize = ifDef (opt_outSize args) (outSize cfg)
           , today = if (opt_markToday args) then todays_date else (today cfg)
           , font  = opt_font args
           , standalone = opt_standalone args
           , verbose = opt_verbose args
           , file = opt_file args
           , template = opt_template args
           , chartopts = opt_chartopts args
           } c of
    Left e -> putStrLn $ show $ e
    Right g' -> let g = g' { windowStart  = if diffDays (windowStart g') (fromGregorian 1970 1 1) == 0 then start g' else windowStart g'
                           , windowDur = if windowDur g' == 0 then dur g' else windowDur g'
                           } 
                in if (opt_outfile args) == "stdout" then printGantt g (ST.newSTMP t) stdout else 
                   case takeExtension (opt_outfile args) of
                     ".pdf" -> makePDF g t (opt_outfile args) 
                     ".png" -> let pdfFile = (takeBaseName (opt_outfile args)) <.> "pdf" in
                               (makePDF g t $ pdfFile) >>
                               -- the density is high so image can be resized without pixelating.
                               (system $ "convert -density 1200 -quality 100 " ++ pdfFile ++ " " ++ (opt_outfile args)) >>
                               return ()
                     otherwise -> (openFile (opt_outfile args) WriteMode >>= (\h ->
                                   printGantt g (ST.newSTMP t) h >> hClose h))

