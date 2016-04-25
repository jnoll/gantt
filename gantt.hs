{-# LANGUAGE DeriveDataTypeable #-}
-- following for 'here' docs
{-# LANGUAGE QuasiQuotes #-} 
--import Data.Maybe (fromMaybe)
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

formatLink :: String -> Day -> Day -> Day -> Day -> String
formatLink label s e s' e' | s < s' && e < e' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} %
                                                    \\ganttlink[link type=slipend]{${label}}{${label}r} \\ganttnewline |] 
                           | s < s' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} \\ganttnewline |]
                           | e < e' = [i| \\ganttlink[link type=slipend]{${label}}{${label}r}  \\ganttnewline |] 
                           | True   = "\\ganttnewline"

formatEntry :: ChartLine -> Reader Gantt String
formatEntry (Group n s e) = do
  st_day <- startToDay s
  end_day <- endToDay e
  return $ printf "\\ganttgroup{%s}{%s}{%s}\t\\ganttnewline" n 
                            (formatTime defaultTimeLocale "%F" st_day) 
                            (formatTime defaultTimeLocale "%F" end_day )

formatEntry (SlippedGroup n st end st' end') = 
    do
    let label = itemName n 
    s <- startToDay st
    e <- endToDay end
    s' <- startToDay st'
    e' <- endToDay end'
    let slipColor = if end < end' then "red" else "green"
    return $ [i|%
 \\ganttgroup[name=${label}, group/.append style={draw=black,fill=black}]{${n}}{${s}}{${e}} 
 \\ganttgroup[name=${label}r, group/.append style={draw=black,fill=${slipColor}}]{${n}}{${s'}}{${e'}}
 |] ++ formatLink label s e s' e'

formatEntry (Task n s e) = endToDay e >>= (\end_day -> 
                           startToDay s >>= (\st_day ->
                           return $ printf "\\ganttbar{%s}{%s}{%s}\t\\ganttnewline" n (formatTime defaultTimeLocale "%F" st_day) (formatTime defaultTimeLocale "%Y-%m-%d" end_day)))

formatEntry (SlippedTask n st end st' end') = do
    let label = itemName n 
    s <- startToDay st
    e <- endToDay end
    s' <- startToDay st'
    e' <- endToDay end'
    let slipColor = if end < end' then "red" else "green"
    return $ [i|%
 \\ganttbar[name=${label}, bar/.append style={draw=black, fill=black}]{${n}}{${s}}{${e}}
 \\ganttbar[name=${label}r, bar/.append style={draw=black, fill=${slipColor}}]{${n}}{${s'}}{${e'}}
 |] ++ formatLink label s e s' e'

formatEntry (Milestone n due) = endToDay due >>= (\end_day -> return $ printf "\\ganttmilestone{%s}{%s}\t\\ganttnewline" n (formatTime defaultTimeLocale "%F" end_day))

formatEntry (SlippedMilestone n due due') = do
    let label = itemName n 
    d <- endToDay due >>= (\e -> return $ formatTime defaultTimeLocale "%F" e)
    d'<- endToDay due' >>= (\e -> return $ formatTime defaultTimeLocale "%F" e)
    let slipColor = if d < d' then "red" else "green"
    return $ [i|%
 \\ganttmilestone[name=${label}, milestone/.append style={draw=black, fill=black}]{${n}}{${d}}
 \\ganttmilestone[name=${label}r, milestone/.append style={draw=black, fill=${slipColor}}]{${n}}{${d'}}
 \\ganttlink[link type=slipms]{${label}}{${label}r} \\ganttnewline 
 |]

-- This is a hack to get different color diamonds.
formatEntry (Deliverable n d) = endToDay d >>= (\end_day -> return $ 
    printf "\\ganttmilestone[milestone/.append style={draw=black, fill=green}]{%s}{%s}\t\\ganttnewline" n (formatTime defaultTimeLocale "%F" end_day))

formatEntry (SlippedDeliverable n due due') = do
    let label = itemName n 
    d <- endToDay due >>= (\e -> return $ formatTime defaultTimeLocale "%F" e)
    d'<- endToDay due' >>= (\e -> return $ formatTime defaultTimeLocale "%F" e)
    let slipColor = if d < d' then "red" else "green"
    return $ [i|%
 \\ganttmilestone[name=${label}, milestone/.append style={draw=black, fill=black}]{${n}}{${d}} 
 \\ganttmilestone[name=${label}r, milestone/.append style={draw=black, fill=${slipColor}}]{${n}}{${d'}}
 \\ganttlink[link type=slipms]{${label}}{${label}r} \\ganttnewline 
 |]


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
-- default is monthly
formatCalendar _ start end = [i|
 \\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar, title label font=\\tiny]{${start}}{${end}}{year, month=shortname} \\ganttnewline 
 |]
-- \\gantttitlelist{1,...,$numPeriods$}{1} \\ganttnewline 

dayOfWeek :: Day -> Int
dayOfWeek d = let (_, _, n) = toWeekDate d in n

formatGrid :: Gantt ->  String
formatGrid g = case (outSize g) of
                 Daily -> let offset = (-) 7 $ dayOfWeek (start g) in [i|%%%% formatGrid Daily 
  vgrid={*${offset}{green, dashed},*1{blue, solid},*${7 - offset -1}{green, dashed},},
  milestone height=.75,
  milestone top shift=.125,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 Weekly -> let offset = (-) 7 $ dayOfWeek (start g) in [i|%%%% formatGrid Weekly
  vgrid={*${offset}{white},*1{blue, solid},*${7 - offset -1}{white},},
  x unit=1pt,
  milestone height=.75,
  milestone top shift=.125,
  milestone left shift=-2,
  milestone right shift=2,
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

calcPeriods :: Int -> Reader Gantt Int
calcPeriods dur = do
  g <- ask
  end_day <- endToDay dur
  let p = inSize g
      st_day = start g
  return $ case p of
      Daily     -> fromIntegral $ (+) 1 $ diffDays end_day st_day
      Weekly    -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 7
      Quarterly -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365 * 4
      Yearly    -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365
      otherwise -> ceiling $ (fromIntegral (diffDays end_day st_day)) / 365 * 12

calcEnd :: Day -> Reader Gantt Int
calcEnd day = do
  g <- ask
  return $ case (inSize g) of
             Daily  -> fromIntegral $ (+) 1 $  diffDays day (start g)
             Weekly -> round $ (fromIntegral (diffDays day (start g))) / 7
             Quarterly -> let (y, m, _) = toGregorian day
                              (st_y, _, _) = toGregorian (start g)
                          in ceiling $ (fromIntegral ((((fromIntegral y) - (fromIntegral st_y)) * 12) + m)) / 3
             Yearly -> let (y, m, _) = toGregorian day
                           (st_y, _, _) = toGregorian (start g)
                       in (fromIntegral y) - (fromIntegral st_y)
             otherwise -> let (y, m, _) = toGregorian day
                              (st_y, st_m, _) = toGregorian (start g)
                          in (((fromIntegral y) - (fromIntegral st_y)) * 12) + (m - st_m + 1)
                                      
calcStart :: Day -> Reader Gantt Int
calcStart day = do
  g <- ask
  e <- calcEnd day 
  let p = inSize g
      s = start g
  return $ case p of
             Daily -> e 
             Weekly -> (+) 1 $ round  $ (fromIntegral $ diffDays day s) / 7
             otherwise -> e


-- convert a chart start offset into a Day.  The start period is
-- actually the *end* of the previous period.
startToDay :: Int -> Reader Gantt Day
startToDay offset = do
  g <- ask
  return $ let offset' = toInteger (offset) in
           case (inSize g) of
             Daily -> addDays offset' (start g) 
             Weekly -> addDays (toInteger (offset' * 7) + 0) (start g)
             Quarterly -> addGregorianMonthsClip (offset' * 3) (start g)
             Yearly -> addGregorianMonthsClip (offset' * 12) (start g)
             otherwise -> addGregorianMonthsClip offset' (start g)

endOfMonth :: Day -> Day
endOfMonth day = let (y, m, _) = toGregorian day 
                 in fromGregorian y m $ gregorianMonthLength y m -- move to end of month; months w. less than 31 days get correct last day.

-- Convert a chart end offset into a Day.  The calculated date has to
-- be at the *end* of the period (for example, 28 Feb not 1 Feb).
endToDay :: Int -> Reader Gantt Day
endToDay offset = do
  g <- ask
  let p = inSize g
      st = start g
      offset' = toInteger (offset - 1) 
  return $ case p of
             Daily -> addDays offset' st -- no adjustment necessary?
             Weekly -> addDays ((offset' * 7) + 6) st
             Quarterly -> endOfMonth $ addGregorianMonthsClip (toInteger (offset * 3) - 1) st
             Yearly -> endOfMonth $ addGregorianMonthsClip (toInteger (offset * 12) - 1) st
             otherwise -> endOfMonth $ addGregorianMonthsClip offset' st -- Monthly is default

printGantt :: Gantt -> ST.StringTemplate String -> Handle -> IO ()
printGantt g tmpl h = do
  when (verbose g) $ do
    putStrLn "--- gantt ---"
    putStrLn $ show $ g
  let end = dur g :: Int
  let st = 1 :: Int
  let end_date = runReader (endToDay $ dur g) g
  let body = runReader formatGantt g 
  when (verbose g) $ do
    putStrLn "--- body ---"
    putStrLn $ body
    putStrLn "--- ------ ---"
  hPutStrLn h $ ST.toString 
                $ (ST.setManyAttrib $ filter (\(k, v) -> length v > 0) $ showEm g)
                $ (ST.setAttribute "vgrid" $ formatGrid g)
                $ (ST.setAttribute "calendar" $ formatCalendar (outSize g) (start g) end_date)
                $ (ST.setAttribute "numPeriods" $ runReader (calcPeriods $ dur g) g)
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
defaultGantt :: Gantt
defaultGantt = Gantt { 
                 start  = def            &= help "Project start date"
               , dur    = def            &= help "Project duration (months)"
               , inSize = def            &= help "Input period size (default: monthly)" 
               , outSize = enum [ DefaultPeriod
                               , Monthly &= help "Output report Monthly (default)"
                               , Daily   &= help "Output report Daily"
                               , Weekly  &= help "Output report Weekly"
                               , Quarterly &= help "Output report Quarterly"
                               , Yearly  &= help "Output report Yearly"
                               ]
                                             
               , entries = def           &= ignore
               , msg     = def           &= ignore
               , today   = def           &= ignore
               -- Command line only options.
               , font = def              &= help "Typeface for printed chart"
               , standalone = False      &= help "Generate standlone latex file"
               , markToday = False       &= help "Show today's date as 'today'"
               , outfile = "stdout"      &= help "Output file"
               , verbose = False         &= help "Print diagnostics as well"
               , file   = "test.gantt"   &= args &= typFile 
               , template = def          &= help "Template for standalone output"
               , chartopts = def         &= help "Options for \\pgfganttchart"
               }
       &= summary "Budget calculater v0.1, (C) 2015 John Noll"
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

main :: IO ()
main = do
  cfg <- cmdArgs defaultGantt
  todays_date <- getCurrentTime >>= return . utctDay 

  when (verbose cfg) $ do
      putStrLn "--- opts ---"
      putStrLn $ show $ cfg

  t <- getTemplate (template cfg)
  c <- readFile (file cfg)

  case parseGantt cfg { today = if markToday cfg then todays_date else today cfg } c of
    Left e -> putStrLn $ show $ e
    Right g -> if (outfile cfg) == "stdout" then printGantt g (ST.newSTMP t) stdout else 
                   case takeExtension (outfile cfg) of
                     ".pdf" -> makePDF g t (outfile cfg) 
                     ".png" -> let pdfFile = (takeBaseName (outfile cfg)) <.> "pdf" in
                               (makePDF g t $ pdfFile) >>
                               -- the density is high so image can be resized without pixelating.
                               (system $ "convert -density 1200 -quality 100 " ++ pdfFile ++ " " ++ (outfile cfg)) >>
                               return ()
                     otherwise -> (openFile (outfile cfg) WriteMode >>= (\h ->
                                   printGantt g (ST.newSTMP t) h >> hClose h))

