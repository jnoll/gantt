{-# LANGUAGE DeriveDataTypeable #-}
import DateRange 
import Parse
import FormatPGF (formatPGF)
import FormatMarkdown (formatMarkdown)

import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.Reader
import Data.Data (constrFields, toConstr, gmapQ, cast)
import Data.List
import Data.String.Utils (replace)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays, diffDays, addGregorianMonthsClip, addGregorianMonthsRollOver, addGregorianYearsRollOver, fromGregorian, toGregorian, gregorianMonthLength,)
import Paths_gantt (getDataFileName)
import System.Console.CmdArgs
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, takeBaseName, (</>), (<.>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process (system)
import Text.StringTemplate as ST


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

printPGF :: Gantt -> ST.StringTemplate String -> Day -> Handle -> IO ()
printPGF g tmpl end h = do 
  let attr = filter (\(k, v) -> length v > 0) $ showEm g
  hPutStrLn h $ ST.toString $ ST.setManyAttrib (attr ++ formatPGF g end) tmpl

printMarkdown :: Gantt -> ST.StringTemplate String -> Handle -> IO ()
printMarkdown g tmpl h = do
  let body = formatMarkdown g 
  hPutStrLn h $ ST.toString $ ST.setAttribute "body" body tmpl


printGantt :: Gantt -> ST.StringTemplate String -> Handle -> IO ()
printGantt g tmpl h = do
  when (verbose g) $ do
    putStrLn "--- gantt ---"
    putStrLn $ show $ g
  let end = dur g :: Int
  let st = 1 :: Int
--  let end_date = runReader (endToDay $ addDays (windowDur g) (windowStart g)) g
  let end_date = offsetToDay (windowStart g) (toInteger $ (windowDur g) - 1) (inSize g)
  case charttype g of
    Markdown -> printMarkdown g tmpl h
    otherwise -> printPGF g tmpl end_date h


-- Command line parsing and processing --------------------------------------------------------------------------



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
    , opt_labelWidth :: Int          -- with of task, group, milestone labels
    , opt_standalone :: Bool
    , opt_markToday :: Bool 
    , opt_outfile :: FilePath
    , opt_verbose :: Bool
    , opt_file    :: FilePath
    , opt_template :: FilePath
    , opt_chartopts :: String
    , opt_charttype :: ChartType
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
               , opt_labelWidth = def        &= help "Width (in ems) of group, task, milestone label column" &= explicit &= name "labelwidth"
               , opt_standalone = True       &= help "Generate standlone latex file" &= explicit &= name "standalone"
               , opt_markToday = False       &= help "Show today's date as 'today'" &= explicit &= name "today"
               , opt_outfile = "stdout"      &= help "Output file" &= name "outfile" 
               , opt_verbose = False         &= help "Print diagnostics as well" &= explicit &= name "verbose"
               , opt_file   = "test.gantt"   &= args &= typFile 
               , opt_template = def          &= help "Template for standalone output" &= explicit &= name "template"
               , opt_chartopts = def         &= help "Options for \\pgfganttchart" &= explicit &= name "chartopts"
               , opt_charttype = def         &= help "Chart type: Gantt (default) or Markdown" &= explicit &= name "type"
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

getTemplate :: Options -> IO String
getTemplate opts = do
  if opt_template opts == "" then
      if opt_charttype opts == Markdown then
          (getDataFileName $ "templates" </> "memo.st") >>= (\d -> readFile d)
      else
          (getDataFileName $ "templates" </> "gantt.st") >>= (\d -> readFile d)
  else readFile $ opt_template opts


ifDef :: (Eq a, Default a)  => a -> a -> a
ifDef x y = if x == def then y else x

main :: IO ()
main = do
  args <- cmdArgs defaultOptions
  todays_date <- getCurrentTime >>= return . utctDay 

  when (opt_verbose args) $ do
      putStrLn "--- args ---"
      putStrLn $ show $ args


  let cfg = defaultGantt 
  c <- readFile (opt_file args)
  case parseGantt cfg {      
             start = if (opt_start args) /= def then parseDate (opt_start args)
                     else (start cfg)
           , dur   = ifDef (opt_dur args)   (dur cfg)
           , windowStart = if (opt_windowStart args) /= def then parseDate (opt_windowStart args)
                           else if (opt_start args) /= def then parseDate (opt_start args) else (start cfg)
           , windowDur = ifDef (opt_windowDur args) (windowDur cfg)
           , inSize = ifDef (opt_inSize args) (inSize cfg)
           , outSize = ifDef (opt_outSize args) (outSize cfg)
           , today = if (opt_markToday args) then todays_date else (today cfg)
           , font  = opt_font args
           , labelWidth = ifDef (opt_labelWidth args) (labelWidth cfg)
           , standalone = opt_standalone args
           , verbose = opt_verbose args
           , file = opt_file args
           , template = opt_template args
           , chartopts = opt_chartopts args
           , charttype = ifDef (opt_charttype args) (charttype cfg)
           } c of
    Left e -> putStrLn $ show $ e
    Right g' -> do
         t <- getTemplate args


         let g = g' { windowStart  = if diffDays (windowStart g') (fromGregorian 1970 1 1) == 0 then start g' else windowStart g'
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

