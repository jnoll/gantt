{-# LANGUAGE DeriveDataTypeable #-}
-- following for 'here' docs
{-# LANGUAGE QuasiQuotes #-} 
import Text.Printf (printf)
import Control.Monad.Error
import Data.Data (constrFields, toConstr, gmapQ, cast)
import Data.List
--import Data.Maybe (fromMaybe)
import Data.String.Here (i)
import Data.String.Utils (replace)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Calendar (addGregorianMonthsClip)
import System.Console.CmdArgs
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, takeBaseName, (</>), (<.>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process (system)
import Text.StringTemplate as ST
import Parse

replChar :: Char -> Char
replChar ' ' = '-'
replChar '.' = '-'
replChar c = c
itemName :: String -> String
itemName s = map replChar s

formatLink :: String -> Int -> Int -> Int -> Int -> String
formatLink label s e s' e' | s < s' && e < e' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} \\ganttnewline 
                                                    \\ganttlink[link type=slipend]{${label}}{${label}r} \\ganttnewline |] 
                           | s < s' = [i| \\ganttlink[link type=slipstart]{${label}}{${label}r} \\ganttnewline |]
                           | e < e' = [i| \\ganttlink[link type=slipend]{${label}}{${label}r}  \\ganttnewline |] 
                           | True   = "\\ganttnewline"

formatEntry :: ChartLine -> String
formatEntry (Group n s e)   = printf "\\ganttgroup{%s}{%d}{%d}\t\\ganttnewline" n s e
formatEntry (SlippedGroup n s e s' e')   = let label = itemName n in 
                                     [i| \\ganttgroup[name=${label}, group/.append style={draw=black,fill=white}]{${n}}{${s}}{${e}} 
                                         \\ganttgroup[name=${label}r]{${n}}{${s'}}{${e'}} \\ganttnewline 
                                      |] ++ formatLink label s e s' e' 

formatEntry (Task n s e)    = printf "\\ganttbar{%s}{%d}{%d}\t\\ganttnewline" n s e
formatEntry (SlippedTask n s e s' e')  = let label = itemName n in
                                         [i| \\ganttbar[name=${label}, bar/.append style={draw=black, fill=white}]{${n}}{${s}}{${e}}\t
                                             \\ganttbar[name=${label}r]{${n}'}{${s'}}{${e'}}\t\\ganttnewline 
                                          |] ++ formatLink label s e s' e' 

formatEntry (Milestone n d) = printf "\\ganttmilestone{%s}{%d}\t\\ganttnewline" n d
formatEntry (SlippedMilestone n d d') = let label = itemName n in 
                                        [i| \\ganttmilestone[name=${label}, milestone/.append style={draw=black, fill=white}]{${n}}{${d}} 
                                            \\ganttmilestone[name=${label}r]{${n}}{${d'}} \\ganttnewline 
                                            \\ganttlink[link type=slipms]{${label}}{${label}r}\t\\ganttnewline 
                                         |] 
formatGantt :: Gantt -> String
formatGantt g = 
    intercalate "\n" $ map formatEntry (entries g)

printGantt :: Gantt -> ST.StringTemplate String -> Handle -> IO ()
printGantt g tmpl h = do
  when (verbose g) $ do
    putStrLn "--- gantt ---"
    putStrLn $ show $ g

  let end = dur g :: Int
  let st = 1 :: Int
  let end_date = addGregorianMonthsClip (fromIntegral ((dur g) - 1)) (start g)
  let body = formatGantt g 
  when (verbose g) $ do
    putStrLn "--- body ---"
    putStrLn $ body
    putStrLn "--- ------ ---"
  hPutStrLn h $ ST.toString $ (ST.setManyAttrib $ filter (\x -> x /= def) $ showEm g)  $ ST.setAttribute "end" (formatTime defaultTimeLocale "%Y-%m" $ end_date) $ ST.setAttribute "body" body tmpl



-- Command line parsing and processing --------------------------------------------------------------------------

-- Convert a record to a list.
showEm :: (Data a) => a -> [(String, String)]
showEm x = zip (constrFields $ toConstr x) (gmapQ (showIt [showPeriod, showBool, showDouble, showInt, showDate, showStr]) x)

-- Using a list of "showing" functions, find a representation of d.
showIt :: (Data a) => [(a -> Either String a)] -> a -> String
showIt ss d = either id (\_ -> "XXX Nope XXX") (foldl (>>=) (return d) ss)

-- Show various primitve types.
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
               , period = enum [ Monthly &= help "Report monthly"
                               , Quarterly &= help "Report quarterly"
                               , Yearly  &= help "Report yearly"
                               ]
               , entries = def           &= ignore
               , msg     = def           &= ignore
               -- Command line only options.
               , font = def              &= help "Typeface for printed chart"
               , standalone = False      &= help "Generate standlone latex file"
               , outfile = "stdout"      &= help "Output file"
               , verbose = False         &= help "Print diagnostics as well"
               , file   = "test.gantt"   &= args &= typFile 
               , template = "templates/gantt.st" &= help "Template for standalone output"
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
main :: IO ()
main = do
  cfg <- cmdArgs defaultGantt

  when (verbose cfg) $ do
      putStrLn "--- opts ---"
      putStrLn $ show $ cfg

  t <- readFile (template cfg)
  c <- readFile (file cfg)
  case parseGantt cfg c of
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

