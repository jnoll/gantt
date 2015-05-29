{-# LANGUAGE DeriveDataTypeable #-}
import Text.Printf (printf)
import Control.Monad.Error
import Data.Data (constrFields, toConstr, gmapQ, cast)
import Data.List
--import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Calendar (addGregorianMonthsClip)
import System.Console.CmdArgs
import Text.StringTemplate as ST
import Parse

replChar :: Char -> Char
replChar ' ' = '-'
replChar '.' = '-'
replChar c = c
itemName :: String -> String
itemName s = map replChar s

formatEntry :: ChartLine -> String
formatEntry (Group n s e)   = printf "\\ganttgroup{%s}{%d}{%d}\t\\ganttnewline" n s e
formatEntry (Task n s e)    = printf "\\ganttbar{%s}{%d}{%d}\t\\ganttnewline" n s e
formatEntry (SlippedTask n s e s' e')  = let label = itemName n in
                                         intercalate "\n" $ (printf "\\ganttbar[name=%s]{%s}{%d}{%d}\t\\ganttnewline" label n s e) :
                                                         (printf "\\ganttbar[name=%sr]{%s'}{%d}{%d}\t\\ganttnewline" label n s' e') :
                                                         [(printf "\\ganttlink{%s}{%sr}\t\\ganttnewline" label label)]
formatEntry (Milestone n d) = printf "\\ganttmilestone{%s}{%d}\t\\ganttnewline" n d

formatGantt :: Gantt -> String
formatGantt g = 
    intercalate "\n" $ map formatEntry (entries g)

printGantt :: Gantt -> ST.StringTemplate String -> IO ()
printGantt g tmpl = do
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

    
  putStrLn $ ST.toString $ (ST.setManyAttrib $ filter (\x -> x /= def) $ showEm g)  $ ST.setAttribute "end" (formatTime defaultTimeLocale "%Y-%m" $ end_date) $ ST.setAttribute "body" body tmpl

--[ ("chartoptions", (chartopts g))
--                                           , ("font", (font g))
--                                           , ("dur", show (dur g))
--                                           , ("start", formatTime defaultTimeLocale "%Y-%m" $ (start g))
--                                           , ("end", formatTime defaultTimeLocale "%Y-%m" $ end_date)
--                                           , ("body", body)
--                                           ] tmpl


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
               , verbose = False         &= help "Print diagnostics as well"
               , file   = "test.gantt"   &= args &= typFile 
               , template = "templates/gantt.st" &= help "Template for standalone output"
               , chartopts = def         &= help "Options for \\pgfganttchart"
               }
       &= summary "Budget calculater v0.1, (C) 2015 John Noll"
       &= program "main"


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
    Right g -> printGantt g (ST.newSTMP t)
  
