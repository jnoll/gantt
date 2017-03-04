module FormatMarkdown (formatMarkdown) where
import Parse
import DateRange
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Text.Printf (printf)


status :: Day -> Day -> Day -> String
status start end today =
    if before today start then "not started"
    else if after today end then "**late**"
         else "_in progress_"

replChar :: Char -> Char
replChar ' ' = '-'
replChar '.' = '-'
replChar c = c

itemName :: String -> String
itemName s = map replChar s

formatGroup :: String -> String -> Maybe (Day, Day, Clipped) -> Reader Gantt String
formatGroup nm fill r= 
    case r of 
      Nothing -> return $ formatOutOfRange nm
      Just (st, en, cl) -> return $ printf "\n### %s\n" nm

formatBar ::  String -> String -> Maybe (Day, Day, Clipped) -> Reader Gantt String
formatBar nm fill r = do
    g <- ask 
    case r of 
      Nothing -> return $ formatOutOfRange nm
      Just (st, en, _) -> return $ printf "#. %s: %s (due %s)\n" nm (status st en (today g)) (formatTime defaultTimeLocale "%F" st)
    
formatOutOfRange :: String -> String
formatOutOfRange label = printf "<!-- %s out of range -->\n" label

formatEntry :: ChartLine -> Reader Gantt String

formatEntry (Group n s e) = 
  let color = "cyan" in 
  dayRange s e >>= formatGroup n color
formatEntry (SlippedGroup nm st end st' end') = formatEntry (Group nm st' end' )

formatEntry (Task n s e) = 
    let color = "blue" in
    dayRange s e >>= formatBar n color 
formatEntry (SlippedTask nm st end st' end') = formatEntry (Task nm st' end')

formatEntry (Milestone n due) = return ""
formatEntry (SlippedMilestone nm d d') = return ""

formatEntry (Deliverable n d) = return ""

formatEntry (SlippedDeliverable n due due') = return ""

formatBody :: Reader Gantt String
formatBody = asks (\g -> entries g) >>= (mapM formatEntry) >>= (\ls -> return $ intercalate "\n" ls )

formatMarkdown :: Gantt -> String
formatMarkdown g = runReader formatBody g
