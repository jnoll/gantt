module DateRange where
import Parse
import Control.Monad.Reader
import Data.Time.Calendar (addDays, diffDays, addGregorianMonthsClip, addGregorianMonthsRollOver, addGregorianYearsRollOver, fromGregorian, toGregorian, gregorianMonthLength,)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)

-- 1 for Monday, 7 for Sunday
dayOfWeek :: Day -> Int
dayOfWeek d = let (_, _, n) = toWeekDate d in n

data Clipped = StartClipped | EndClipped | BothClipped | NeitherClipped | UhOh

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
--after a b = if diffDays a b >= 0 then True else False
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
