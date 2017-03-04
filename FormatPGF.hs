-- following for 'here' docs
{-# LANGUAGE QuasiQuotes #-} 
module FormatPGF (formatPGF) where 
import Parse
import DateRange
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Time.Calendar (toGregorian)
import Data.String.Here (i)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Text.Printf (printf)


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
    let color = "white" in 
    msInRange d >>= formatMilestone n color

formatEntry (SlippedDeliverable n due due') = formatEntry (SlippedMilestone n due due')



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
2 \\gantttitlecalendar*[time slot format=isodate-yearmonth, compress calendar, title label font=\\tiny]{${start}}{${end}}{year, month=letter} \\ganttnewline 
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


grid :: Day -> String -> (String, String)
grid d color = let offset = (-) 7 $ dayOfWeek d  
                   style = "solid" in 
            case offset - 1 of
            -1 -> ([i||], [i|,*6{${color}, ${style}}|])
            0 -> ([i||], [i|,*6{${color}, ${style}}|])
            6 -> ([i|*6{${color}, ${style}},|], [i||])
            otherwise -> ([i|*${offset - 1}{${color}, ${style}},|], [i|,*${7 - offset}{${color}, ${style}}|])
                         
formatGrid :: Gantt ->  String
formatGrid g = case (outSize g) of
                 Daily -> let (preg, postg) = grid (windowStart g) "black"
                              labelw = labelWidth g
                          in [i|%%%% formatGrid Daily 
  vgrid={${preg}*1{blue, solid}${postg}},
  milestone height=.75,
  milestone top shift=.125,
  milestone label node/.append style={left=-.5em, align=left, text width=${labelw}em},
  %%%% /formatGrid|]
                 Weekly -> let (preg, postg) = grid (windowStart g) "white"
                               labelw = labelWidth g
                           in [i|%%%% formatGrid Weekly
  vgrid={${preg}*1{blue, solid}${postg}},
  x unit=1pt,
  milestone height=.75,
  milestone top shift=.125,
  milestone left shift=-2,
  milestone right shift=2,
  milestone label node/.append style={left=-.5em, align=left, text width=${labelw}em},
  %%%% /formatGrid|]
                 Monthly -> let (preg, postg) = grid (windowStart g) "white"
                                labelw = labelWidth g
                            in [i|%%%% formatGrid Monthly
  vgrid={${preg}*{6}{white},{green, dotted},*{6}{white},{green, dotted},*{6}{white},{blue, solid}${postg}},
  x unit=.5pt,                  % try to get months to reflect actual dates
  milestone height=.75,
  milestone top shift=.125,
  milestone left shift=-4,
  milestone right shift=4,
  milestone label node/.append style={left=-.5em, align=left, text width=${labelw}em},
  %%%% /formatGrid|]
                 Quarterly -> let labelw = labelWidth g
                              in [i|%%%% formatGrid Quarterly
  compress calendar, 
  vgrid={*2{white},*1{blue, solid}},
  x unit=.67em,
  milestone height=.75,
  milestone top shift=.125,
%% doesn't work  milestone left shift=-.5,
%% doesn't work  milestone right shift=.5,
  milestone label node/.append style={left=-.5em, align=left, text width=9em},
  %%%% /formatGrid|]
                 otherwise -> let labelw = labelWidth g
                              in [i|%%%% formatGrid default
  compress calendar, 
  vgrid={*2{green, dashed},*1{blue, solid}},
  milestone height=.75,
  milestone top shift=.125,
  milestone label node/.append style={left=-.5em, align=left, text width=${labelw}em}, 
  %%%% /formatGrid|]


formatToday :: Day -> String
formatToday d = let ds = formatTime defaultTimeLocale "%F" d in 
                if d == defaultDay then "%%%% formatToday: today is def(ault)" 
                else [i| today=${ds}, today rule/.style={draw=green, ultra thick}, |]



formatBody :: Reader Gantt String
formatBody = asks (\g -> entries g) >>= (mapM formatEntry) >>= (\ls -> return $ intercalate "\n" ls )

formatPGF :: Gantt -> Day -> [(String, String)]
formatPGF g  end_date = 
  let body = runReader formatBody g 
  in [ ("vgrid", formatGrid g)
     , ("calendar",  formatCalendar (outSize g) (windowStart g) end_date)
     , ("numPeriods", show $ runReader (calcPeriods $ windowDur g) g)
     , ("todayDate", formatToday (today g))
     , ("end",  (formatTime defaultTimeLocale "%F" $ end_date)) -- end date, for calendar lines in monthly
     , ("body", body) -- actual chart elements
     ]
--      in ST.toString 
--                $ (ST.setManyAttrib attr)
--                $ (ST.setAttribute "vgrid" $ formatGrid g)
--                $ (ST.setAttribute "calendar" $ formatCalendar (outSize g) (windowStart g) end_date)
--                $ (ST.setAttribute "numPeriods" $ runReader (calcPeriods $ windowDur g) g)
--                $ (ST.setAttribute "todayDate" $ formatToday (today g))
--                $ ST.setAttribute "end" (formatTime defaultTimeLocale "%F" $ end_date) -- end date, for calendar lines in monthly
--                $ ST.setAttribute "body" body -- actual chart elements
--                  tmpl
