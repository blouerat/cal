module Cal where

import Data.List
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Control.Monad

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Enum, Bounded)
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Read, Enum, Bounded)
type Year = Integer
type DayOfMonth = Int

currentMonthCal :: IO String
currentMonthCal = do
  currentTime <- getCurrentTime
  (y, m, _) <- return . toGregorian . utctDay $ currentTime
  return . unlines . (monthCal True y) $ toEnum (m - 1)

yearCal :: Year -> String
yearCal y = unlines $ (center 60 $ show y) : "" : (concatMap mergeMonths $ grouped 3 . map (monthCal False y) $ months)
  where months :: [Month]
        months = enumFrom . toEnum $ 0
        mergeMonths :: [[String]] -> [String]
        mergeMonths = foldl1 (zipWith ((++) . (++ "  ")))
        yearLine :: [String] -> String
        yearLine = intercalate "  "

monthCal :: Bool -> Year -> Month -> [String]
monthCal displayYear year month = header : dayNames : monthDays year month
  where header = center 20 $ if displayYear then show month ++ " " ++ show year else show month

dayNames :: String
dayNames = intercalate " " $ map (take 2 . show) days
  where days :: [DayOfWeek]
        days = enumFrom . toEnum $ 0

monthDays :: Year -> Month -> [String]
monthDays y m = map (intercalate " ") $ grouped 7 $ marginLeft ++ days ++ marginRight
  where marginLeft = replicate (fromEnum $ firstWeekDay y m) "  "
        marginRight = replicate (42 - (length marginLeft + lastDay y m)) "  "
        days = map showDayOfMonth [1..lastDay y m]

lastDay :: Year -> Month -> DayOfMonth
lastDay y m = day
  where (_, _, day) = toGregorian $ fromGregorian y (fromEnum m + 1) 31

firstWeekDay :: Year -> Month -> DayOfWeek
firstWeekDay y m = dayOfWeek day
  where (_, _, day) = toWeekDate $ fromGregorian y (fromEnum m + 1) 1
        dayOfWeek :: Int -> DayOfWeek
        dayOfWeek 7 = Sunday
        dayOfWeek i = toEnum i

showDayOfMonth :: DayOfMonth -> String
showDayOfMonth i
  | i < 10    = ' ' : show i
  | otherwise = show i

center :: Int -> String -> String
center width str = replicate marginLeft ' ' ++ str ++ replicate marginRight ' '
  where totalMargin = max 0 (width - length str)
        marginLeft = div totalMargin 2
        marginRight = totalMargin - marginLeft

grouped :: Int -> [a] -> [[a]]
grouped i as = case splitAt i as of ([], _) -> []
                                    (p, tl) -> p : grouped i tl