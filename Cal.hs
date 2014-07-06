module Cal where

import Data.List
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Control.Monad

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Enum, Bounded)
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Enum, Bounded)
type Year = Integer
type DayOfMonth = Int

currentCal :: IO String
currentCal = do
  currentTime <- getCurrentTime
  (y, m, _) <- return . toGregorian . utctDay $ currentTime
  return . intercalate "\n" .monthCal y $ toEnum (m - 1)

monthCal :: Year -> Month -> [String]
monthCal y m = header : dayNames : monthDays y m
  where header = center 20 $ show m ++ " " ++ show y

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
  where totalMargin = max 0 (20 - length str)
        marginLeft = div totalMargin 2
        marginRight = totalMargin - marginLeft

grouped :: Int -> [a] -> [[a]]
grouped i as = case splitAt i as of ([], _) -> []
                                    (p, tl) -> p : grouped i tl