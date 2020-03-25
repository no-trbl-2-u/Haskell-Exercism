module LeapYear where

isLeapYear :: Int -> Bool
isLeapYear year
  | yearIsDivBy 400                         = True
  | yearIsDivBy 4 && not (yearIsDivBy 100)  = True
  | otherwise                               = False
    where
      yearIsDivBy = (== 0) . (year `mod`)
