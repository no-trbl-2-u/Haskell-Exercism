module Acronym (abbreviate) where

import Data.List.Split(splitOn, linesBy)
import Data.Char(isUpper, toUpper)
import Text.Casing(unIdentifier, fromHumps)

allCaps :: String
allCaps = "GNU Image Manipulation Program" -- CCW

ruby :: String
ruby = "ruby on rails"

flattenAcro :: String -> String
flattenAcro xs
  | all isUpper xs == True = [head xs]
  | otherwise              = xs

removeAcros :: String -> String
removeAcros xs = concatMap flattenAcro (words xs)

abbreviate :: String -> String
abbreviate xs  = map (toUpper . head) finalSet
  where
    finalSet = concatMap words (unIdentifier $ fromHumps $ removeAcros  xs)



-- isHumpedCase --> has lowerCase next to Uppercase
-- isAcronym    --> has a word of all caps
-- isDashCasing --> has a word with a hyphen-casing

-- TODO -> Either:
-- -- a) Modularize all the specific acro makers, make some predicates, make giant switch/PM statement
-- -- b) Stay on this road and figure out how to prevent removeAcros from folding ""ALL LOWER CASE!!""
-- -- -- 1) Maybe add some if/then logic to removeAcros