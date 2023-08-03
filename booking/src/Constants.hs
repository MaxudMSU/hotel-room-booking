module Constants 
( week,
weekS1,
weekS2,
weekS,
typeS,
assocWeekSD,
help
) where

import DataTypes

week :: [Day]
week = [Monday .. Sunday]

weekS2 :: [String]
weekS2 = map show week

weekS1 :: [String]
weekS1 = ["M","Tu","W","Th","F","Sa","Su"]

weekS :: [String]
weekS = ["M","Monday","Tu","Tuesday","W","Wednesday","Th","Thursday","F","Friday","Sa","Saturday","Su","Sunday"]

typeS :: [String]
typeS = ["Single", "Double"]

assocWeekSD :: [(String, Day)]
assocWeekSD = [("Monday",Monday),("Tuesday",Tuesday),("Wednesday",Wednesday),("Thursday",Thursday),("Friday",Friday),("Saturday",Saturday),("Sunday",Sunday),("M",Monday),("Tu",Tuesday),("W",Wednesday),("Th",Thursday),("F",Friday),("Sa",Saturday),("Su",Sunday)]

help :: [[String]]
help = [weekS, weekS, typeS]