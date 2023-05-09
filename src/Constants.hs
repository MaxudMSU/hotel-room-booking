module Constants 
( week,
weekS1,
weekS2,
weekS,
typeS,
assocWeekSD,
requestPrompts,
help
) where

import DataTypes

week :: [Day]
week = [Monday .. Sunday]

weekS2 :: [String]
weekS2 = map show week

--список обозначений для дней недели в текстовом файле
weekS1 :: [String]
weekS1 = ["M","Tu","W","Th","F","Sa","Su"]

--список обозначений для дней недели, которые может ввести пользователь при бронировании
weekS :: [String]
weekS = ["M","Monday","Tu","Tuesday","W","Wednesday","Th","Thursday","F","Friday","Sa","Saturday","Su","Sunday"]

typeS :: [String]
typeS = ["Single", "Double"]

--ассоциативный список для перевода строкового представления дня недели (которое вводит пользователь) в отдельный тип данных
assocWeekSD :: [(String, Day)]
assocWeekSD = [("Monday",Monday),("Tuesday",Tuesday),("Wednesday",Wednesday),("Thursday",Thursday),("Friday",Friday),("Saturday",Saturday),("Sunday",Sunday),("M",Monday),("Tu",Tuesday),("W",Wednesday),("Th",Thursday),("F",Friday),("Sa",Saturday),("Su",Sunday)]

--список подсказок при получении запроса (последний элемент - костыль, без него не работало)
requestPrompts :: [String]
-- requestPrompts = ["Type of room: Single or Double?", "Check-in-date?", "Check-out-date?",""]
requestPrompts = ["Specify type of room", ""]

help :: [[String]]
help = [weekS, weekS, typeS]