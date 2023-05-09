module TypesHandle where

{-МОДУЛЬ ДЛЯ РАБОТЫ С ВНУТРЕННИМ ПРЕДСТАВЛЕНИЕМ ДАННЫХ-}
import DataTypes
import Constants
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.List (intercalate)

--ФУНКЦИИ ДЛЯ РАБОТЫ С ОСНОВНЫМ ФАЙЛОМ ОТЕЛЯ
--перевод файла сразу в два типа: Room и Hotel
transformFile :: String -> Either String ([Room], Hotel)
transformFile file = case parseFile $ lines file of
    Left err -> Left err
    Right (name, rawRooms) -> if EmptyRoom `elem` rooms then Left "Error in the file." else Right (rooms, initializeHotel name rooms)
     where rooms = initializeRooms rawRooms

parseFile :: [String] -> Either String (String, [String])
parseFile [] = Left "Error: Empty file."
parseFile [_] = Left "Error: Empty list of rooms"
parseFile (name : rawRooms) = Right (name, rawRooms)

--инициализация типа данных Hotel по его имени и списку комнат
initializeHotel :: String -> [Room] -> Hotel
initializeHotel name rooms = Hotel name [] (length rooms) $ weekBusy rooms

--инициализация типа данных Room по списку строк-комнат
initializeRooms :: [String] -> [Room]
initializeRooms = map stringToRoom

--создание списка занятости отеля на неделю (в виде списка списков номеров комнат) по списке комнат.
weekBusy :: [Room] -> [[Int]]
weekBusy rooms = weekBusyHelp rooms $ replicate 7 []

--вспомогательная для weekBusy
weekBusyHelp :: [Room] -> [[Int]] -> [[Int]]
weekBusyHelp [] result = result
weekBusyHelp (r:rs) initt = weekBusyHelp rs $ addToBusy r initt

--обновление списка занятости
addToBusy :: Room -> [[Int]] -> [[Int]]
addToBusy (Room rid _ _ bw) bb = zipWith (\a b -> if b then rid : a else a) bb bw
addToBusy _ _ = []

--перевод строки, соответствующей номеру отеля из файла, в тип данных Room
stringToRoom :: String -> Room
stringToRoom str = case words str of
    (i:t:p:rest) -> case (readMaybe i :: Maybe Int, readMaybe t :: Maybe RoomType, readMaybe p :: Maybe Int) of
        (Just rid, Just rtype, Just rprice) ->  Room rid rtype rprice (transformDaysToBool rest weekS1)
        _ -> EmptyRoom
    _ -> EmptyRoom

--перевод списка дней недели из списка строк/дней недели в формате Day в список True/False 
transformDaysToBool :: (Eq a) => [a] -> [a] -> [Bool]
transformDaysToBool days = map (`elem` days)

--перевод комнаты в формате Room обратно в строку
roomToString :: Room -> String
roomToString EmptyRoom = ""
roomToString (Room i t p bw) = unwords [show i, show t, show p] ++ " " ++ transformDaysFromBool bw ++ "\n"

--перевод списка True/False, обозначающего занятость комнаты на неделю, обратно в строку
transformDaysFromBool :: [Bool] -> String
transformDaysFromBool bw = unwords $ filter (not.null) $ zipWith (\a b -> if a then b else "") bw weekS1

--ФУНКЦИИ ДЛЯ РАБОТЫ С ВНУТРЕННИМ ПРЕДСТАВЛЕНИЕМ ОТЕЛЯ
--обновление одной комнаты в соответствии с запросом на бронирование
updateRoom :: Room -> Request -> Room
updateRoom (Room x y z bw1) (Request _ bw2) = Room x y z $ zipWith (||) bw1 bw2
updateRoom _ _ = EmptyRoom

--обновлени списка комнат по одной обновленной комнате
updateRooms :: [Room] -> Room -> [Room]
updateRooms (r1@(Room x _ _ _):rooms) r2@(Room y _ _ _) | x == y    = r2 : rooms
                                                        | otherwise = r1 : updateRooms rooms r2
updateRooms _ _ = []

--обновление внутренного представления отеля по одной обновленной комнате
updateHotel :: Hotel -> Room -> Hotel
updateHotel (Hotel x y z wb) room = Hotel x y z $ addToBusy room wb

--нахождение списка комнат, подходящих под запрос
findRooms :: Request -> [Room] -> [Room]
findRooms _ [] = []
findRooms req@(Request Nothing bw1) (r@(Room _ _ _ bw2):rs) | isRoomBusy bw1 bw2 = r : findRooms req rs
                                                             | otherwise = findRooms req rs
findRooms req@(Request (Just t1) bw1) (r@(Room _ t2 _ bw2):rs) | t1 == t2 && isRoomBusy bw1 bw2 = r : findRooms req rs
                                                               | otherwise = findRooms req rs
findRooms _ _ = []

--проверка занятности комнаты
isRoomBusy :: [Bool] -> [Bool] -> Bool
isRoomBusy bw1 bw2 = not $ or $ zipWith (&&) bw1 bw2

--преобразование комнаты в формате Room в строку, которая потом будет выведена в качестве доступной для брони
showRoom :: Room -> String
showRoom (Room rid t price _) = show t ++ " room #" ++ show rid ++ " for " ++ show price ++ " rubles per night."
showRoom EmptyRoom = ""

--преобразование списка комнат в строку для печати
showRooms :: [Room] -> String
showRooms [] = []
showRooms (room:rs) = showRoom room ++ "\n" ++ showRooms rs

showBusy :: [[Int]] -> String
showBusy rs = intercalate "\n" $ showBusyHelp rs weekS2

showBusyHelp :: [[Int]] -> [String] -> [String]
showBusyHelp [] _ = []
showBusyHelp (rs:rrs) (d1:ds) = (d1 ++ ": " ++ showBusyDay rs) : showBusyHelp rrs ds
showBusyHelp _ _ = []

showBusyDay :: [Int] -> String
showBusyDay [] = ""
showBusyDay ints = intercalate ", " (map show ints)

--ФУНКЦИИ ДЛЯ РАБОТЫ С ВНУТРЕННИМ ПРЕДСТАВЛЕНИЕМ ЗАПРОСА
--преобразование запроса из списка строк в тип данных Request
--TODO: рассмотреть все паттерны
transform :: [String] -> Request
transform req@[_, _] = Request Nothing $ transformDaysIntervalToBool req
transform (d1:d2:[t]) = Request (Just tt) $ transformDaysIntervalToBool [d1,d2]
                        where tt = read t :: RoomType
transform _ = EmptyRequest

--преобразование интервала дней недели (в формате строки) в список True/False
--TODO: добавить последний паттерн (хз зачем)
transformDaysIntervalToBool :: [String] -> [Bool]
transformDaysIntervalToBool [] = transformDaysToBool [] week
transformDaysIntervalToBool [d1,d2] = transformDaysToBool [td1 .. td2] week
                                where td1 = transformOneDay d1 assocWeekSD
                                      td2 = transformOneDay d2 assocWeekSD
transformDaysIntervalToBool _ = []             

--преобразование дня недели из строки в Day и наоборот
transformOneDay :: (Eq a) => a -> [(a,b)] -> b
transformOneDay day list = fromJust $ lookup day list