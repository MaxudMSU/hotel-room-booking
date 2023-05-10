module Book
    ( run,
    transformFile,
    Hotel(..)
    ) where

import Data.Maybe (fromJust)
import Data.List (find)

import DataTypes
import Constants (help, typeS)
import TypesHandle

fullRequestHandle :: [Room] -> IO Request
fullRequestHandle rooms = do
    putStrLn "\n> Type your request below in form\n1.Check-in date.\n2.Check-out date.\n3.(optional) Type of room: Single or Double.\n\
    \For example: \"W Su Single\" or \"M Th\"."
    rawRequest <- getInitialRequest
    case rawRequest of
        [] -> return EmptyRequest
        _ -> processReq rooms $ transform rawRequest


getInitialRequest :: IO [String]
getInitialRequest = do
    putStr "\nYour input -> "
    rawreq <- getLine
    case rawreq of
        "/cancel" -> return []
        _ -> do
            let rr = words rawreq
                checkR = zipWith elem rr help
            case (and checkR, checkR) of
                (True, [_]) -> do
                    putStrLn "> Request is not complete. Please try again."
                    getInitialRequest
                (True, _) -> return rr
                _ -> do
                    putStrLn "> Wrong data. Please try again."
                    getInitialRequest

processReq :: [Room] -> Request -> IO Request
processReq rooms req = do
    let frooms = findRooms req rooms
    case frooms of
        [] -> do
            putStrLn "> There are no availible rooms at your request."
            fullRequestHandle rooms
        _ -> do
            case req of
                Request (Just _) _ -> do
                    putStrLn "=====Rooms======"
                    putStr $ showRooms frooms
                    putStrLn "> If you want to change your request, enter /change. If you don't press any key."
                    putStr "Your input -> "
                    key <- getLine
                    case key of
                        "/change" -> fullRequestHandle rooms
                        "/cancel" -> return EmptyRequest
                        _ -> do
                            putStrLn "> To book room type its number below"
                            return req
                _                   -> do
                    putStrLn "=====Rooms======"
                    putStr $ showRooms frooms
                    putStrLn "================"
                    putStrLn "> Specify type of room."
                    newReq <- getRequest req
                    case newReq of
                        EmptyRequest -> return EmptyRequest
                        _ -> processReq frooms newReq

getRequest :: Request -> IO Request
getRequest req@(Request _ days) = do
    putStr "Your input -> "
    rawreq <- getLine
    case rawreq of
        "/cancel" -> return EmptyRequest
        _ -> do
            if rawreq `elem` typeS
                then return (Request (Just (read rawreq :: RoomType)) days)
                else do
                    putStrLn "> Try again."
                    getRequest req
getRequest EmptyRequest = return EmptyRequest

book :: [Room] -> Request -> IO Room
book rooms req = do
    let nums = map (show . roomId) rooms
    putStr "Your input -> "
    num <- getLine
    case num of
        "/cancel" -> return EmptyRoom
        _ -> do
            if num `elem` nums
                then do
                    let n = read num :: Int
                        rtb = fromJust $ find (\(Room rid _ _ _) -> rid == n) rooms
                        ur = updateRoom rtb req
                    putStrLn "> You booked this room:"
                    putStrLn $ showRoom ur
                    return ur
                else do
                    putStrLn "> Try again."
                    book rooms req

run :: [Room] -> Hotel -> String-> IO ()
run rooms hotel fileHotel = do
    putStr "Your input -> "
    str <- getLine
    case str of
        "/start" -> do
            requestForBooking <- fullRequestHandle rooms
            case requestForBooking of
                EmptyRequest -> do 
                    putStrLn "Booking canceled."
                    run rooms hotel fileHotel
                _ -> do
                    bookedRoom <- book rooms requestForBooking
                    case bookedRoom of
                        EmptyRoom -> do
                            putStrLn "Booking canceled."
                            run rooms hotel fileHotel
                        _ -> do
                            let newRooms = updateRooms rooms bookedRoom
                                newHotel = updateHotel hotel bookedRoom
                            run newRooms newHotel fileHotel
        "/admin" -> do
            putStrLn "====All booked rooms===="
            putStrLn $ showBusy $ hotelBusy hotel
            putStrLn "========================"
            run rooms hotel fileHotel
        "/exit" -> do
            let newfile = concatMap roomToString rooms
            writeFile fileHotel $ hotelName hotel ++ "\n" ++ newfile
            putStrLn "See you again!"
        _ -> do
            putStrLn "Undefined command."
            run rooms hotel fileHotel