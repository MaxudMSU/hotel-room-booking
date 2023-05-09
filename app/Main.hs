module Main (main) where

import Book

main :: IO ()
main = do
    putStrLn "> To start booking process type file with hotel info."
    filename <- getLine
    file <- readFile filename
    let x = transformFile file
    case x of
        Left error -> putStrLn error
        Right (x, y) -> do
            let (rooms, hotel) = (x, y)
            putStrLn "********************************"
            putStrLn $ "> Hello! Glad to see you in our hotel " ++ hotelName hotel ++ "!"
            putStrLn "> Commands: \n/start - to start booking a room,\n/change - to change booking parameteres,\n\
            \/cancel - to stop booking process,\n/exit - to exit from program."
            putStrLn "********************************"
            run rooms hotel filename