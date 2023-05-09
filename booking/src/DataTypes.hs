module DataTypes( 
Day (..),
RoomType (..),
Room (..),
Hotel (..),
Request (..)
) where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data RoomType = Single | Double deriving (Show, Read, Eq)

data Room = Room {
  roomId       :: Int,
  roomType     :: RoomType,
  roomPrice    :: Int,
  roomBookWeek :: [Bool]
} | EmptyRoom deriving (Show, Read, Eq)

data Hotel = Hotel {
  hotelName :: String,
  hotelTypes :: [RoomType],
  hotelRoomAmount :: Int,
  hotelBusy :: [[Int]]
} deriving (Show, Read)

data Request = Request {
  roomtype :: Maybe RoomType,
  days :: [Bool]
} | EmptyRequest deriving (Show, Read)