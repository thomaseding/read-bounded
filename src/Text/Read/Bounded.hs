module Text.Read.Bounded (
    BoundedRead(..),
    ReadBounded(..)
) where


import Data.Int
import Data.Word
import Text.Read (readMaybe)


data BoundedRead a
    = NoRead
    | ExactRead a
    | ClampedRead a
    deriving (Show, Read, Eq, Ord)


fromMaybe :: Maybe a -> BoundedRead a
fromMaybe = maybe NoRead ExactRead


data Clamped a
    = Exact a
    | Clamped a


clamp :: (Bounded a, Integral a) => Integer -> Clamped a
clamp x = if x < minInteger
    then Clamped minNum
    else if x > maxInteger
        then Clamped maxNum
        else Exact $ fromInteger x
    where
        minNum = minBound
        maxNum = maxBound
        minInteger = toInteger minNum
        maxInteger = toInteger maxNum


readClamped :: (Bounded a, Read a, Integral a) => String -> BoundedRead a
readClamped str = case readMaybe str of
    Nothing -> NoRead
    Just x -> case clamp x of
        Exact y -> ExactRead y
        Clamped y -> ClampedRead y


class ReadBounded a where
    readBounded :: String -> BoundedRead a


instance ReadBounded Integer where
    readBounded = fromMaybe . readMaybe


instance ReadBounded Int where
    readBounded = readClamped


instance ReadBounded Int8 where
    readBounded = readClamped


instance ReadBounded Int16 where
    readBounded = readClamped


instance ReadBounded Int32 where
    readBounded = readClamped


instance ReadBounded Int64 where
    readBounded = readClamped


instance ReadBounded Word where
    readBounded = readClamped


instance ReadBounded Word8 where
    readBounded = readClamped


instance ReadBounded Word16 where
    readBounded = readClamped


instance ReadBounded Word32 where
    readBounded = readClamped


instance ReadBounded Word64 where
    readBounded = readClamped




