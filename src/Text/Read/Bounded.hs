module Text.Read.Bounded (
    BoundedRead(..),
    ReadBounded(..),
    readBoundedInteger,
) where


import Data.Int
import Data.Word
import Text.Read (readMaybe)


-- | Information about a bounded read.
data BoundedRead a
    = NoRead -- ^ The read failed.
    | ExactRead a -- ^ The value was successfully read exactly, and did not have to be clamped to a narrower representation.
    | ClampedRead a -- ^ The value was successfully read, but had to be clamped to a narrower representation because its value was too wide.
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


-- | Reads a clamped value for any integer type with the given class constraints.
-- Useful for implementing a 'ReadBounded' instance or avoiding one.
readBoundedInteger :: (Bounded a, Read a, Integral a) => String -> BoundedRead a
readBoundedInteger str = case readMaybe str of
    Nothing -> NoRead
    Just x -> case clamp x of
        Exact y -> ExactRead y
        Clamped y -> ClampedRead y


-- | Much like the 'Prelude.Read' class, but will return (possibly) clamped values.
--
-- Typical instances of this class will clamp against 'Prelude.Bounded.minBound' and 'Prelude.Bounded.maxBound'
--
-- This class is designed to avoid inconsistency problems such as the following:
--
-- >>> read "999999999999999999999" :: Int
-- 3875820019684212735
-- >>> read "9999999999999999999999" :: Int
-- 1864712049423024127
--
-- Using this class, the results are predictable and precise:
--
-- >>> readBounded "999999999999999999999" :: BoundedRead Int
-- ClampedRead 9223372036854775807
-- >>> readBounded "9999999999999999999999" :: BoundedRead Int
-- ClampedRead 9223372036854775807
-- >>> readBounded "9223372036854775807" :: BoundedRead Int
-- ExactRead 9223372036854775807
-- >>> readBounded "1337" :: BoundedRead Int
-- ExactRead 1337
-- >>> readBounded "xxx" :: BoundedRead Int
-- NoRead
class ReadBounded a where
    readBounded :: String -> BoundedRead a


instance ReadBounded Integer where
    readBounded = fromMaybe . readMaybe


instance ReadBounded Int where
    readBounded = readBoundedInteger


instance ReadBounded Int8 where
    readBounded = readBoundedInteger


instance ReadBounded Int16 where
    readBounded = readBoundedInteger


instance ReadBounded Int32 where
    readBounded = readBoundedInteger


instance ReadBounded Int64 where
    readBounded = readBoundedInteger


instance ReadBounded Word where
    readBounded = readBoundedInteger


instance ReadBounded Word8 where
    readBounded = readBoundedInteger


instance ReadBounded Word16 where
    readBounded = readBoundedInteger


instance ReadBounded Word32 where
    readBounded = readBoundedInteger


instance ReadBounded Word64 where
    readBounded = readBoundedInteger




