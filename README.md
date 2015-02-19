# read-bounded
Library for reading in values that have bounded representations.


```haskell
> readBounded "1" :: BoundedRead Word8
ExactRead 1
> readBounded "1000" :: BoundedRead Word8
ClampedRead 255
> readBounded "" :: BoundedRead Word8
NoRead
```

