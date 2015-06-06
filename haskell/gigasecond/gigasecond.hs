module Gigasecond (fromDay) where
import Data.Time

fromDay :: UTCTime -> UTCTime
fromDay x = addUTCTime (10^9) x