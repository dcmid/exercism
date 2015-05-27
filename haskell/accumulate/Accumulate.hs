module Accumulate (accumulate) where

accumulate op col = [op x|x<-col]