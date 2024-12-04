module Model.Streamly where

import Streamly.Data.Stream as S
import Streamly.Data.StreamK (fromFoldable, toStream)

flatten :: Stream IO [a] -> Stream IO a
flatten = S.concatMap (toStream . fromFoldable)
