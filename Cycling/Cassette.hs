-- | Cassette and sprocket related constants and functions
module Cycling.Cassette where

import Data.List

shimano_105_11_23 :: Integral i => [i]
shimano_105_11_23 = [11,12,13,14,15,16,17,19,21,23]

shimano_105_11_25 :: Integral i => [i]
shimano_105_11_25 = [11,12,13,14,15,17,19,21,23,25]

shimano_105_12_25 :: Integral i => [i]
shimano_105_12_25 = [12,13,14,15,16,17,19,21,23,25]

shimano_105_12_27 :: Integral i => [i]
shimano_105_12_27 = [12,13,14,15,16,17,19,21,24,27]

-- | Comma separated integer sequence.
--
-- > cassette_string shimano_105_11_23 == "11,12,13,14,15,16,17,19,21,23"
cassette_string :: Integral i => [i] -> String
cassette_string = intercalate "," . map show
