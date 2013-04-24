-- | Cassette and sprocket related constants and functions
module Cycling.Cassette where

import Data.List {- base -}

-- | 105 cassettes.
shimano_105 :: Integral i => (i,i) -> [i]
shimano_105 c =
    case c of
      (11,23) -> [11,12,13,14,15,16,17,19,21,23]
      (11,25) -> [11,12,13,14,15,17,19,21,23,25]
      (11,28) -> [11,12,13,14,115,17,19,21,24,28]
      (12,25) -> [12,13,14,15,16,17,19,21,23,25]
      (12,27) -> [12,13,14,15,16,17,19,21,24,27]
      _ -> error "shimano_105: unknown cassette"

-- | Ultegra cassettes.
shimano_ultegra :: Integral i => (i,i) -> [i]
shimano_ultegra c =
    case c of
      (11,23) -> [11,12,13,14,15,16,17,19,21,23]
      (11,25) -> [11,12,13,14,15,17,19,21,23,25]
      (11,28) -> [11,12,13,14,15,17,19,21,24,28]
      (12,23) -> [12,13,14,15,16,17,18,19,21,23]
      (12,25) -> [12,13,14,15,16,17,19,21,23,25]
      _ -> error "shimano_ultegra: unknown cassette"

-- | Comma separated integer sequence.
--
-- > cassette_string (shimano_105 (11,23)) == "11,12,13,14,15,16,17,19,21,23"
cassette_string :: (Show i,Integral i) => [i] -> String
cassette_string = intercalate "," . map show
