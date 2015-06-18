{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe {- base -}
import qualified Data.Text as T {- text -}
import GHC.Generics {- base -}

-- | Fractional minutes are mm.ss, so that 15.35 is 15 minutes and 35 seconds.
--
-- > fmin_to_sec 15.35 == 935
fmin_to_sec :: (RealFrac f,Integral i) => f -> i
fmin_to_sec n =
    let m = floor n
        s = round ((n - fromIntegral m) * 100)
    in (m * 60) + s

-- > sec_to_fmin 935 == 15.35
sec_to_fmin :: (Fractional f, Integral i) => i -> f
sec_to_fmin n =
    let m = fromIntegral (floor (fromIntegral n / 60))
        s = fromIntegral n - (m * 60)
    in m + (s / 100)

-- > fmin_add 1.30 0.45 == 2.15
-- > fmin_add 1.30 0.45 == 2.15
fmin_add :: RealFrac f => f -> f -> f
fmin_add p q = sec_to_fmin (fmin_to_sec p + fmin_to_sec q)

fmin_sub :: RealFrac f => f -> f -> f
fmin_sub p q = sec_to_fmin (fmin_to_sec p - fmin_to_sec q)

-- > fmin_mul 0.45 2 == 1.30
fmin_mul :: (Integral i, RealFrac f) => f -> i -> f
fmin_mul t n = sec_to_fmin (fmin_to_sec t * n)

type FMIN = Double
type SEC = Double
data Cue = Cue {dur :: FMIN,cue :: T.Text} deriving Generic
instance ToJSON Cue

type IEL = (FMIN,FMIN,String)

iel_dur :: [IEL] -> Double
iel_dur = foldl fmin_add 0 . map (\(n,_,_) -> n)

iel_cue :: String -> IEL -> [Cue]
iel_cue def (i,e,l) =
    let e' = if e > 0 then Just (Cue e (T.pack l)) else Nothing
        d = fmin_sub i e
        i' = if d > 0 then Just (Cue d (T.pack def)) else Nothing
    in catMaybes [e',i']

iel_cues :: String -> [IEL] -> [Cue]
iel_cues def = flip (++) [Cue 0 (T.pack "...")] . concatMap (iel_cue def)

-- > iel_dur e_02 == 20
e_02 :: [IEL]
e_02 =
    let x = "VERY HARD"
        y = "LIMIT"
    in [(4.00,4.00,"WARM UP")
       ,(2.00,0.30,x)
       ,(2.45,0.45,x)
       ,(3.00,1.00,y)
       ,(2.15,0.45,x)
       ,(2.00,0.30,y)
       ,(0.10,0.10,y)
       ,(0.50,0.50,x)
       ,(0.10,0.10,y)
       ,(2.50,2.50,"WARM DOWN")]

type ITL = (FMIN,FMIN,Int,String)

itl_to_iel :: ITL -> [IEL]
itl_to_iel (_,i,t,l) = replicate t (fmin_mul i 2,i,l)

itl_cues :: String -> [ITL] -> [Cue]
itl_cues def = iel_cues def . concatMap itl_to_iel

itl_dur :: [ITL] -> (Double,Double)
itl_dur x =
    let f = foldl fmin_add 0 . map (\(n,_,_,_) -> n)
        g = foldl fmin_add 0 . map (\(_,i,t,_) -> fmin_mul i (t * 2))
    in (f x,g x)

-- > itl_dur e_03
e_03 :: [ITL]
e_03 =
    let x = "HARD"
        y = "VERY HARD"
        z = "LIMIT"
    in [(2.30,0.15,5,z)
       ,(3.00,0.30,3,y)
       ,(6.00,0.45,4,x)
       ,(3.00,0.45,2,x)
       ,(3.00,0.30,3,y)
       ,(2.30,0.15,5,z)]

prj_file :: FilePath -> FilePath
prj_file = (++) "/home/rohan/sw/hcycling/util/cues/data/"

main :: IO ()
main = do
  B.writeFile (prj_file "e.2.json") (encode (iel_cues "REST" e_02))
  B.writeFile (prj_file "e.3.json") (encode (itl_cues "TEMPO" e_03))
