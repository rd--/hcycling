{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe {- base -}
import qualified Data.Text as T {- text -}
import GHC.Generics {- base -}

data Cue = Cue {dur :: Double,cue :: T.Text} deriving Generic
instance ToJSON Cue

type IEL = (Double,Double,String)

iel_dur :: Num n => [(n,x,y)] -> n
iel_dur = sum . map (\(n,_,_) -> n)

iel_cue :: IEL -> [Cue]
iel_cue (i,e,l) =
    let e' = if e > 0 then Just (Cue e (T.pack l)) else Nothing
        d = i - e
        i' = if d > 0 then Just (Cue d (T.pack "REST")) else Nothing
    in catMaybes [e',i']

iel_cues :: [IEL] -> [Cue]
iel_cues = flip (++) [Cue 0 (T.pack "...")] . concatMap iel_cue

-- > iel_dur e_02 == 20
e_02 :: [IEL]
e_02 =
    let x = "VERY HARD"
        y = "LIMIT"
    in [(4,4,"WARM UP")
       ,(2,1/2,x)
       ,(2+3/4,3/4,x)
       ,(3,1,y)
       ,(2+1/4,3/4,x)
       ,(2,1/2,y)
       ,(1/6,1/6,y)
       ,(5/6,5/6,x)
       ,(1/6,1/6,y)
       ,(2+5/6,2+5/6,"WARM DOWN")]

type ITL = (Double,Double,Int,String)

itl_to_iel :: ITL -> [IEL]
itl_to_iel (_,i,t,l) = replicate t (i * 2,i,l)

itl_cues :: [ITL] -> [Cue]
itl_cues = iel_cues . concatMap itl_to_iel

itl_dur :: [ITL] -> (Double,Double)
itl_dur x =
    let f = sum . map (\(n,_,_,_) -> n)
        g = sum . map (\(_,i,t,_) -> i * fromIntegral t * 2)
    in (f x,g x)

-- > itl_dur e_03
e_03 :: [ITL]
e_03 =
    let x = "HARD"
        y = "VERY HARD"
        z = "LIMIT"
    in [(2+1/2,1/4,5,z)
       ,(3,1/2,3,y)
       ,(6,3/4,4,x)
       ,(3,3/4,2,x)
       ,(3,1/2,3,y)
       ,(2+1/2,1/4,5,z)]

prj_file :: FilePath -> FilePath
prj_file = (++) "/home/rohan/sw/hcycling/util/cues/data/"

main :: IO ()
main = do
  B.writeFile (prj_file "e.2.json") (encode (iel_cues e_02))
  B.writeFile (prj_file "e.3.json") (encode (itl_cues e_03))
