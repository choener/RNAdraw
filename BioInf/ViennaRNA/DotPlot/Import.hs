{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.ViennaRNA.DotPlot.Import where

import Data.Text (Text(..))
import qualified Data.Array.IArray as A
import qualified Data.Text as T
import qualified Data.Text.Read as R

import BioInf.ViennaRNA.DotPlot



textToDotPlot :: Text -> DotPlot
textToDotPlot t = DotPlot (A.accumArray (const id) Nothing ((1,1),(l,l)) (map f ps)) True sq where
  l  = T.length sq
  ls = T.lines t
  sq = case (drop 1 . dropWhile (not . T.isPrefixOf "/sequence") $ ls) of
         (x:_)     -> T.init x
         otherwise -> error $ "lines do not contain sequence part: " ++ show ls
  ps = filter (T.isSuffixOf "box")
     . filter (not . T.isPrefixOf "%")
     . takeWhile (not . T.isPrefixOf "showpage")
     . drop 1
     . dropWhile (not . T.isPrefixOf "%start of base pair probability data")
     $ ls
  f p'
    | last p == "cbox" = ( (rdI $ p!!1, rdI $ p!!0), (Just (rdP $ p!!2, Just (rdP $ p!!3, rdP $ p!!4, rdP $ p!!5))) )
    | last p == "ubox" = ( (rdI $ p!!0, rdI $ p!!1), (Just (rdP $ p!!2, Nothing)) )
    | last p == "lbox" = ( (rdI $ p!!1, rdI $ p!!0), (Just (rdP $ p!!2, Nothing)) )
    where p = T.words p'
          rdI :: Text -> Int
          rdI = chk . R.decimal
          rdP :: Text -> Double
          rdP = chk . R.rational
          chk (Left e)      = error $ "dotplot/import read error: " ++ e ++ " in " ++ T.unpack p' ++ show p
          chk (Right (k,_)) = k

