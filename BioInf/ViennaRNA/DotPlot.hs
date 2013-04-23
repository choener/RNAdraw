{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Handles ViennaRNA dot-plots.

module BioInf.ViennaRNA.DotPlot where

import qualified Data.Array.IArray as A
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as M
import Data.List
import Control.Arrow

import Biobase.Secondary
import Biobase.Secondary.Diagrams



type Color = (Double,Double,Double)

data DotPlot = DotPlot
  { dotplot     :: A.Array (Int,Int) (Maybe (Double,Maybe Color))
  , isPartition :: Bool   -- are we doing a partition function plot
  , rnaSequence :: T.Text
  }
  deriving (Eq,Show)



clearMFE :: DotPlot -> DotPlot
clearMFE dp@DotPlot{..} = dp { dotplot = dotplot A.// (os) } where
  os = [ (ij,Nothing) | ij@(i,j) <- A.indices dotplot, j<i ]

addStructure :: [(String,Color)] -> String -> [([Int],String)] -> DotPlot -> [String] -> DotPlot
addStructure cs dc cm dp@DotPlot{..} xs
  | Nothing <- dc `lookup` cs = error "default color not in list of current colors"
  | any ((/=) (snd . snd . A.bounds $ dotplot) . length) xs = error $ "structure(s) with wrong length detected:\n" ++ unlines xs
  | otherwise = dp { dotplot = dotplot A.// cells }
  where
    Just defC = dc `lookup` cs
    lps :: [(Int,[PairIdx])]
    lps = map (fromD1S . mkD1S) xs
    ms  = zipWith mkMap [(1::Int) ..] (map snd lps)
    mkMap k ps = M.fromList $ map (,[k]) $ map ((+1) *** (+1)) ps
    unn = M.unionsWith (++) ms
    cells = [ ((j,i), mkEntry i j zs) | ((i,j),zs) <- M.assocs unn ]
    mkEntry i j zs
      | Just clr <- lookupColor zs = Just (0.95, Just clr)
      | otherwise                  = Just (0.95, Nothing)
    lookupColor zs = do
      cmJ <- (sort zs) `lookup` cm
      cmJ `lookup` cs

