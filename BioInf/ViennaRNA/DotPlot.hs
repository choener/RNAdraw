
-- | Handles ViennaRNA dot-plots.

module BioInf.ViennaRNA.DotPlot where

import           Control.Arrow
import           Data.List
import qualified Data.Array.IArray as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import           Biobase.Secondary
import           Biobase.Secondary.Diagrams



type Color = (Double,Double,Double)

data DotPlot = DotPlot
  { dotplot     :: A.Array (Int,Int) [(Double,Maybe Color)]
  , isPartition :: Bool   -- are we doing a partition function plot
  , rnaSequence :: T.Text
  }
  deriving (Eq,Show)



clearMFE :: DotPlot -> DotPlot
clearMFE dp@DotPlot{..} = dp { dotplot = dotplot A.// (os) } where
  os = [ (ij,[]) | ij@(i,j) <- A.indices dotplot, j<i ]

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
      | Just clr <- lookupColor zs = [(0.95, Just clr)]
      | otherwise                  = [(0.95, Nothing)]
    lookupColor zs = do
      cmJ <- (sort zs) `lookup` cm
      cmJ `lookup` cs

-- | colorize probability dots

colorizeProbDots :: [(String,Color)] -> String -> DotPlot -> DotPlot
colorizeProbDots cs c dp@DotPlot{..}
  | Nothing <- c `lookup` cs = error "default color not in list of current colors"
  | otherwise = dp { dotplot = dotplot A.// probcells }
  where
    probcells = [ ((i,j), mkEntry i j zs) | ((i,j),zs) <- A.assocs dotplot ]
    mkEntry :: Int -> Int -> [(Double,Maybe Color)] -> [(Double,Maybe Color)]
    mkEntry i j [] = []
    mkEntry i j zs = map (second (const $ c `lookup` cs)) zs

-- | Merges two dot-plots.
--
-- TODO add some error checking!

mergeDotPlots :: DotPlot -> DotPlot -> DotPlot
mergeDotPlots (DotPlot ldp lip ls) (DotPlot rdp rip rs) = DotPlot dp ip s where
  dp = A.accumArray (++) [] (A.bounds ldp) (A.assocs ldp ++ A.assocs rdp)
  ip = lip && rip
  s  = ls

