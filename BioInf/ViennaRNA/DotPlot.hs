{-# LANGUAGE TypeOperators #-}

-- | Handles ViennaRNA dot-plots.

module BioInf.ViennaRNA.DotPlot where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Text as T

import qualified Data.PrimitiveArray      as PA
import qualified Data.PrimitiveArray.Zero as PA
import Data.Array.Repa.Index.Subword



data DotPlot = DotPlot
  { upperTri :: PA.Unboxed (Z:.Subword) Double
  , lowerTri :: PA.Unboxed (Z:.Subword) Double -- need to store as (j,i) ?
  , isPartition :: Bool   -- are we doing a partition function plot
  , rnaSequence :: T.Text
  }
  deriving (Eq,Show)

