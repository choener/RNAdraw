{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Tree

main = defaultMain $ xyz

xyz = circle 1

-- | A nucleotide is a circle, with different color based on the nucleotide
-- given

nucleotide c = circle 1 # fc nc where
  nc = case c of
        'A' -> white
        _   -> white

-- | some test data

test = Node "" [Node "CG" [Node "CG" [Node "CG" [x,x]]]] where
  x = Node "AU" [Node "AU" [Node "AU" []]]
