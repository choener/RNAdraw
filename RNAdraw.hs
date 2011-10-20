{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Tree



-- ** Secondary structure trees
--
-- TODO this needs to go into BiobaseXNA later on

-- | Pseudoknot-free secondary structure tree. We are using the GHC rose-tree
-- (Data.Tree) as underlying data structure.

type SSTree pairtype payload = Tree (Loop pairtype payload)

-- | Pseudoknot-free secondary structure tree are based on loops. Each loop
-- contains 0 or more basepairs. We have 0 pairs only for the completely
-- unpaired sequence. 1 pair is a hairpin, 2 pairs for stems, and bulges. 3 or
-- more pairs are multibranched loops (or the external loop). The external
-- loop, however, has no closing pair.

data Loop pairtype payload
  = Loop
    {
    }
  | External
    {
    }

-- | Create a secondary structure tree.

mkSSTree :: () -> SSTree () ()
mkSSTree = undefined

-- ** testing area

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
