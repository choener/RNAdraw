{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- | Several methods to draw ascii dot-bracket figures onto the screen. The
-- result will be one sequence line, a dot-bracket string, and possibly a list
-- of annotated basepairs. The dot-bracket string can handle base triplets.

module BioInf.Secondary.Draw.DotBracket where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V
import Text.Printf
import Prelude hiding (sequence)

import Biobase.Secondary



-- | The class of dot-bracket drawable structures.

class DotBracketDraw a where
  -- | Draw a secondary structure. It is assumed that the structure is
  -- pseudoknot-free.
  draw :: PairAnno -> SequenceNumbering -> a -> String
  -- | As 'draw' but returns the different strings as parts. These can then be
  -- further altered by the receiving end.
  drawParts :: PairAnno -> SequenceNumbering -> a -> Parts
  -- | Draw a pseudoknotted secondary structures.
  drawPK :: PairAnno -> SequenceNumbering -> a -> String

data Parts = Parts
  { numbers :: Maybe String
  , sequence :: String
  , structure :: String
  , extended :: [String]
  }

-- | 

instance DotBracketDraw (String,[ExtPairIdx]) where
  draw pa sn (seq,xs) = unlines $
    maybe [] (:[]) numbers ++
    [ sequence
    , structure
    ] ++ extended
    where
      Parts{..} = drawParts pa sn (seq,xs)
  drawParts pa sn (seq,xs) = Parts
    { numbers = if sn==Numbered then Just . take l . concat $ repeat ['0'..'9'] else Nothing
    , sequence = seq
    , structure = V.toList $ V.accum updateStructure (V.fromList $ replicate l '.') (map ((,'(').baseL) xs ++ map ((,')').baseR) xs)
    , extended = [ printf "  %4d %4d   %c-%c   %s" (baseL x) (baseR x) (seq!!baseL x) (seq!!baseR x) (show $ baseT x)
                 | x <- xs, pa == Always || pa == Required && baseT x /= cWW
                 ]
    } where
      l = length seq
      updateStructure cur new
        | cur == '.' = new
        | cur == new && cur == '(' = '<'
        | cur == new && cur == ')' = '>'
        | cur `elem` "()" && new `elem` "()" = 'X'
        | otherwise = '3' -- three outgoing edges...

instance DotBracketDraw (String,[PairIdx]) where
  draw pa sn (seq,xs) = draw pa sn (seq,map (\x -> ((baseL x, baseR x), baseT x)) xs)
  drawParts pa sn (seq,xs) = drawParts pa sn (seq, map (\x -> ((baseL x, baseR x), baseT x)) xs)

instance DotBracketDraw (ByteString,[PairIdx]) where
  draw pa sn (seq,xs) = draw pa sn (BS.unpack seq,xs)
  drawParts pa sn (seq,xs) = drawParts pa sn (BS.unpack seq, xs)

instance DotBracketDraw (ByteString,[ExtPairIdx]) where
  draw pa sn (seq,xs) = draw pa sn (BS.unpack seq,xs)
  drawParts pa sn (seq,xs) = drawParts pa sn (BS.unpack seq, xs)

-- | How to handle the list of annotated pairs.

data PairAnno
  -- | Never write out the pair list with pair-type information.
  = Never
  -- | Only write out those pairs that are non-canonical, or the required list
  -- of pairs to get all information on the secondary structure.
  | Required
  -- | Always write out all pairs.
  | Always
  deriving (Show,Read,Eq)

data SequenceNumbering
  = Numbered
  | NotNumbered
  deriving (Show,Read,Eq)
