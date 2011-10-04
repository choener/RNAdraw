
-- | Several methods to draw ascii dot-bracket figures onto the screen. The
-- result will be one sequence line, a dot-bracket string, and possibly a list
-- of annotated basepairs.

module BioInf.Secondary.Draw.DotBracket where



-- | The class of dot-bracket drawable structures.

class DotBracketDraw a where
  -- | Draw a secondary structure. It is assumed that the structure is
  -- pseudoknot-free.
  draw :: PairAnno -> a -> ByteString

  -- | Draw a pseudoknotted secondary structures.
  drawPK :: PairAnno -> a -> ByteString



-- | How to handle the list of annotated pairs.

data PairAnno
  -- | Never write out the pair list with pair-type information.
  = Never
  -- | Only write out those pairs that are non-canonical, or the required list
  -- of pairs to get all information on the secondary structure.
  | Required
  -- | Always write out all pairs.
  | Always
