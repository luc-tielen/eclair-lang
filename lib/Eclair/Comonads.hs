module Eclair.Comonads
  ( module Eclair.Comonads
  ) where


data Triple a b c
  = Triple
  { tFst :: a
  , tSnd :: b
  , tThd :: c
  } deriving Functor

instance Comonad (Triple a b) where
  extract (Triple _ _ c) = c

  duplicate (Triple a b c) =
    Triple a b (Triple a b c)

data Quad a b c d
  = Quad
  { qFirst :: a
  , qSecond :: b
  , qThird :: c
  , qFourth :: d
  } deriving Functor

instance Comonad (Quad a b c) where
  extract (Quad _ _ _ d) = d

  duplicate (Quad a b c d) =
    Quad a b c (Quad a b c d)

