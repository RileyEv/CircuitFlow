module Pipeline.Nat (
  module Pipeline.Internal.Common.Nat,
  N1,
  N2,
  N3,
  N4,
  N5,
  N6,
  N7,
  N8,
  N9,
) where

import Pipeline.Internal.Common.Nat (Nat(..))


type N1 = 'Succ 'Zero
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4
type N6 = 'Succ N5
type N7 = 'Succ N6
type N8 = 'Succ N7
type N9 = 'Succ N8
