module Lattice
( StateDom(..)
, top
, bot
, meet
, join
) where

import StabilizerTableau

newtype StateDom = StateDom StabTableau deriving (Eq, Show)

-----------------------------
-- Constans
-----------------------------
top :: Int -> StateDom
top n = StateDom (trivialTableau n)

bot :: Int -> StateDom
bot n = StateDom (unsatTableau n)

-----------------------------
-- Operations
-----------------------------
meet :: StateDom -> StateDom -> StateDom
meet (StateDom tab1) (StateDom tab2) = StateDom (union tab1 tab2)

join :: StateDom -> StateDom -> StateDom
join (StateDom tab1) (StateDom tab2) = StateDom (intersection tab1 tab2)