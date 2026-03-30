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
top n = StateDom (infeasibleTableau n)

bot :: Int -> StateDom
bot n = StateDom (Tableau n [negIdentityPauli n])

-----------------------------
-- Operations
-----------------------------
meet :: StateDom -> StateDom -> StateDom
meet = undefined

join :: StateDom -> StateDom -> StateDom
join = undefined