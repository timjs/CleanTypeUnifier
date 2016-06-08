definition module TypeUnify

import TypeDef

from Data.Maybe import ::Maybe

class unify a :: [Instance] a a -> Maybe Unifier
instance unify Type
