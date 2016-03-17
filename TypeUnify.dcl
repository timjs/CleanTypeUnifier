definition module TypeUnify

import TypeDef

from Data.Maybe import :: Maybe

class unify a :: a a -> Maybe ([TypeVarAssignment], [TypeVarAssignment])
instance unify Type

