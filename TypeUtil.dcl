definition module TypeUtil

import TypeDef
from StdOverloaded import class toString (toString)

from Data.Maybe import ::Maybe

class print a :: a -> [String]

instance print String
instance print Int

instance print [a] | print a
instance print (Maybe a) | print a

instance print ClassOrGeneric
instance print ClassRestriction
instance print ClassContext
instance print Type
instance print TypeDef

propagate_uniqueness :: Type -> Type
