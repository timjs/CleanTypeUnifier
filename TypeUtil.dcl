definition module TypeUtil

import TypeDef
from StdOverloaded import class toString (toString)

class print a :: a -> [String]

instance print String
instance print [a] | print a

instance print ClassOrGeneric
instance print ClassRestriction
instance print ClassContext
instance print Type

