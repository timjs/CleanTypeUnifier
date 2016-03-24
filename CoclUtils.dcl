definition module CoclUtils

from TypeDef import class toType

// Cocl frontend
from syntax import ::SymbolType, ::Type

instance toType SymbolType
instance toType Type

