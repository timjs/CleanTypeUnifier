definition module CoclUtils

from TypeDef import class toType, class toTypeVar

// Cocl frontend
from syntax import ::SymbolType, ::Type, ::TypeVar

instance toType SymbolType
instance toType Type

instance toTypeVar TypeVar

