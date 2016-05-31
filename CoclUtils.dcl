definition module CoclUtils

from TypeDef import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField

// Cocl frontend
from syntax import ::SymbolType, ::Type, ::TypeVar, ::ParsedSelector,
	::ParsedConstructor, ::RhsDefsOfType
import qualified syntax

instance toType SymbolType
instance toType Type

instance toTypeVar TypeVar

instance toTypeDef 'syntax'.ParsedTypeDef
instance toTypeDefRhs RhsDefsOfType
instance toConstructor ParsedConstructor
instance toRecordField ParsedSelector
