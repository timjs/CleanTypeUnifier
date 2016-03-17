implementation module CoclUtils

import qualified Type as T
from Type import class toType

import syntax

class toType` a :: a -> 'T'.Type

instance toType` Type
where
    toType` (TA tsi ats) = tsiToType tsi ats
    toType` (TAS tsi ats _) = tsiToType tsi ats
    toType` (TB bt) = 'T'.Type (toString bt) []
    toType` (TV tv) = 'T'.Var tv.tv_ident.id_name
    toType` (GTV tv) = 'T'.Var tv.tv_ident.id_name
    toType` (t1 --> t2) = 'T'.Func [toType` t1.at_type] (toType` t2.at_type) []
    toType` ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map (\at->toType` at.at_type) ats)
    toType` _ = 'T'.Var "UNIMPL" //TODO
        
tsiToType :: TypeSymbIdent [AType] -> 'T'.Type
tsiToType tsi ats
| name == "_List"
    = 'T'.List 'T'.NormalList (toType` (hd ats).at_type) 'T'.NormalSpine
| name % (0,size name-2) == "_Tuple"
    = 'T'.Tuple [('T'.NotStrict, toType` at.at_type) \\ at <- ats]
| otherwise // are these really all the cases?
    = 'T'.Type name (map (\at->toType` at.at_type) ats)
where
    name = tsi.type_ident.id_name

instance toType SymbolType
where
    toType {st_args,st_result}
        = 'T'.Func (map (\at->toType` at.at_type) st_args) (toType` st_result.at_type) []

