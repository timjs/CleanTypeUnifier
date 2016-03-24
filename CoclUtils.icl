implementation module CoclUtils

import qualified Type as T
from Type import class toType

import syntax

instance toType Type
where
    toType (TA tsi ats) = tsiToType tsi ats
    toType (TAS tsi ats _) = tsiToType tsi ats
    toType (TB bt) = 'T'.Type (toString bt) []
    toType (TV tv) = 'T'.Var tv.tv_ident.id_name
    toType (GTV tv) = 'T'.Var tv.tv_ident.id_name
    toType (t1 --> t2) = 'T'.Func ['T'.toType t1.at_type] ('T'.toType t2.at_type) []
    toType ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map (\at->'T'.toType at.at_type) ats)
    toType _ = 'T'.Var "UNIMPL" //TODO
        
tsiToType :: TypeSymbIdent [AType] -> 'T'.Type
tsiToType tsi ats
| name == "_List"
    | isEmpty ats = 'T'.List 'T'.NormalList ('T'.Var "length") 'T'.NormalSpine // it seems that length on [] is the only instance where a list does not have a type argument. WHY!?
    = 'T'.List 'T'.NormalList ('T'.toType (hd ats).at_type) 'T'.NormalSpine
| name % (0,size name-2) == "_Tuple"
    = 'T'.Tuple [('T'.NotStrict, 'T'.toType at.at_type) \\ at <- ats]
| otherwise // are these really all the cases?
    = 'T'.Type name (map (\at->'T'.toType at.at_type) ats)
where
    name = tsi.type_ident.id_name

instance toType SymbolType
where
    toType {st_args,st_result}
        = 'T'.Func (map (\at->'T'.toType at.at_type) st_args) ('T'.toType st_result.at_type) []

