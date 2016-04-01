implementation module CoclUtils

import qualified Type as T
from Type import class toType, class toTypeVar

import syntax

from StdList import map

instance toType Type
where
    toType (TA tsi ats) = 'T'.Type tsi.type_ident.id_name (map (\at -> 'T'.toType at.at_type) ats)
    toType (TAS tsi ats _) = 'T'.Type tsi.type_ident.id_name (map (\at -> 'T'.toType at.at_type) ats)
    toType (TB bt) = 'T'.Type (toString bt) []
    toType (TV tv) = 'T'.Var tv.tv_ident.id_name
    toType (GTV tv) = 'T'.Var tv.tv_ident.id_name
    toType (t1 --> t2) = 'T'.Func ['T'.toType t1.at_type] ('T'.toType t2.at_type) []
    toType ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map (\at->'T'.toType at.at_type) ats)
    toType _ = 'T'.Var "UNIMPL" //TODO

instance toType SymbolType
where
    toType {st_args,st_result}
        = 'T'.Func (map (\at->'T'.toType at.at_type) st_args) ('T'.toType st_result.at_type) []

instance toTypeVar TypeVar where toTypeVar {tv_ident} = tv_ident.id_name

