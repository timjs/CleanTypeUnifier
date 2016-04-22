implementation module CoclUtils

import qualified Type as T
from Type import class toType, class toTypeVar

import syntax

from StdList import map

instance toType AType
where
	toType {at_type,at_attribute}
		| at_attribute == TA_Unique = 'T'.Uniq ('T'.toType at_type)
		| otherwise = 'T'.toType at_type

instance toType Type
where
	toType (TA tsi ats) = case tsi.type_ident.id_name of
		"_String" = 'T'.Type "_#Array" ['T'.Type "Char" []]
		type_name = 'T'.Type type_name (map 'T'.toType ats)
	toType (TAS tsi ats _) = 'T'.Type tsi.type_ident.id_name (map 'T'.toType ats)
	toType (TB bt) = 'T'.Type (toString bt) []
	toType (TV tv) = 'T'.Var tv.tv_ident.id_name
	toType (GTV tv) = 'T'.Var tv.tv_ident.id_name
	toType (t1 --> t2) = 'T'.Func ['T'.toType t1.at_type] ('T'.toType t2.at_type) []
	toType ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map 'T'.toType ats)
	toType _ = 'T'.Var "UNIMPL" //TODO

instance toType SymbolType
where
	toType {st_args,st_result,st_context}
		= 'T'.Func (map 'T'.toType st_args) ('T'.toType st_result) cc
	where
		cc = [('T'.Class gds.glob_object.ds_ident.id_name,
		       chainTypes (map 'T'.toType tc_types))
		        \\ {tc_class=(TCClass gds),tc_types} <- st_context]

		chainTypes :: ['T'.Type] -> 'T'.Type
		chainTypes [('T'.Type t ts):rest] = 'T'.Type t (ts ++ rest)
		chainTypes [('T'.Cons t ts):rest] = 'T'.Cons t (ts ++ rest)
		chainTypes [('T'.Var v):rest] = 'T'.Cons v rest

instance toTypeVar TypeVar where toTypeVar {tv_ident} = tv_ident.id_name

