implementation module CoclUtils

import qualified Type as T
from Type import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField,
	::ClassContext, ::ClassRestriction, ::ClassOrGeneric, class toClassContext(..)

import syntax
import qualified syntax

from StdList import map

instance toClassContext [TypeContext]
where
	toClassContext context
		= [('T'.Class gds.glob_object.ds_ident.id_name,
		    chainTypes (map 'T'.toType tc_types))
		     \\ {tc_class=(TCClass gds),tc_types} <- context] ++
		  [('T'.Generic gtc_generic.glob_object.ds_ident.id_name (kind gtc_kind),
		    chainTypes (map 'T'.toType tc_types))
		     \\ {tc_class=(TCGeneric {gtc_generic,gtc_kind}),tc_types} <- context]
	where
		chainTypes :: ['T'.Type] -> 'T'.Type
		chainTypes [('T'.Type t ts):rest] = 'T'.Type t (ts ++ rest)
		chainTypes [('T'.Cons t ts):rest] = 'T'.Cons t (ts ++ rest)
		chainTypes [('T'.Var v)]          = 'T'.Var v
		chainTypes [('T'.Var v):rest]     = 'T'.Cons v rest

		kind :: TypeKind -> 'T'.Kind
		kind KindConst = 'T'.KindConst
		kind (KindArrow ks) = 'T'.KindArrow (map kind ks)

instance toClassContext TypeContext
where toClassContext tc = toClassContext [tc]

instance toType ATypeVar
where
	toType {atv_attribute=TA_Unique,atv_variable}
		= 'T'.Uniq ('T'.Var ('T'.toTypeVar atv_variable))
	toType {atv_variable} = 'T'.Var ('T'.toTypeVar atv_variable)

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
	toType (t1 --> t2) = 'T'.Func ['T'.toType t1] ('T'.toType t2) []
	toType ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map 'T'.toType ats)
	toType _ = 'T'.Var "unimplemented" //TODO

instance toType SymbolType
where
	toType {st_args,st_result,st_context}
		= 'T'.Func (map 'T'.toType st_args) ('T'.toType st_result) (toClassContext st_context)

instance toTypeVar TypeVar where toTypeVar {tv_ident} = tv_ident.id_name

instance toTypeDef 'syntax'.ParsedTypeDef
where
	toTypeDef {td_ident,td_attribute,td_args,td_rhs}
		= 'T'.typedef td_ident.id_name
			(td_attribute == TA_Unique)
			(map 'T'.toType td_args)
			('T'.toTypeDefRhs td_rhs)

instance toTypeDefRhs RhsDefsOfType
where
	toTypeDefRhs (ConsList pcs)
		= 'T'.TDRCons False (map 'T'.toConstructor pcs)
	toTypeDefRhs (SelectorList id exi_vars _ pss)
		= 'T'.TDRRecord id.id_name
			(map (\t -> 'T'.toTypeVar t.atv_variable) exi_vars)
			(map 'T'.toRecordField pss)
	toTypeDefRhs (TypeSpec atype)
		= 'T'.TDRSynonym ('T'.toType atype)
	toTypeDefRhs (EmptyRhs _)
		= 'T'.TDRAbstract
	toTypeDefRhs (AbstractTypeSpec _ atype)
		= 'T'.TDRAbstractSynonym ('T'.toType atype)
	toTypeDefRhs (ExtensibleConses pcs)
		= 'T'.TDRCons True (map 'T'.toConstructor pcs)

instance toConstructor ParsedConstructor
where
	toConstructor {pc_cons_ident,pc_arg_types,pc_exi_vars,pc_context}
		= 'T'.constructor pc_cons_ident.id_name
			(map 'T'.toType pc_arg_types)
			(map (\t -> 'T'.toTypeVar t.atv_variable) pc_exi_vars)
			(toClassContext pc_context)

instance toRecordField ParsedSelector
where
	toRecordField {ps_selector_ident,ps_field_type}
		= 'T'.recordfield ps_selector_ident.id_name ('T'.toType ps_field_type)
