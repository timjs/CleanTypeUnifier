implementation module TypeDef

from StdOverloaded import class ==(..), class length(..)
from StdClass import class Eq
import StdList
import StdTuple
from StdString import instance == {#Char}
import StdBool
from StdFunc import o, id
from GenEq import generic gEq, ===
from Data.Func import $

derive gEq ClassOrGeneric, Type, Instance, Kind

instance == Type where (==) a b = a === b
instance == Instance where (==) a b = a === b

subtypes :: Type -> [Type]
subtypes t=:(Type s ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Func is r cc) = removeDup [t : flatten (map subtypes [r:is])]
subtypes t=:(Cons c ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Uniq t`) = removeDup [t : subtypes t`]
subtypes t = [t]

// All the type and constructor variables in a type
allVars :: Type -> [TypeVar]
allVars t = map varName $ filter (\t -> isCons t || isVar t) $ subtypes t
where
	varName :: Type -> TypeVar
	varName (Cons v _) = v; varName (Var v) = v

isVar :: Type -> Bool
isVar (Var _) = True; isVar _ = False

fromVar :: Type -> TypeVar
fromVar (Var v) = v

isCons :: Type -> Bool
isCons (Cons _ _) = True; isCons _ = False

isCons` :: TypeVar Type -> Bool
isCons` v (Cons v` _) = v == v`; isCons` _ _ = False

isVarOrCons` :: TypeVar Type -> Bool
isVarOrCons` v (Var v`)    = v == v`
isVarOrCons` v (Cons v` _) = v == v`
isVarOrCons` _ _           = False

isType :: Type -> Bool
isType (Type _ _) = True; isType _ = False

isFunc :: Type -> Bool
isFunc (Func _ _ _) = True; isFunc _ = False

isUniq :: Type -> Bool
isUniq (Uniq _) = True; isUniq _ = False

arity :: Type -> Int
arity (Type _ ts) = length ts
arity (Func is _ _) = length is
arity (Var _) = 0
arity (Cons _ ts) = length ts
//TODO arity of Uniq t?

constructorsToFunctions :: TypeDef -> [(String,Type)]
constructorsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRCons _ conses}
	= [(c.cons_name, Func c.cons_args return c.cons_context) \\ c <- conses]
where return = if td_uniq Uniq id $ Type td_name td_args
constructorsToFunctions _ = []

td_name :: TypeDef -> String
td_name {td_name} = td_name

typedef :: String Bool [Type] TypeDefRhs -> TypeDef
typedef name uniq args rhs
	= {td_name=name, td_uniq=uniq, td_args=args, td_rhs=rhs}

constructor :: String [Type] [TypeVar] ClassContext -> Constructor
constructor name args exi_vars cc
	= {cons_name=name, cons_args=args, cons_exi_vars=exi_vars, cons_context=cc}

recordfield :: String Type -> RecordField
recordfield selector type = {rf_name=selector, rf_type=type}
