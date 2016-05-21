implementation module TypeDef

from StdOverloaded import class ==(..), class length(..)
from StdClass import class Eq
import StdList
import StdTuple
from StdString import instance == {#Char}
import StdBool
from StdFunc import o
from GenEq import generic gEq, ===
from Data.Func import $

derive gEq ClassOrGeneric, Type, Instance

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

isType :: Type -> Bool
isType (Type _ _) = True; isType _ = False

isUniq :: Type -> Bool
isUniq (Uniq _) = True; isUniq _ = False

arity :: Type -> Int
arity (Type _ ts) = length ts
arity (Func is _ _) = length is
arity (Var _) = 0
arity (Cons _ ts) = length ts
//TODO arity of Uniq t?

isClass :: ClassOrGeneric -> Bool
isClass (Class _) = True
isClass _ = False

isClassRestriction :: (ClassRestriction -> Bool)
isClassRestriction = isClass o fst

