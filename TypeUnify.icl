implementation module TypeUnify

import TypeDef

from StdFunc import o, flip
import StdString
import StdTuple
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

derive gEq ListKind, SpineStrictness, Strict, ArrayKind, ClassOrGeneric, Type

unifyEq a b :== if (a === b) (Just ([],[])) Nothing
instance unify ArrayKind where unify a b = unifyEq a b
instance unify ListKind where unify a b = unifyEq a b
instance unify SpineStrictness where unify a b = unifyEq a b
instance unify Strict where unify a b = unifyEq a b

instance unify [Type]
where
    unify ts1 ts2 
    | length ts1 <> length ts2 = Nothing
    | otherwise = apptuple removeDup <$> foldM (\(as1,as2) (t1,t2) ->
        let (t1`,t2`) = (assignAll as1 t1, assignAll as2 t2) in
          (\(a,b)->(a++as1, b++as2)) <$> unify t1` t2`) ([],[]) (zip2 ts1 ts2)

instance unify Type
where
    unify a=:(Var a`) b=:(Var b`) = Just ([(a`,b)], [(b`,a)]) >>= sane
    unify (Var a) b = Just ([(a, b)],[]) >>= sane
    unify a (Var b) = Just ([],[(b, a)]) >>= sane
    unify (Type t1 vs1) (Type t2 vs2) = if (t1==t2) (unify vs1 vs2 >>= sane) Nothing
    unify (List k1 t1 s1) (List k2 t2 s2)
        = unify k1 k2 >>| unify s1 s2 >>| unify t1 t2 >>= sane
    unify (Tuple ts1) (Tuple ts2) = unify (map snd ts1) (map snd ts2) >>= sane//TODO unify strictness?
    unify (Array k1 t1) (Array k2 t2) = unify k1 k2 >>| unify t1 t2 >>= sane
    unify (Func ts1 r1 cc1) (Func ts2 r2 cc2) = unify [r1:ts1] [r2:ts2] >>= sane //TODO unify class context
    unify (Uniq t1) (Uniq t2) = unify t1 t2 >>= sane
    unify (Cons c1 ts1) (Cons c2 ts2) = unify [Var c1:ts1] [Var c2:ts2] >>= sane
    unify _ _ = Nothing

//-----------------------//
// Unification utilities //
//-----------------------//

// Apply a TypeVarAssignment to a Type
assign :: TypeVarAssignment Type -> Type
assign (v,a) (Var v`) = if (v == v`) a (Var v`)
assign va (List k t s) = List k (assign va t) s
assign va (Tuple ts) = Tuple (map (\(s,t)->(s,assign va t)) ts)
assign va (Array k t) = Array k (assign va t)
assign va (Func ts r cc) = Func (map (assign va) ts) (assign va r) cc
assign va (Uniq t) = Uniq (assign va t)
assign _ t = t

// Apply a list of TypeVarAssignments in the same manner as assign to a Type
assignAll :: ([TypeVarAssignment] Type -> Type)
assignAll = flip $ foldr assign

// All the type and constructor variables in a type
allVars :: Type -> [TypeVar]
allVars (Var a) = [a]
allVars (Cons c ts) = removeDup [c:flatten $ map allVars ts]
allVars (Type _ ts) = removeDup $ flatten $ map allVars ts
allVars (List _ t _) = allVars t
allVars (Tuple ts) = removeDup $ flatten $ map (allVars o snd) ts
allVars (Array _ t) = allVars t
allVars (Func ts r _) = removeDup $ flatten $ map allVars [r:ts]
allVars (Uniq t) = allVars t

// Pass-through if the TypeVarAssignments are sane, i.e.:
//  - No recursive assignments
//  - TODO no duplicate assignments?
//  - TODO more?
sane :: ([TypeVarAssignment],[TypeVarAssignment]) -> Maybe ([TypeVarAssignment],[TypeVarAssignment])
sane (as1,as2)
| not $ and $ map noRecursiveAssignments $ as1++as2 = Nothing
// more?
| otherwise = Just (as1,as2)
where
    noRecursiveAssignments :: TypeVarAssignment -> Bool
    noRecursiveAssignments (tv,t) = not $ isMember tv $ allVars t

//-------------------//
// General utilities //
//-------------------//

// Apply a function to a tuple with elements of the same type
apptuple :: (a -> b) (a,a) -> (b,b)
apptuple f (x, y) = (f x, f y)

