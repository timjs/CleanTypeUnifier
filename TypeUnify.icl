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
instance unify ArrayKind where unify _ a b = unifyEq a b
instance unify ListKind where unify _ a b = unifyEq a b
instance unify SpineStrictness where unify _ a b = unifyEq a b
instance unify Strict where unify _ a b = unifyEq a b

instance unify [Type]
where
    unify is ts1 ts2 
    | length ts1 <> length ts2 = Nothing
    | otherwise = apptuple removeDup <$> foldM (\(as1,as2) (t1,t2) ->
        let (t1`,t2`) = (assignAll as1 t1, assignAll as2 t2) in
          (\(a,b)->(a++as1, b++as2)) <$> unify is t1` t2`) ([],[]) (zip2 ts1 ts2)

instance unify Type
where
    unify _ a=:(Var a`) b=:(Var b`) = Just ([(a`,b)], [(b`,a)]) >>= sane
    unify _ (Var a) b = Just ([(a, b)],[]) >>= sane
    unify _ a (Var b) = Just ([],[(b, a)]) >>= sane
    unify is (Type t1 vs1) (Type t2 vs2)
        = if (t1==t2) (unify is vs1 vs2 >>= sane) Nothing
    unify is (List k1 t1 s1) (List k2 t2 s2)
        = unify is k1 k2 >>| unify is s1 s2 >>| unify is t1 t2 >>= sane
    unify is (Tuple ts1) (Tuple ts2)
        = unify is (map snd ts1) (map snd ts2) >>= sane//TODO unify strictness?
    unify is (Array k1 t1) (Array k2 t2)
        = unify is k1 k2 >>| unify is t1 t2 >>= sane
    unify is (Func ts1 r1 cc1) (Func ts2 r2 cc2)
        = unify is [r1:ts1] [r2:ts2] >>= sane //TODO unify class context
    unify is (Uniq t1) (Uniq t2) = unify is t1 t2 >>= sane
    unify is (Cons c1 ts1) (Cons c2 ts2)
        = unify is [Var c1:ts1] [Var c2:ts2] >>= sane
    unify _ _ _ = Nothing

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
sane :: ([TypeVarAssignment],[TypeVarAssignment])
        -> Maybe ([TypeVarAssignment],[TypeVarAssignment])
sane (as1,as2)
| not $ and $ map noRecursiveAssignments $ as1++as2 = Nothing
// more?
| otherwise = Just (as1,as2)
where
    noRecursiveAssignments :: TypeVarAssignment -> Bool
    noRecursiveAssignments (tv,t) = not $ isMember tv $ allVars t

saneCC :: ClassContext [Instance] ([TypeVarAssignment],[TypeVarAssignment])
          -> Maybe ([TypeVarAssignment],[TypeVarAssignment])
saneCC [] _ ass = Just ass
saneCC [cc:ccs] is (as1,as2)
| saneCC` cc is as1 = saneCC ccs is (as1,as2)
| otherwise = Nothing
where
    saneCC` :: ClassRestriction [Instance] [TypeVarAssignment] -> Bool
    saneCC` (Class c, tv) is as1
    # ts = map snd $ filter ((==) tv o fst) as1
    | isEmpty ts = True
    | otherwise = not $ isEmpty $ filter ((==)(Instance c (hd ts))) is
    saneCC` _ _ _ = False // TODO saneCC` for generics not implemented

//-------------------//
// General utilities //
//-------------------//

// Apply a function to a tuple with elements of the same type
apptuple :: (a -> b) (a,a) -> (b,b)
apptuple f (x, y) = (f x, f y)

