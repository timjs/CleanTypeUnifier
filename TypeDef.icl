implementation module TypeDef

import StdBool
from StdFunc import o
import StdList
import StdString
import StdTuple
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
from GenEq import generic gEq

derive gEq ListKind, SpineStrictness, Strict, ArrayKind, ClassOrGeneric, Type

instance == Type where (==) a b = a === b // needed for removeDup

unifyEq a b :== if (a === b) (Just []) Nothing
instance unify ArrayKind where unify a b = unifyEq a b
instance unify ListKind where unify a b = unifyEq a b
instance unify SpineStrictness where unify a b = unifyEq a b
instance unify Strict where unify a b = unifyEq a b

instance unify [Type]
where
    unify ts1 ts2 = removeDup <$> foldM (\tvas (t1,t2) ->
            let (t1`,t2`) = (assignAll tvas t1, assignAll tvas t2) in
            ((++)tvas) <$> unify t1` t2`) [] (zip2 ts1 ts2)

instance unify Type
where
    unify a=:(Var a`) b=:(Var b`) = Just (removeDup [(a`,b), (b`,a)])
    unify (Var a`) b = Just [(a`, b)]
    unify (Type t1 vs1) (Type t2 vs2)
    | t1 == t2 = unify (map Var vs1) (map Var vs2)
    | otherwise = Nothing
    unify (List k1 t1 s1) (List k2 t2 s2)
        = unify k1 k2 >>| unify s1 s2 >>| unify t1 t2
    unify (Tuple ts1) (Tuple ts2)
        | length ts1 <> length ts2 = Nothing
        = foldM (\tvas ((s1,t1),(s2,t2)) ->
            let (t1`,t2`) = (assignAll tvas t1, assignAll tvas t2) in
            ((++)tvas) <$> (unify s1 s2 >>| unify t1` t2`)) [] (zip2 ts1 ts2)
    unify (Array k1 t1) (Array k2 t2)
        = unify k1 k2 >>| unify t1 t2
    unify (Func ts1 r1 cc1) (Func ts2 r2 cc2)
        | length ts1 <> length ts2 = Nothing
        = foldM (\tvas (t1,t2) ->
            let (t1`,t2`) = (assignAll tvas t1, assignAll tvas t2) in
            ((++)tvas) <$> unify t1` t2`) [] (zip2 [r1:ts1] [r2:ts2]) //TODO unify class context
    unify (Uniq t1) (Uniq t2) = unify t1 t2
    unify _ _ = Nothing

assign :: TypeVarAssignment Type -> Type // replace arg1 with arg2 in arg3
assign va=:(v,a) (Var v`) = if (v == v`) a (Var v`)
assign va=:(v,a) (List k t s) = List k (assign va t) s
assign va=:(v,a) (Tuple ts) = Tuple (map (\(s,t)->(s,assign va t)) ts)
assign va=:(v,a) (Array k t) = Array k (assign va t)
assign va=:(v,a) (Func ts r cc) = Func (map (assign va) ts) (assign va r) cc
assign va=:(v,a) (Uniq t) = Uniq (assign va t)
assign _ t = t

assignAll :: [TypeVarAssignment] Type -> Type
assignAll vas t = foldr assign t vas

