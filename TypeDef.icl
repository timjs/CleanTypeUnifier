implementation module TypeDef

import StdBool
from StdFunc import o
import StdList
import StdString
import StdTuple
from Data.Func import $
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
from GenEq import generic gEq

derive gEq ListKind, SpineStrictness, Strict, ArrayKind, ClassOrGeneric, Type

instance unify ArrayKind where unify a b = if (a === b) (Just []) Nothing
instance unify ListKind where unify a b = if (a === b) (Just []) Nothing
instance unify SpineStrictness where unify a b = if (a === b) (Just []) Nothing

instance unify Type
where
    unify a=:(Var a`) b=:(Var b`) = Just [(a`,b), (b`,a)]
    unify (Var a`) b = Just [(a`, b)]
    unify (List k1 t1 s1) (List k2 t2 s2)
        = unify k1 k2 >>| unify s1 s2 >>| unify t1 t2
    unify (Tuple ts1) (Tuple ts2)
        = foldM (\tvas ((s1,t1),(s2,t2)) . 
            if (s1===s2) (unify t1 t2) Nothing >>| pure tvas) [] (zip2 ts1 ts2)
    unify (Array k1 t1) (Array k2 t2)
        = unify k1 k2 >>| unify t1 t2
    unify (Function ts1 r1 cc1) (Function ts2 r2 cc2)
        = Nothing //TODO
    unify (Uniq t1) (Uniq t2) = unify t1 t2
    unify _ _ = Nothing

