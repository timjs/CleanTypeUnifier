implementation module Type

import StdBool
from StdFunc import o
import StdList
import StdString
import StdTuple
import Data.Maybe
import Control.Applicative
import Control.Monad
from GenEq import generic gEq

derive gEq ListKind
derive gEq SpineStrictness
derive gEq Strict
derive gEq ArrayKind
derive gEq ClassOrGeneric
derive gEq Type
instance == ListKind            where (==) x y = gEq{|*|} x y
instance == SpineStrictness     where (==) x y = gEq{|*|} x y
instance == Strict              where (==) x y = gEq{|*|} x y
instance == ArrayKind           where (==) x y = gEq{|*|} x y
instance == ClassOrGeneric      where (==) x y = gEq{|*|} x y
instance == Type                where (==) x y = gEq{|*|} x y

(<+) infixr 1 :: a b -> String | toString a & toString b
(<+) a b = toString a +++ toString b

instance toString ListKind
where
    toString HeadStrict = "!"
    toString HeadStrictUnboxed = "#"
    toString NormalList = ""

instance toString SpineStrictness
where
    toString SpineStrict = "!"
    toString NormalSpine = ""

instance toString Strict
where
    toString Strict = "!"
    toString NotStrict = ""

instance toString ArrayKind
where
    toString ArrayStrict = "!"
    toString ArrayUnboxed = "#"
    toString NormalArray = ""

instance toString ClassOrGeneric
where
    toString (Class s) = s
    //TODO generic?

instance toString ClassRestriction
where
    toString (cog, v) = cog <+ " " <+ v

instance toString ClassContext
where
    toString [] = ""
    toString [cr:crs] = foldl (+++) ("| " <+ cr) (map ((+++)" & " o toString) crs)

instance toString (Strict, Type)
where
    toString (s,t) = s <+ t

instance toString Type
where
    toString (Type s vs) = foldl (+++) s (map ((+++)" ") vs)
    toString (List k t s) = "[" <+ k <+ t <+ s <+ "]"
    toString (Tuple [t:ts]) =  "(" <+ foldl (+++) (toString t) (map ((+++)"," o toString) ts) <+ ")"
    toString (Array k t) = "{" <+ k <+ t <+ "}"
    toString (Var v) = v
    toString (Function [] r []) = toString r
    toString (Function [] r cc) = r <+ " " <+ cc
    toString (Function [t:ts] r []) = foldl (+++) (toString t) (map ((+++)" " o toString) ts) <+ " -> " <+ r
    toString (Function ts r cc) = (Function ts r []) <+ " " <+ cc
    toString (Uniq t) = "*" <+ t

instance unify ArrayKind
where
    unify a b
    | a == b = Just []
    = Nothing

instance unify ListKind
where
    unify a b
    | a == b = Just []
    = Nothing

instance unify SpineStrictness
where
    unify a b
    | a == b = Just []
    = Nothing

instance unify Type
where
    unify a=:(Var a`) b=:(Var b`) = Just [(a`,b), (b`,a)]
    unify (Var a`) b = Just [(a`, b)]
    unify (List k1 t1 s1) (List k2 t2 s2) = unify k1 k2 >>| unify s1 s2 >>| unify t1 t2
    unify (Array k1 t1) (Array k2 t2) = unify k1 k2 >>| unify t1 t2
    unify _ _ = Nothing

