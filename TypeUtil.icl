implementation module TypeUtil

import TypeDef

from Data.Func import $
import Data.List
from Text import class Text (concat), instance Text String
from GenEq import generic gEq, ===

(<+) infixr 1 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

printersperse :: a [b] -> [String] | print a & print b
printersperse a bs = intercalate (print a) (map print bs)

instance print String where print s = [s]

instance print [a] | print a where print xs = [concat e \\ e <- map print xs]

instance print ListKind
where
    print HeadStrict = ["!"]
    print HeadStrictUnboxed = ["#"]
    print NormalList = []

instance print SpineStrictness
where
    print SpineStrict = ["!"]
    print NormalSpine = []

instance print Strict
where
    print Strict = ["!"]
    print NotStrict = []

instance print ArrayKind
where
    print ArrayStrict = ["!"]
    print ArrayUnboxed = ["#"]
    print NormalArray = []

instance print ClassOrGeneric
where
    print (Class s) = [s]
    //TODO generic?

instance print ClassRestriction
where
    print (cog, v) = [concat (cog <+ " " <+ v)]

instance print ClassContext
where
    print [] = []
    print crs = "| " <+ printersperse " & " crs

instance print (Strict, Type)
where
    print (s,t) = [concat (s <+ t)]

instance print Type
where
    print (Type s []) = print s
    print (Type s vs) = "(" <+ s <+ " " <+ printersperse " " vs <+ ")"
    print (List k t s) = "[" <+ k <+ t <+ s <+ "]"
    print (Tuple ts) = "(" <+ printersperse "," ts <+ ")"
    print (Array k t) = "{" <+ k <+ t <+ "}"
    print (Var v) = [v]
    print (Func [] r []) = print r
    print (Func [] r cc) = r <+ " " <+ cc
    print (Func ts r []) = "(" <+ printersperse " " ts <+ " -> " <+ r <+ ")"
    print (Func ts r cc) = (Func ts r []) <+ " " <+ cc
    print (Cons tv []) = print tv
    print (Cons tv ats) = "(" <+ tv <+ " " <+ printersperse " " ats <+ ")"
    print (Uniq t) = "*" <+ t

