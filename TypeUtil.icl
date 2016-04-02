implementation module TypeUtil

import TypeDef

import StdString
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

instance print Type
where
    print (Type s []) = if (s == "_String") ["String"] (print s)
    print (Type s vs)
    | s == "_List" = "[" <+ vs <+ "]"
    | s == "_String" = ["String"]
    | s == "_#Array"
        | vs == [Type "Char" []] = ["String"]
        | otherwise = "{#" <+ vs <+ "}"
    | s % (0,6) == "_Tuple" = "(" <+ vs <+ ")"
    | otherwise = "(" <+ s <+ " " <+ printersperse " " vs <+ ")"
    print (Var v) = [v]
    print (Func [] r []) = print r
    print (Func [] r cc) = r <+ " " <+ cc
    print (Func ts r []) = "(" <+ printersperse " " ts <+ " -> " <+ r <+ ")"
    print (Func ts r cc) = (Func ts r []) <+ " " <+ cc
    print (Cons tv []) = print tv
    print (Cons tv ats) = "(" <+ tv <+ " " <+ printersperse " " ats <+ ")"
    print (Uniq t) = "*" <+ t

