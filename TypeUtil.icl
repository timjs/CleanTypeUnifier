implementation module TypeUtil

import TypeDef

import StdArray, StdString, StdTuple

from Data.Func import $
import Data.List
import Data.Maybe
from Text import class Text (concat), instance Text String
from GenEq import generic gEq, ===

(<+) infixr 1 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

printersperse :: a [b] -> [String] | print a & print b
printersperse a bs = intercalate (print a) (map print bs)

instance print String where print s = [s]
instance print Int where print i = [toString i]

instance print [a] | print a where print xs = [concat e \\ e <- map print xs]

instance print (Maybe a) | print a
where print Nothing = []; print (Just x) = print x

instance print ClassOrGeneric
where
	print (Class s) = [s]
	//TODO generic?

instance print ClassRestriction
where
	print (cog, v) = cog <+ " " <+ v

instance print ClassContext
where
	print [] = []
	print crs = "| " <+ printersperse " & " [printersperse ", " (map fst grp) <+ " " <+ snd (hd grp) \\ grp <- grps]
	where
		grps = groupBy (\a b -> snd a == snd b) $ filter isClassRestriction crs

instance print Type
where
	print (Type s vs)
		// Lists
		| s == "_List"   = "[" <+ vs <+ "]"
		| s == "_!List"  = "[!" <+ vs <+ "]"
		| s == "_List!"  = "[" <+ vs <+ "!]"
		| s == "_!List!" = "[!" <+ vs <+ "!]"
		| s == "_#List"  = "[#" <+ vs <+ "]"
		| s == "_#List!" = "[#" <+ vs <+ "!]"
		// Arrays
		| s == "_#Array"
			| vs == [Type "Char" []]
			             = ["String"]
			| otherwise  = "{#" <+ vs <+ "}"
		| s == "_Array"  = "{" <+ hd vs <+ "}"
		// Tuples
		| s % (0,5) == "_Tuple"
		                 = "(" <+ printersperse ", " vs <+ ")"
		// Other predefined types
		| s == "_Unit"   = ["()"]
		| s.[0] == '_'   = [s % (1, size s - 1)]
		// Other types
		| isEmpty vs     = print s
		| otherwise      = "(" <+ s <+ " " <+ printersperse " " vs <+ ")"
	print (Var v) = [v]
	print (Func [] r []) = print r
	print (Func [] r cc) = r <+ " " <+ cc
	print (Func ts r []) = "(" <+ printersperse " " ts <+ " -> " <+ r <+ ")"
	print (Func ts r cc) = (Func ts r []) <+ " " <+ cc
	print (Cons tv [])   = print tv
	print (Cons tv ats)  = "(" <+ tv <+ " " <+ printersperse " " ats <+ ")"
	print (Uniq t)       = "*" <+ t

propagate_uniqueness :: Type -> Type
propagate_uniqueness (Type t ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Type t ts)) (Type t ts)
propagate_uniqueness (Func is r cc)
	= Func (map propagate_uniqueness is) (propagate_uniqueness r) cc
propagate_uniqueness (Cons v ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Cons v ts)) (Cons v ts)
propagate_uniqueness t = t
