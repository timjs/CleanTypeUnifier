implementation module TypeUtil

import TypeDef

import StdArray, StdOrdList, StdString, StdTuple

from Data.Func import $
import Data.List
import Data.Maybe
from Text import class Text (concat), instance Text String
from GenEq import generic gEq, ===

(<+) infixr 1 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

printersperse :: a [b] -> [String] | print a & print b
printersperse a bs = intercalate (print a) (map print bs)

instance toInt Bool where toInt True = 1; toInt False = 0

instance print String where print s = [s]
instance print Int where print i = [toString i]
instance print Char where print c = [{c}]

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

instance print TypeDef
where
	print {td_name,td_uniq,td_args,td_rhs}
		= ":: " <+ if td_uniq "*" "" <+ td_name <+ " " <+
			printersperse " " td_args <+ if (isEmpty td_args) "" " " <+
			case td_rhs of
				(TDRCons ext cs) = "= " <+ makeADT ext cs
				(TDRRecord _ exi fields) = "= " <+ 
					if (isEmpty exi) [] ("E." <+ printersperse " " exi <+ ": ") <+
					makeRecord exi fields
				(TDRSynonym t) = ":== " <+ t
				TDRAbstract = []
				(TDRAbstractSynonym t) = "(:== " <+ t <+ ")"
	where
		indent = size td_name + toInt td_uniq + 2 * length td_args
		recordIndent exi = repeatn (indent + 6 + if (isEmpty exi) 0 (3 + length exi + sum (map size exi))) ' '
		consIndent = repeatn (indent + 4) ' '

		makeRecord :: [TypeVar] [RecordField] -> String
		makeRecord _ [] = "{}"
		makeRecord exi [f1:fs]
			= concat ("{ " <+ printRf f1 <+ "\n" <+
				concat [concat (recordIndent exi <+ ", " <+ printRf f <+ "\n")
				        \\ f <- fs] <+ recordIndent exi <+ "}")
		where
			padLen = maxList (map (\f -> size f.rf_name) [f1:fs])
			pad i s = s +++ toString (repeatn (i - size s) ' ')

			printRf {rf_name,rf_type} = pad padLen rf_name <+ " :: " <+ rf_type

		makeADT :: Bool [Constructor] -> String
		makeADT True []  = " .."
		makeADT False [] = ""
		makeADT False [c1:cs]
			= concat (c1 <+ "\n" <+
				concat [concat (consIndent <+ "| " <+ c <+ "\n") \\ c <- cs])
		makeADT True cs = concat (makeADT False cs <+ consIndent <+ "| ..")

instance print Constructor
where
	print {cons_name,cons_args,cons_exi_vars=evars,cons_context}
		= if (isEmpty evars) [] ("E." <+ printersperse " " evars <+ ": ") <+
			cons_name <+ " " <+ printersperse " " cons_args <+
			if (isEmpty cons_context) [] (" & " <+ cons_context)

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
