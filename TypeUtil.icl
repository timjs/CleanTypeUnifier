implementation module TypeUtil

import TypeDef

import StdArray, StdOrdList, StdString, StdTuple
from StdFunc import o

from Data.Func import $
import Data.List
import Data.Maybe
from Text import class Text (concat), instance Text String
from GenEq import generic gEq, ===

(--) infixr 1 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b
(+-) infixr 1 :: a b -> [String] | print a & print b
(+-) a b = print True a ++ print False b
(-+) infixr 1 :: a b -> [String] | print a & print b
(-+) a b = print False a ++ print True b
(+-+) infixr 1 :: a b -> [String] | print a & print b
(+-+) a b = print True a ++ print True b

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

instance toInt Bool where toInt True = 1; toInt False = 0

instance print String where print _ s = [s]
instance print Int where print _ i = [toString i]
instance print Char where print _ c = [{c}]
instance print [a] | print a
where print _ xs = [concat e \\ e <- map (print False) xs]

instance print (Maybe a) | print a
where print _ Nothing = []; print b (Just x) = print b x

instance print Kind
where
	print _ KindConst = ["*"]
	print b (KindArrow ks) = parlft -- printersperse True "->" (ks ++ [KindConst]) -- parrgt
	where (parlft,parrgt) = if b ("(",")") ("","")

instance print ClassOrGeneric
where
	print _ (Class s) = [s]
	print _ (Generic n k) = n -- "{|" -- k -- "|}"

instance print ClassRestriction where print _ (cog, v) = cog -- " " -- v

instance print ClassContext
where
	print _ [] = []
	print _ crs = "| " -- printersperse False " & "
		[printersperse False ", " (map fst gr) -- " " -- snd (hd gr) \\ gr <- grps]
	where
		grps = groupBy (\a b -> snd a == snd b) crs

instance print Type
where
	print isArg (Type s vs)
		// Lists
		| s == "_List"   = "[" -- vs -- "]"
		| s == "_!List"  = "[!" -- vs -- "]"
		| s == "_List!"  = "[" -- vs -- "!]"
		| s == "_!List!" = "[!" -- vs -- "!]"
		| s == "_#List"  = "[#" -- vs -- "]"
		| s == "_#List!" = "[#" -- vs -- "!]"
		// Arrays
		| s == "_#Array"
			| vs == [Type "Char" []]
			             = ["String"]
			| otherwise  = "{#" -- vs -- "}"
		| s == "_Array"  = "{" -- vs -- "}"
		| s == "_!Array" = "{!" -- vs -- "}"
		// Tuples
		| s % (0,5) == "_Tuple"
			# n = toInt (s % (6, size s - 1))
			| n > length vs = "((" -- repeatn (n-1) ',' -- ") " -- printersperse True " " vs -- ")"
			| otherwise     = "(" -- printersperse False ", " vs -- ")"
		// Other predefined types
		| s == "_Unit"   = ["()"]
		| s.[0] == '_'   = [s % (1, size s - 1)]
		// Other types
		| isEmpty vs     = print isArg s
		| otherwise      = parens isArg (s -- " " -- printersperse True " " vs)
	print _ (Var v) = [v]
	print ia (Func [] r []) = print ia r
	print _ (Func [] r cc) = r -- " " -- cc
	print ia (Func ts r []) = parens ia (printersperse True " " ts -- " -> " -- r)
	print _ (Func ts r cc) = (Func ts r []) -- " " -- cc
	print ia (Cons tv [])  = print ia tv
	print _ (Cons tv ats)  = "(" -- tv -- " " -- printersperse True " " ats -- ")"
	print _ (Uniq t)       = "*" -+ t
	print _ (Forall tvs t []) = "(A." -- printersperse True " " tvs -- ": " -- t -- ")"
	print _ (Forall tvs t cc) = "(A." -- printersperse True " " tvs -- ": " -- t -- " " -- cc -- ")"
	print _ (Arrow Nothing)  = ["(->)"]
	print _ (Arrow (Just t)) = "((->) " -+ t +- ")"

parens :: Bool [String] -> [String]
parens False ss = ss
parens True ss  = ["(":ss] ++ [")"]

instance print TypeDef
where
	print _ {td_name,td_uniq,td_args,td_rhs}
		= ":: " -- if td_uniq "*" "" -- td_name -- " " --
			printersperse True " " td_args -- if (isEmpty td_args) "" " " --
			case td_rhs of
				(TDRCons ext cs) = "= " -- makeADT ext cs
				(TDRRecord _ exi fields) = "= " --
					if (isEmpty exi) [] ("E." -- printersperse False " " exi -- ": ") --
					makeRecord exi fields
				(TDRSynonym t) = ":== " -- t
				TDRAbstract = []
				(TDRAbstractSynonym t) = "(:== " -- t -- ")"
	where
		indent = size td_name + toInt td_uniq + length td_args + sum (map (size o concat o print True) td_args)
		recordIndent exi = repeatn (indent + 6 + if (isEmpty exi) 0 (3 + length exi + sum (map size exi))) ' '
		consIndent = repeatn (indent + 4) ' '

		makeRecord :: [TypeVar] [RecordField] -> String
		makeRecord _ [] = "{}"
		makeRecord exi [f1:fs]
			= concat ("{ " -- printRf f1 -- "\n" --
				concat [concat (recordIndent exi -- ", " -- printRf f -- "\n")
				        \\ f <- fs] -- recordIndent exi -- "}")
		where
			padLen = maxList (map (\f -> size f.rf_name) [f1:fs])
			pad i s = s +++ toString (repeatn (i - size s) ' ')

			printRf {rf_name,rf_type} = pad padLen rf_name -- " :: " -- rf_type

		makeADT :: Bool [Constructor] -> String
		makeADT exten [] = if exten " .." ""
		makeADT False [c1:cs]
			= concat (c1 -- "\n" --
				concat [concat (consIndent -- "| " -- c -- "\n") \\ c <- cs])
		makeADT True cs = concat (makeADT False cs -- consIndent -- "| ..")

instance print Constructor
where
	print _ {cons_name,cons_args,cons_exi_vars=evars,cons_context}
		= if (isEmpty evars) [] ("E." -- printersperse False " " evars -- ": ") --
			cons_name -- " " -- printersperse True " " cons_args --
			if (isEmpty cons_context) [] (" & " -- cons_context)

propagate_uniqueness :: Type -> Type
propagate_uniqueness (Type t ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Type t ts)) (Type t ts)
propagate_uniqueness (Func is r cc)
	= Func (map propagate_uniqueness is) (propagate_uniqueness r) cc
propagate_uniqueness (Cons v ts)
	# ts = map propagate_uniqueness ts
	= if (any isUniq ts) (Uniq (Cons v ts)) (Cons v ts)
propagate_uniqueness (Forall vs t cc)
	= Forall vs (propagate_uniqueness t) cc
propagate_uniqueness t = t
