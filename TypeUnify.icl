implementation module TypeUnify

import TypeDef, TypeUtil

import StdOrdList

from StdFunc import o, flip
from StdMisc import abort
import StdBool
import StdList
import StdString
import StdTuple
import StdArray
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

derive gEq ClassOrGeneric, Type, Kind

prepare_unification :: !Bool !Type -> Type
prepare_unification b (Func [] t _) = prepare_unification b t
prepare_unification isleft t
# t = propagate_uniqueness t
# t = reduceArities t
# t = appendToVars (if isleft "l" "r") t
= t
where
	appendToVars :: String Type -> Type
	appendToVars s t = fromJust $ assignAll (map rename $ allVars t) t
	where rename v = (v, Var (v+++s))

finish_unification :: ![TVAssignment] -> Unifier
finish_unification tvs
# (tvs1, tvs2) = (filter (endsWith "l") tvs, filter (endsWith "r") tvs)
# (tvs1, tvs2) = (map removeEnds tvs1, map removeEnds tvs2)
= (tvs1, tvs2)
where
	endsWith :: String TVAssignment -> Bool
	endsWith n (h,_) = h % (size h - size n, size h - 1) == n

	removeEnds :: TVAssignment -> TVAssignment
	removeEnds (v,t) = let rm s = s % (0, size s - 2) in (rm v, fromJust $
	                   assignAll (map (\v->(v,Var (rm v))) $ allVars t) t)

unify :: ![Instance] !Type !Type -> Maybe [TVAssignment]
unify is t1 t2 //TODO instances ignored; class context not considered
	= unify2 $ toMESystem t1 t2

:: MultiEq = ME ![TypeVar] ![Type]

:: MESystem = { solved   :: ![MultiEq]
              , unsolved :: ![(Int, MultiEq)]
              }

allAssignments :: MultiEq -> [TVAssignment]
allAssignments (ME vs ts) = [(v,t) \\ v <- vs, t <- ts]

toMESystem :: !Type !Type -> MESystem
toMESystem t1 t2
	= { solved   = []
	  , unsolved = [(0, ME ["type"] [t1,t2])]
	  }

instance == MultiEq where (==) (ME a b) (ME c d) = a == c && b == d

// Implementation of 'Algorithm 3', described by Martelli, Montanari in An
// Efficient Unification Algorithm, 1982, section 2. It has been modified a bit
// to be able to deal with constructor variables and universal quantifiers.
unify2 :: !MESystem -> Maybe [TVAssignment]
unify2 {solved,unsolved}
# unsolved = sortBy (\(a,b) (c,d) -> a < c) unsolved
| isEmpty unsolved     = Just $ solution solved
# (count, me=:(ME vars types)) = hd unsolved
| count <> 0           = Nothing // cycle
# unsolved             = tl unsolved
| isEmpty types
	= unify2 {solved=[me:solved],unsolved=removeFromCounters vars unsolved}
# cPaF                 = commonPartAndFrontier types
| isNothing cPaF       = Nothing // clash
# (cPart,frontier)     = fromJust cPaF
// MultiEq reduction
# unsolved             = updateCounters $ compactify $
                         unsolved ++ [(0,f) \\ f <- frontier]
# solved               = [ME vars [cPart]:solved]
// Check universal quantifiers
# univars              = flatten $ map allUniversalVars types
| any
	(\(v,t) -> not (isVar t) && isMember v univars)
	(flatten (map (allAssignments o snd) unsolved))
                       = Nothing // Universally quantified var was assigned
= unify2 {solved=solved,unsolved=unsolved}
where
	solution :: [MultiEq] -> [TVAssignment]
	solution [] = []
	solution [ME [v:vs] ts:mes]
		= [(v,t) \\ t <- ts] ++ [(v`,Var v) \\ v` <- vs] ++ solution mes

	removeFromCounters :: ![TypeVar] ![(Int,MultiEq)] -> [(Int,MultiEq)]
	removeFromCounters vs [] = []
	removeFromCounters vs [(i,me=:(ME _ ts)):mes]
		= [(i - sum (map (count vs) ts),me):removeFromCounters vs mes]
	where
		count :: ![TypeVar] !Type -> Int
		count vs (Var v) = if (isMember v vs) 1 0
		count vs (Cons v ts) = if (isMember v vs) 1 0 + sum (map (count vs) ts)
		count vs (Type _ ts) = sum $ map (count vs) ts
		count vs (Func is r _) = sum $ map (count vs) [r:is]
		count vs (Uniq t) = count vs t

	updateCounters :: [(Int, MultiEq)] -> [(Int, MultiEq)]
	updateCounters eqs
		= [(sum (map (count mes) vars), me) \\ me=:(ME vars _) <- mes]
	where
		mes = map snd eqs

		count :: [MultiEq] TypeVar -> Int
		count mes v
			= sum [length (filter (isVarOrCons` v) (flatten (map subtypes ts)))
			       \\ (ME _ ts) <- mes]

	compactify :: [(Int, MultiEq)] -> [(Int, MultiEq)]
	compactify [] = []
	compactify [(c,ME vars types):mes] = case lookup vars mes of
		Nothing = [(c,ME vars types):compactify mes]
		(Just me=:(c`,ME vars` types`))
			# vars = removeDup $ vars ++ vars`
			# types = removeDup $ types ++ types`
			= compactify [(c+c`, ME vars types):removeMember me mes]
	where
		lookup :: [TypeVar] [(Int,MultiEq)] -> Maybe (Int, MultiEq)
		lookup vs [] = Nothing
		lookup vs [me=:(i,ME vars _):mes]
		| isEmpty (intersect vs vars) = lookup vs mes
		= Just me

:: CommonPart :== Type
:: Frontier :== [MultiEq]

commonPartAndFrontier :: [Type] -> Maybe (CommonPart, Frontier)
commonPartAndFrontier ts
| isEmpty ts = Nothing
| any isForall ts // TODO class context
	= commonPartAndFrontier $ map (\t -> if (isForall t) (fromForall t) t) ts
| any isVar ts = Just (hd $ filter isVar ts, makemulteq ts)
| all isType ts
	# names = map (\(Type n _) -> n) ts
	| (<>) 1 $ length $ removeDup $ names = Nothing
	# name = hd names
	# lengths = map (length o (\(Type _ ts) -> ts)) ts
	| (<>) 1 $ length $ removeDup $ lengths = Nothing
	# len = hd lengths
	| len == 0 = Just (Type name [], [])
	# args = map (\(Type _ ts) -> ts) ts
	# cpafs = [commonPartAndFrontier [a!!i \\ a <- args] \\ i <- [0..len-1]]
	| any isNothing cpafs = Nothing
	# (cps, fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	= Just (Type name cps, flatten fronts)
| all isFunc ts // TODO class context
	# types = map (\(Func is t _) -> [t:is]) ts
	# lengths = map length types
	| (<>) 1 $ length $ removeDup $ lengths = Nothing
	# len = hd lengths
	# cpafs = [commonPartAndFrontier [t!!i \\ t <- types] \\ i <- [0..len-1]]
	| any isNothing cpafs = Nothing
	# ([cp:cps], fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	= Just (Func cps cp [], flatten fronts)
| all isCons ts
	# lengths = [length ts \\ (Cons _ ts) <- ts]
	| 1 == length (removeDup lengths)
		// All same arity, pairwise unification
		# len = hd lengths
		# lists = map (\(Cons v ts) -> [Var v:ts]) ts
		# cpafs = [commonPartAndFrontier [l!!i \\ l <- lists] \\ i <- [0..len]]
		| any isNothing cpafs = Nothing
		# ([Var cp:cps], fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
		= Just (Cons cp cps, flatten fronts)
	// Different arities, curry in some arguments
	# (minlen,maxlen) = (minList lengths, maxList lengths)
	# maxvar = hd [v \\ (Cons v ts) <- ts | length ts == maxlen]
	# splits = [splitAt (length ts - minlen) ts \\ (Cons v ts) <- ts]
	# types = [if (isEmpty init) (Var v) (Cons v init) \\ (init,_) <- splits & (Cons v _) <- ts]
	# rests = map snd splits
	# cpafs = [commonPartAndFrontier [r!!i \\ r <- rests] \\ i <- [0..minlen - 1]]
	| any isNothing cpafs = Nothing
	# (cps, fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	# cpaf = commonPartAndFrontier types
	| isNothing cpaf = Nothing
	# (cp, front) = fromJust cpaf
	| isCons cp
		# (Cons cpv cpts) = cp
		= Just (Cons cpv (cpts ++ cps), flatten [front:fronts])
	# (Var v) = cp
	= Just (Cons v cps, flatten [front:fronts])
| all (\t -> isCons t || isType t) ts
	# types = filter isType ts
	# conses = filter isCons ts
	// Unify types separately
	# cpaft = commonPartAndFrontier types
	| isNothing cpaft = Nothing
	# (cpt=:(Type cptn cptts), frontt) = fromJust cpaft
	// Unify conses separately
	# cpafc = commonPartAndFrontier conses
	| isNothing cpafc = Nothing
	# (cpc, frontc) = fromJust cpafc
	// Merge results
	| isVar cpc = let (Var cpc`) = cpc in
		Just (cpt, [ME [cpc`] [cpt]] ++ frontt ++ frontc)
	# (Cons cpcv cpcts) = cpc
	| length cpcts > length cptts = Nothing
	# (cptts_curry, cptts_unify) = splitAt (length cptts - length cpcts) cptts
	# cpafs = [commonPartAndFrontier [t,c] \\ t <- cptts_unify & c <- cpcts]
	| any isNothing cpafs = Nothing
	# (cps,fronts) = let cfs = map fromJust cpafs in (map fst cfs, map snd cfs)
	# cps = cptts_curry ++ cps
	| isEmpty cps = Just (Var cpcv, flatten fronts ++ frontt ++ frontc)
	= Just (Cons cpcv cps, flatten fronts ++ [ME [cpcv] [Type cptn cptts_curry]] ++ frontt ++ frontc)
| all isUniq ts
	= (\(cpaf,front) -> (Uniq cpaf,front))
		<$> commonPartAndFrontier (map (\(Uniq t) -> t) ts)
| otherwise = Nothing
where
	makemulteq :: [Type] -> Frontier
	makemulteq ts = let (vs,ts`) = partition isVar ts in [ME (map fromVar vs) ts`]

//-----------------------//
// Unification utilities //
//-----------------------//

// Apply a TVAssignment to a Type
assign :: !TVAssignment !Type -> Maybe Type
assign va (Type s ts) = Type s <$^> map (assign va) ts
assign va (Func ts r cc) = Func <$^> map (assign va) ts 
		>>= (\f->f <$> assign va r) >>= (\f->pure $ f cc) // TODO cc
assign (v,a) (Var v`) = pure $ if (v == v`) a (Var v`)
assign va=:(v,Type s ts) (Cons v` ts`)
	| v == v`   = Type s <$^> map (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$^> map (assign va) ts`
assign va=:(v,Cons c ts) (Cons v` ts`)
	| v == v`   = Cons c <$^> map (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$^> map (assign va) ts`
assign va=:(v,Var v`) (Cons v`` ts)
	| v == v``  = Cons v` <$^> map (assign va) ts
	| otherwise = Cons v`` <$^> map (assign va) ts
assign va=:(v,_) (Cons v` ts)
	| v == v` = empty
	| otherwise = Cons v` <$^> map (assign va) ts
assign va (Uniq t) = Uniq <$> (assign va t)
assign va=:(v,Var v`) (Forall tvs t cc)
	= Forall <$^> map (assign va) tvs >>= (\f -> flip f cc <$> assign va t)
assign va=:(v,_) (Forall tvs t cc)
	| isMember (Var v) tvs = empty
	| otherwise = flip (Forall tvs) cc <$> assign va t

(<$^>) infixl 4 //:: ([a] -> b) [Maybe a] -> Maybe b
(<$^>) f mbs :== ifM (all isJust mbs) $ f $ map fromJust mbs

//ifM :: Bool a -> m a | Alternative m
ifM b x :== if b (pure x) empty

// Make all functions arity 1 by transforming a b -> c to a -> b -> c
reduceArities :: !Type -> Type
reduceArities (Func ts r cc)
	| length ts > 1 = Func [reduceArities $ hd ts] (reduceArities $ Func (tl ts) r cc) cc
	| otherwise = Func (map reduceArities ts) (reduceArities r) cc
reduceArities (Type s ts) = Type s $ map reduceArities ts
reduceArities (Cons v ts) = Cons v $ map reduceArities ts
reduceArities (Uniq t) = Uniq $ reduceArities t
reduceArities (Var v) = Var v
reduceArities (Forall tvs t cc) = Forall tvs (reduceArities t) cc
