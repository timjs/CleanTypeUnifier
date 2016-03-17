module coclparser

// CleanTypeUnifier
import Type
import CoclUtils

// Standard libraries
import StdArray, StdFile, StdList, StdMisc, StdString, StdFunc
import Data.Maybe, Text
import ArgEnv

// frontend
//import Heap, compile, parse, predef
import Heap
from hashtable import ::HashTable, ::QualifiedIdents(NoQualifiedIdents), ::IdentClass(IC_Module), ::BoxedIdent{..}, putIdentInHashTable
from predef import init_identifiers
from compile import empty_cache, ::DclCache{hash_table}
from general import ::Optional(..)
from syntax import ::SymbolTable, ::SymbolTableEntry, ::Ident{..}, ::SymbolPtr, ::Position(NoPos), ::Module{mod_defs}, ::ParsedDefinition(PD_TypeSpec), ::FunSpecials, ::Priority, ::ParsedModule, ::SymbolType
from parse import wantModule

default_lib :== "clean-platform/OS-Independent"
default_mod_id :== "Data.Func"

mkdir :: String -> String
mkdir s = toString (map (\c.case c of '.'='/'; c=c) (fromString s))

Start w
# args = [a \\ a <-: getCommandLine]
# mod_id = if (length args > 1) (args!!1) default_mod_id
# lib = if (length args > 2) (args!!2) default_lib
# filename = "/opt/clean/lib/" +++ lib +++ "/" +++ mkdir mod_id +++ ".dcl"
# (ok,f,w) = fopen filename FReadText w
| not ok = abort ("Couldn't open file " +++ filename +++ "\nUsage: ./coclparser -b [module [library]]\n")
# (st, w) = init_identifiers newHeap w
# cache = empty_cache st
# (mod_id, ht) = putIdentInHashTable mod_id (IC_Module NoQualifiedIdents) cache.hash_table
  cache = {cache & hash_table=ht}
# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" False mod_id.boxed_ident NoPos False ht stderr) w
# (ok,w) = fclose f w
# pds = filter (\pd->case pd of (PD_TypeSpec _ _ _ _ _)=True; _=False) pm.mod_defs
# sts = map (\(PD_TypeSpec pos id prio st funspecs) -> (id.id_name,st)) pds
# sts = filter (\st->case st of (_,(Yes _))=True; _=False) sts
# sts = map (\(n,Yes x)->(n,toType x)) sts
//= concat (join "\n" (map (\(n,t)->alignl 16 n <+ ":: " <+ print t) sts)) +++ "\n"
# ugrps = unigroups (\t u . isJust (unify t u)) sts
# ugrps = unigroups (==) sts
= concat (join "\n" [alignl 32 (concat (print t)) <+ "\t" <+ foldl (\a b.b +++ ", " +++ a) n ns \\ ([n:ns], t) <- ugrps]) +++ "\n"

unigroups :: (Type Type -> Bool) [(a,Type)] -> [([a],Type)]
unigroups f ts = unigroups` ts []
where
    unigroups` [] groups = groups
    unigroups` [(a,t):ts] [] = unigroups` ts [([a],t)]
    unigroups` [(a,t):ts] [(ns,ut):groups]
    | f t ut    = unigroups` ts [([a:ns],ut):groups]
    | otherwise = unigroups` ts [(ns,ut):unigroups` [(a,t)] groups]

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

join :: a [b] -> [String] | print a & print b
join _ [] = []
join a [b:[]] = print b
join a [b:bs] = b <+ a <+ join a bs

alignl :: Int a -> [String] | print a
alignl i s
# s = print s
# len = sum (map size s)
| len >= i = s
| otherwise = s ++ [{' ' \\ i <- [0..i-len]}]

wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
	-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
wantModule` f s b1 i p b2 ht io fs
# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
= ((b1,b2,pm,ht,f),fs)

