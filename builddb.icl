module builddb

// CleanTypeUnifier
import Type
import CoclUtils

// Standard libraries
import StdArray, StdBool, StdFile, StdList, StdMisc, StdString, StdFunc, StdTuple
import Data.Maybe, Text
import System.Directory, Data.Error, Data.Func, Data.Tuple

// frontend
//import Heap, compile, parse, predef
import Heap
from hashtable import ::HashTable, ::QualifiedIdents(NoQualifiedIdents), ::IdentClass(IC_Module), ::BoxedIdent{..}, putIdentInHashTable
from predef import init_identifiers
from compile import empty_cache, ::DclCache{hash_table}
from general import ::Optional(..)
from syntax import ::SymbolTable, ::SymbolTableEntry, ::Ident{..}, ::SymbolPtr, ::Position(NoPos), ::Module{mod_defs}, ::ParsedDefinition(PD_TypeSpec), ::FunSpecials, ::Priority, ::ParsedModule, ::SymbolType
from parse import wantModule

CLEAN_LIB :== "/opt/clean/lib/"
libraries :== [ 
              //  "StdEnv"
              //, "clean-platform/OS-Independent"
              /*,*/ "Generics"
              ]

Start w
# (mods, w) = findModules` libraries w //libraries w

# (st, w) = init_identifiers newHeap w
# cache = empty_cache st

= loop mods cache w
where
    loop :: [(String,String)] *DclCache *World -> *World
    loop [] _ w = w
    loop [(lib,mod):list] cache w
    # (sts, cache, w) = getModuleTypes mod lib cache w
    # ugrps = unigroups (==) sts
    # (io, w) = stdio w
    | isEmpty ugrps
        # io = fwrites (line +++ "\nSkipping " +++ mod +++ " in " +++ lib +++ " (no function types found)\n") io
        = loop list cache (snd (fclose io w))
    # io = fwrites (line +++ "\nParsing " +++ mod +++ " in " +++ lib +++ "\n" +++ line +++ "\n") io
    # io = fwrites (concat (join "\n" [alignl 42 (concat (print t)) <+ "\t" <+ join ", " ns \\ (ns, t) <- ugrps]) +++ "\n") io
    # (ok, w) = fclose io w
    | not ok = abort "Couldn't close stdio\n"
    = loop list cache w

    line = ("-----------------------------------------------------------" +++ 
            "-------------------------------------")

//              Libraries             Library Module
findModules` :: ![String] !*World -> *(![(String,String)], !*World)
findModules` [] w = ([], w)
findModules` [lib:libs] w
#! (mods, w) = findModules lib w
#! (moremods, w) = findModules` libs w
= (removeDup (mods ++ moremods), w)

findModules :: !String !*World -> *(![(String,String)], !*World)
findModules lib w
#! (fps, w) = readDirectory (CLEAN_LIB +++ lib) w
| isError fps = ([], w)
#! fps = fromOk fps
#! mods = map (tuple lib) $ map (\s->s%(0,size s-5)) $ filter endsInDcl fps
#! (moremods, w) = findModules` (map ((+++) (lib+++"/")) (filter isDirectory fps)) w
= (removeDup (mods ++ moremods), w)
where
    endsInDcl :: String -> Bool
    endsInDcl s = s % (size s - 4, size s - 1) == ".dcl"

    isDirectory :: String -> Bool
    isDirectory s = not $ isMember '.' $ fromString s

getModuleTypes :: String String *DclCache *World -> *([(String,Type)], *DclCache, *World)
getModuleTypes mod lib cache w
# filename = CLEAN_LIB +++ lib +++ "/" +++ mkdir mod +++ ".dcl"
# (ok,f,w) = fopen filename FReadText w
| not ok = abort ("Couldn't open file " +++ filename +++ ".\n")
//# (st, w) = init_identifiers newHeap w
//# cache = empty_cache st
# (mod_id, ht) = putIdentInHashTable mod (IC_Module NoQualifiedIdents) cache.hash_table
  cache = {cache & hash_table=ht}
# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" False mod_id.boxed_ident NoPos True cache.hash_table stderr) w
  cache = {cache & hash_table=ht}
# (ok,w) = fclose f w
| not ok = abort ("Couldn't close file " +++ filename +++ ".\n")
# pds = filter (\pd->case pd of (PD_TypeSpec _ _ _ _ _)=True; _=False) pm.mod_defs
# sts = map (\(PD_TypeSpec pos id prio st funspecs) -> (id.id_name,st)) pds
# sts = filter (\st->case st of (_,(Yes _))=True; _=False) sts
# sts = map (\(n,Yes x)->(n,toType x)) sts
= (sts,cache,w)
where
    mkdir :: String -> String
    mkdir s = toString (map (\c.case c of '.'='/'; c=c) (fromString s))

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

