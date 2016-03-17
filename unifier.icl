module unifier

import Type
from Text import class Text (concat), instance Text String

a :== Var "a"
list a :== List NormalList a NormalSpine

filtertype = Func [Func [a] (Type "Bool" []) [], list a] (list a) []
functype = Func [Type "Bool" []] (a) []

constype = Cons "t" [Var "a", Type "Bool" []]
constype` = Cons "u" [Type "Int" [], Var "a"]

fmapDefaultType = Func [Func [Var "a"] (Var "b") [], Cons "t" [Var "a"]] (Cons "t" [Var "b"]) []
foldMapDefaultType = Func [Func [Var "a"] (Var "m") [], Cons "t" [Var "a"]] (Var "m") []

//Start = unify fmapDefaultType foldMapDefaultType
Start = unify constype constype`
//Start = assignAll [("a",Var "X")] (filtertype)
//Start = unify filtertype filtertype
//Start = unify (List NormalList (Var "a") NormalSpine) (Var "a")
//Start = unify (List NormalList (Var "x") NormalSpine) (List NormalList (Type "Y" ["a"]) NormalSpine)
//Start = unify (Tuple [(Strict,Var "a"),(Strict,Var "a")]) (Tuple [(Strict,Type "Int" []),(Strict,Type "Real" [])])
//Start = assign ("a", Type "String" []) (Tuple [(Strict,Var "a"),(Strict,Var "a")])
//Start = concat (print type)
//where
//    //type = Func [Var "a", Var "b"] (Type "String" []) [(Class "toString", "a"), (Class "toString", "b")]
//    //type = Func [] (Type "Int" []) []
//    //type = List NormalList (Tuple [(Strict,Var "a"),(NotStrict,Type "Int" [])]) NormalSpine
//    //type = (Strict, Var "a")
//    type = Func [Var "a", Var "b"] (Type "Vector" ["a","b"]) [(Class "toReal", "a"), (Class "toString", "b")]

