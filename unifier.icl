module unifier

import Type
from Text import class Text (concat), instance Text String

//Start = unify (List NormalList (Var "x") NormalSpine) (List NormalList (Type "Y" ["a"]) NormalSpine)
Start = unify (Tuple [(Strict,Var "a"),(Strict,Var "a")]) (Tuple [(Strict,Type "Int" []),(Strict,Type "Real" [])])
//Start = assign ("a", Type "String" []) (Tuple [(Strict,Var "a"),(Strict,Var "a")])
//Start = concat (print type)
//where
//    //type = Func [Var "a", Var "b"] (Type "String" []) [(Class "toString", "a"), (Class "toString", "b")]
//    //type = Func [] (Type "Int" []) []
//    //type = List NormalList (Tuple [(Strict,Var "a"),(NotStrict,Type "Int" [])]) NormalSpine
//    //type = (Strict, Var "a")
//    type = Func [Var "a", Var "b"] (Type "Vector" ["a","b"]) [(Class "toReal", "a"), (Class "toString", "b")]

