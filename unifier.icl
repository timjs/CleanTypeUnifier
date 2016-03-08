module unifier

import Type
from Text import class Text (concat), instance Text String

//Start = unify (List NormalList (Var "x") NormalSpine) (List NormalList (Type "Y" ["a"]) NormalSpine)
Start = (print type, concat (print type))
where
    //type = Function [Var "a", Var "b"] (Type "String" []) [(Class "toString", "a"), (Class "toString", "b")]
    //type = Function [] (Type "Int" []) []
    //type = List NormalList (Tuple [(Strict,Var "a"),(NotStrict,Type "Int" [])]) NormalSpine
    //type = (Strict, Var "a")
    type = Function [Var "a", Var "b"] (Type "Vector" ["a","b"]) [(Class "toReal", "a"), (Class "toString", "b")]

