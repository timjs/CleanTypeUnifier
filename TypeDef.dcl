definition module TypeDef

from StdOverloaded import class ==
from Data.Maybe import :: Maybe

:: Type = Type String [Type]             // Concrete type + arguments
        | List ListKind Type SpineStrictness // List
        | Tuple [(Strict, Type)]         // Tuple
        | Array ArrayKind Type           // Array
        | Func [Type] Type ClassContext  // Function; TODO UnqTypeUnEqualities
        | Var TypeVar                    // Type variable
        | Cons TypeVar [Type]            // Constructor variable + arguments
        | Uniq Type                      // Unique type

:: TypeVar :== String
:: TypeVarAssignment :== (TypeVar, Type)

:: ListKind = HeadStrict | HeadStrictUnboxed | NormalList
:: SpineStrictness = SpineStrict | NormalSpine
:: Strict = Strict | NotStrict
:: ArrayKind = ArrayStrict | ArrayUnboxed | NormalArray
:: ClassContext :== [ClassRestriction]
:: ClassRestriction :== (ClassOrGeneric, TypeVar)
:: ClassOrGeneric = Class String | Generic //TODO generic?
:: Instance = Instance String Type

instance == Type, Instance

class toType a :: a -> Type
class toTypeVar a :: a -> TypeVar

