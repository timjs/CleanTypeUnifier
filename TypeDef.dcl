definition module TypeDef

from StdOverloaded import class ==
from Data.Maybe import :: Maybe

:: Type = Type String [Type]             // Concrete type + arguments
        | Func [Type] Type ClassContext  // Function; TODO UnqTypeUnEqualities
        | Var TypeVar                    // Type variable
        | Cons TypeVar [Type]            // Constructor variable + arguments
        | Uniq Type                      // Unique type

:: TypeVar :== String
:: TVAssignment :== (TypeVar, Type)

:: ClassContext :== [ClassRestriction]
:: ClassRestriction :== (ClassOrGeneric, TypeVar)
:: ClassOrGeneric = Class String | Generic //TODO generic?

:: Instance = Instance String Type

instance == Type, Instance

class toType a :: a -> Type
class toTypeVar a :: a -> TypeVar

subtypes :: Type -> [Type]
allVars :: Type -> [TypeVar]

isVar :: Type -> Bool
fromVar :: Type -> TypeVar

isCons :: Type -> Bool
isCons` :: TypeVar Type -> Bool

arity :: Type -> Int

