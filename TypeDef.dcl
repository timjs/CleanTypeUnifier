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
:: Unifier :== ([TVAssignment], [TVAssignment])

:: ClassContext :== [ClassRestriction]
:: ClassRestriction :== (ClassOrGeneric, Type)
:: ClassOrGeneric = Class String | Generic //TODO generic?

:: Instance = Instance String Type

:: TypeDef = { td_name :: String         // Name of the type
             , td_uniq :: Bool           // Whether the type is unique
             , td_args :: [Type]         // Var or Uniq Var; arguments
             , td_rhs  :: TypeDefRhs     // Right hand side
             }

:: TypeDefRhs = TDRCons
                  Bool                   // Extensible?
                  [Constructor]          // List of constructors
              | TDRRecord
                  String                 // Identifier used internally
                  [TypeVar]              // Existentially qualified variables
                  [RecordField]          // Fields
              | TDRSynonym Type          // Type synonym
              | TDRAbstract              // Abstract type
              | TDRAbstractSynonym Type  // Abstract synonym (sec 5.4.1)

:: Constructor = { cons_name     :: String
                 , cons_args     :: [Type]
                 , cons_exi_vars :: [TypeVar]
                 , cons_context  :: ClassContext
                 }

:: RecordField = { rf_name :: String
                 , rf_type :: Type
                 }

instance == Type, Instance

class toType a :: a -> Type
class toTypeVar a :: a -> TypeVar

class toTypeDef a :: a -> TypeDef
class toTypeDefRhs a :: a -> TypeDefRhs
class toConstructor a :: a -> Constructor
class toRecordField a :: a -> RecordField

subtypes :: Type -> [Type]
allVars :: Type -> [TypeVar]

isVar :: Type -> Bool
fromVar :: Type -> TypeVar

isCons :: Type -> Bool
isCons` :: TypeVar Type -> Bool

isVarOrCons` :: TypeVar Type -> Bool

isType :: Type -> Bool

isFunc :: Type -> Bool

isUniq :: Type -> Bool

arity :: Type -> Int

isClass :: ClassOrGeneric -> Bool
isClassRestriction :: (ClassRestriction -> Bool)

constructorsToFunctions :: TypeDef -> [(String,Type)]

// Record wrappers; needed in CoclUtils because qualified imports don't support
// record fields yet
td_name :: TypeDef -> String
typedef :: String Bool [Type] TypeDefRhs -> TypeDef
constructor :: String [Type] [TypeVar] ClassContext -> Constructor
recordfield :: String Type -> RecordField
