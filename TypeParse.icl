implementation module TypeParse

import TypeDef
import TypeUtil
import Yard

import StdList
import StdString
import StdTuple

import Data.Either
import Data.Maybe
from Data.Func import $
from Data.List import instance Functor []
from Text import class Text(concat), instance Text String
import Data.Functor
import Control.Applicative
import Control.Monad

import GenEq

derive gEq Token
instance == Token where == a b = a === b

:: Token = TIdent String                // UpperCaseId or FunnyId
		 | TVar String                  // LowerCaseId

		 | TArrow                       // ->
		 | TComma                       // ,
		 | TUnique                      // *
		 | TAnonymous                   // .
		 | TUnboxed                     // #
		 | TStrict                      // !
		 | TColon                       // :

		 | TParenOpen | TParenClose     // ( )
		 | TBrackOpen | TBrackClose     // [ ]
		 | TBraceOpen | TBraceClose     // { }

isTIdent (TIdent _) = True; isTIdent _ = False
isTVar   (TVar   _) = True; isTVar   _ = False

tokenize :: ([Char] -> Maybe [Token])
tokenize = tkz []
where
	tkz :: [Token] [Char] -> Maybe [Token]
	tkz tks [] = Just tks
	tkz tks ['-':'>':cs] = tkz (tks ++ [TArrow]) cs
	tkz tks [',':cs] = tkz (tks ++ [TComma]) cs
	tkz tks ['*':cs] = tkz (tks ++ [TUnique]) cs
	tkz tks ['.':cs] = tkz (tks ++ [TAnonymous]) cs
	tkz tks ['#':cs] = tkz (tks ++ [TUnboxed]) cs
	tkz tks ['!':cs] = tkz (tks ++ [TStrict]) cs
	tkz tks ['(':cs] = tkz (tks ++ [TParenOpen]) cs
	tkz tks [')':cs] = tkz (tks ++ [TParenClose]) cs
	tkz tks ['[':cs] = tkz (tks ++ [TBrackOpen]) cs
	tkz tks [']':cs] = tkz (tks ++ [TBrackClose]) cs
	tkz tks ['{':cs] = tkz (tks ++ [TBraceOpen]) cs
	tkz tks ['}':cs] = tkz (tks ++ [TBraceClose]) cs
	tkz tks [c:cs]
	| isSpace c = tkz tks cs
	| isUpper c = let (id, cs`) = span isIdentChar cs in
				  tkz (tks ++ [TIdent $ toString [c:id]]) cs`
	| isFunny c = let (id, cs`) = span isFunny cs in
				  tkz (tks ++ [TIdent $ toString [c:id]]) cs`
	| isLower c = let (var, cs`) = span isIdentChar cs in
				  tkz (tks ++ [TVar $ toString [c:var]]) cs`
	tkz _ _ = Nothing

	isIdentChar :: Char -> Bool
	isIdentChar c = any (\f->f c) [isLower, isUpper, isDigit, (==)'_', (==) '`']

	isFunny :: Char -> Bool
	isFunny c = isMember c ['~@#$%^?!+-*<>\\/|&=:']

type :: Parser Token Type
type = liftM3 Func (some argtype) (item TArrow *> type) (pure []) // no CC for now
	<|> liftM2 Cons cons (some argtype)
	<|> (item (TIdent "String") >>| pure (Type "_#Array" [Type "Char" []]))
	<|> liftM2 Type ident (many argtype)
	<|> argtype
where
	argtype :: Parser Token Type
	argtype = item TParenOpen *> type <* item TParenClose
		<|> (item (TIdent "String") >>| pure (Type "_#Array" [Type "Char" []]))
		<|> liftM (\t->Type t []) ident
		<|> liftM Var var
		<|> liftM Uniq uniq
		<|> liftM (\t -> Type "_#Array" [t])
			(list [TBraceOpen, TUnboxed] *> type <* item TBraceClose)
		<|> liftM (\t -> Type "_Array" [t])
			(item TBraceOpen *> type <* item TBraceClose)
		<|> liftM (\t -> Type "_List" [t])
			(item TBrackOpen *> type <* item TBrackClose)
		<|> liftM (\ts -> Type ("_Tuple" +++ toString (length ts)) ts)
			(item TParenOpen *> seplist TComma type <* item TParenClose)
		<|> item TStrict *> argtype       // ! ignored for now
		<|> item TUnboxed *> argtype      // # ignored for now (except for the _#Array case above)
		<|> item TAnonymous *> argtype    // . ignored for now
		<|> unqvar *> item TColon *> argtype // u: & friends ignored for now

	ident :: Parser Token String
	ident = (\(TIdent id)->id) <$> satisfy isTIdent

	var :: Parser Token TypeVar
	var = (\(TVar var)->var) <$> satisfy isTVar
	cons = var
	unqvar = var

	uniq :: Parser Token Type
	uniq = item TUnique *> argtype

	seplist :: a (Parser a b) -> Parser a [b] | Eq a
	seplist sep p = liftM2 (\es->(\e->reverse [e:es])) (some (p <* item sep)) p
		<|> liftM pure p
		<|> pure empty

parseType :: [Char] -> Maybe Type
parseType cs
# mbTokens = tokenize cs
| isNothing mbTokens = Nothing
= case fst $ runParser type (fromJust mbTokens) of
		(Left _) -> Nothing
		(Right t) -> Just t

