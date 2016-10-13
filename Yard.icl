implementation module Yard

import StdTuple
import StdClass
import StdString
import StdList
import StdInt
from Data.List import intersperse
from Text import instance Text String, class Text(concat)
import Data.Functor
import Data.Either
import Control.Monad
import Control.Applicative
from Data.Func import $
import Data.Void

instance toString Error where
	toString (PositionalError l c e) =
		concat [toString l,":",toString c,": ",e, "\n"]
	toString (Error e) = concat ["-:-: ", e, "\n"]

runParser :: (Parser a b) [a] -> (Either Error b, [a])
runParser (Parser f) i = f i
 
instance Functor (Parser a) where
	fmap f m = liftM f m

instance Applicative (Parser a) where
	pure a	  = Parser \i -> (Right a, i)
	(<*>) sf p  = ap sf p

instance Monad (Parser a) where
	bind p f	= Parser \i -> case runParser p i of
		(Right r, rest) = runParser (f r) rest
		(Left e, _)	 = (Left e, i)

instance Alternative (Parser a) where
	empty	   = Parser \i -> (Left $ Error "" , i)
	(<|>) p1 p2 = Parser \i -> case runParser p1 i of
		(Right r, rest) = (Right r, rest)
		(Left e1, rest) = case runParser p2 i of
			(Left e2, rest)  = (Left e2, i)
			(Right r, rest)  = (Right r, rest)

//Try parser, if it fails decorate the error with the given String and position
(<?>) :: (Parser a b) Error -> Parser a b
(<?>) p e = Parser \i -> case runParser p i of
	(Left e1, rest) = (Left e, rest)
	(Right r, rest) = (Right r, rest)

fail :: Parser a b
fail = empty

top :: Parser a a
top = Parser \i -> case i of
	[]	  = (Left $ Error "", [])
	[x:xs]  = (Right x, xs)

peek :: Parser a a
peek = Parser \i -> case i of
	[]	  = (Left $ Error "", [])
	[x:xs]  = (Right x, [x:xs])

(until) infix 2 :: (Parser a b) (Parser a c) -> Parser a [b]
(until) p guard = try $ until` p guard [] 
	where
		until` :: (Parser a b) (Parser a c) [b] -> Parser a [b]
		until` p guard acc = Parser \i -> case runParser guard i of 
			(Right _, rest) = (Right acc, rest)
			(Left _, _)	 = case runParser p i of
				(Right r, rest) = runParser (until` p guard [r:acc]) rest
				(Left e, _)	 = (Left e, i)
		try :: (Parser a b) -> Parser a b
		try p = Parser \i -> case runParser p i of
			(Left e, _)	 = (Left e, i)
			(Right r, rest) = (Right r, rest)

eof :: Parser a Void
eof = Parser \i -> case i of 
	[]	= (Right Void, [])
	_	= (Left $ Error "", i)

satisfy :: (a -> Bool) -> Parser a a
satisfy f = top >>= \r -> if (f r) (pure r) fail

check :: (a -> Bool) -> Parser a a
check f = peek >>= \r -> if (f r) (pure r) fail

item :: a -> Parser a a | Eq a
item a  = satisfy ((==)a)

list :: [a] -> Parser a [a] | Eq a
list as = mapM item as

