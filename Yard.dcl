definition module Yard

//Blatantly stolen fromhttps://github.com/dopefishh/cc1516/blob/master/yard.dcl

from StdString import class toString
from Data.Either import :: Either
from StdClass import class ==, class Eq
from Data.Functor import class Functor
from Control.Monad import class Monad
from Control.Applicative import class Applicative, class Alternative
from Data.Void import :: Void

:: Error = PositionalError Int Int String | Error String
:: Parser a b = Parser ([a] -> (Either Error b, [a]))

instance Functor (Parser a)
instance Applicative (Parser a) 
instance Monad (Parser a)
instance Alternative (Parser a)

instance toString Error

runParser :: (Parser a b) [a] -> (Either Error b, [a])
(<?>) :: (Parser a b) Error -> Parser a b
fail :: Parser a b
top :: Parser a a
peek :: Parser a a
satisfy :: (a -> Bool) -> Parser a a
check :: (a -> Bool) -> Parser a a
(until) infix 2 :: (Parser a b) (Parser a c) -> Parser a [b]
item :: a -> Parser a a | Eq a
list :: [a] -> Parser a [a] | Eq a
eof :: Parser a Void

