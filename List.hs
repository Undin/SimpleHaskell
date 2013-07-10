module List
(
	List(..),
	headList,
	tailList,
	lastList,
	initList
) where

infixr 5 :::
data List a = EmptyList | a ::: (List a) deriving(Show, Read, Eq, Ord)

infixr 5 +++
(+++) EmptyList x = x
(+++) (x ::: xs) ys = x ::: (xs +++ ys)

headList (x ::: xs) = x

tailList (x ::: xs) = xs

lastList (x ::: EmptyList) = x
lastList (x ::: xs) = lastList xs

initList (x ::: EmptyList) = EmptyList
initList (x ::: xs) = x ::: initList xs

