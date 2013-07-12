module List
(
	List(..),
	listHead,
	listTail,
	listLast,
	listInit,
	listLength,
	listFoldl,
	listFoldl',
	listFoldr,
	listFoldr',
	listMap,
	listFilter,
	listElem,
	listNull
) where

infixr 5 :::
data List a = EmptyList | a ::: (List a) deriving(Show, Read, Eq, Ord)

infixr 5 +++
(+++) EmptyList x = x
(+++) (x ::: xs) ys = x ::: (xs +++ ys)

(!!!) (x ::: _) 0 = x
(!!!) (_ ::: xs) n = xs !!! (n - 1)

listNull xs = xs == EmptyList

listHead (x ::: _) = x

listTail (_ ::: xs) = xs

listLast (x ::: EmptyList) = x
listLast (_ ::: xs) = listLast xs

listInit (_ ::: EmptyList) = EmptyList
listInit (x ::: xs) = x ::: listInit xs

listLength EmptyList = 0
listLength (_ ::: xs) = 1 + listLength xs

listFoldl _ acc EmptyList = acc
listFoldl f acc (x ::: xs) = listFoldl f (f acc x) xs

listFoldl' f (x ::: xs) = listFoldl f x xs

listFoldr _ acc EmptyList = acc
listFoldr f acc (x ::: xs) = f x (listFoldr f acc xs)

listFoldr' f xs = listFoldr f (listLast xs) (listInit xs) 

listMap f = listFoldr (\x acc -> f x ::: acc) EmptyList

listFilter f = listFoldr (\x acc -> if f x then x ::: acc else acc) EmptyList

listElem x = listFoldl (\acc y -> if y == x then True else acc) False
