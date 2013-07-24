module BinaryTrees.Treap
(
	Treap(EmptyTreap),
	insert
) where

import System.Random(RandomGen, random)

data Treap x = EmptyTreap | Node x Int (Treap x) (Treap x) deriving (Show)

merge EmptyTreap second = second
merge first EmptyTreap = first
merge first@(Node x1 y1 left1 right1) second@(Node x2 y2 left2 right2)
	| y1 < y2 = Node x1 y1 left1 (merge right1 second)
	| otherwise = Node x2 y2 (merge first left2) right2

split _ EmptyTreap = (EmptyTreap, EmptyTreap)
split a (Node x y left right)
	| x <= a = let (l, r) = split a right in (Node x y left l, r)
	| otherwise = let (l, r) = split a left in (l, Node x y r right)

makeTreap x y = Node x y EmptyTreap EmptyTreap 

insert x gen tree = merge (merge left (makeTreap x y)) right
	where
		(left, right) = split x tree
		(y, _) = random gen
