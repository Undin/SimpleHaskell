module BinaryTrees.Treap
(
	Treap(..),
	merge,
	split
) where

data Treap x y = EmptyTreap | Node x y (Treap x y) (Treap x y) deriving(Show)

merge EmptyTreap second = second
merge first EmptyTreap = first
merge first@(Node x1 y1 left right1) second@(Node x2 y2 left2 right2)
	| y1 < y2 = Node x1 y1 left (merge right1 second)
	| otherwise = Node x2 y2 (merge first left2) right2

split _ EmptyTreap = (EmptyTreap, EmptyTreap)
split a (Node x y left right)
	| a <= x = let (l, r) = split a right in (Node x y left l, r)
	| otherwise = let (l, r) = split a left in (l, Node x y r right)
