module Trees.BinaryTree
(	BinaryTree(EmptyTree),
	treeInsert,
	treeElem,
	treeErase,
	treeMin,
	treeMax,
	treeLowerBound,
	treeUpperBound
) where

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving(Show, Eq)

singletonTree x = Node x EmptyTree EmptyTree

treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)
	
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

treeErase x EmptyTree = EmptyTree
treeErase x (Node a left right)
	| x == a = if left == EmptyTree then right else let Node top _ _ = left in Node top (treeErase top left) right
	| x < a = Node a (treeErase x left) right
	| x > a = Node a left (treeErase x right)
	
treeMin (Node x EmptyTree _) = x
treeMin (Node _ left _) = treeMin left

treeMax (Node x _ EmptyTree) = x
treeMax (Node _ _ right) = treeMax right

treeLowerBound _ EmptyTree = Nothing
treeLowerBound x (Node a left right)
	| x == a = Just x
	| x < a = treeLowerBound x left
	| x > a = max (Just a) (treeLowerBound x right)

treeUpperBound _ EmptyTree = Nothing	
treeUpperBound x (Node a left right)
	| x == a = Just x
	| x < a = let res = treeUpperBound x left in if res == Nothing then Just a else min (Just a) res
	| x > a = treeUpperBound x right