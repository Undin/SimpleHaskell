module Trees.BinaryTree
(	BinaryTree(EmptyTree),
	treeInsert,
	treeElem
) where

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving(Show)

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