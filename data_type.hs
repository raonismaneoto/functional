module Datatype(
	BST(..)
) where

data BST a = NIL | Node a (BST a) (BST a) deriving (Eq, Show)
data Maybe a = Just a | MyNothing

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

isBST = 0

insert :: Ord a => a -> BST a -> BST a 
insert elem NIL = Node elem NIL NIL
insert elem (Node a left right)
	| elem == a = Node a left right
	| elem < a = Node a (insert elem left) right
	| elem > a = Node a left (insert elem right) 

search elem NIL = NIL
search elem (Node a left right)
	| elem == a = (Node a left right)
	| elem < a = search elem left
	| elem > a = search elem right

maximumBST NIL = NIL
maximumBST (Node a left NIL) = (Node a left NIL)
maximumBST (Node a _ right) = maximumBST right

minimumBST NIL = NIL
minimumBST (Node a NIL right) = (Node a NIL right)
minimumBST (Node a left _) = minimumBST left

predecessorBST :: Ord a => a -> BST a -> BST a
predecessorBST elem (Node a left right)
	| search elem (Node a left right) == NIL = NIL
	| otherwise = predecessorAUX elem (Node a left right)

predecessorAUX elem NIL = NIL
predecessorAUX elem (Node a NIL NIL) = NIL
predecessorAUX elem (Node a NIL (Node c cleft cright))
	| elem == a = NIL
	| elem > a && elem == c = Node a NIL (Node c cleft cright)
	| elem > a = predecessorAUX elem (Node c cleft cright)
predecessorAUX elem (Node a (Node b bleft bright) NIL)
	| elem == b = NIL
	| elem < a && elem == b = maximumBST bleft
	| elem < a && elem /= b = predecessorAUX elem (Node b bleft bright)
predecessorAUX elem (Node a (Node b bleft bright) (Node c cleft cright))
	| elem == a = NIL
	| elem > a && elem == c = Node a (Node b bleft bright) (Node c cleft cright)
	| elem > a && elem /= c = predecessorAUX elem (Node c cleft cright)
	| elem < a && elem == b = maximumBST bleft
	| elem < a && elem /= b = predecessorAUX elem (Node b bleft bright)

-- successorBST :: Ord a => a -> BST a -> BST a
-- successorBST elem (Node a left right)
-- 	| search elem (Node a left right) == NIL = NIL
-- 	| otherwise = successorAUX elem (Node a left right)

-- successorAUX elem NIL = NIL
-- successorAUX elem (Node a NIL NIL) = NIL
-- successorAUX elem (Node a NIL (Node c cleft cright))
-- 	| elem == a = Node c cleft cright
-- 	| otherwise = successorAUX elem (Node c cleft cright)
-- successorAUX elem (Node a (Node b bleft bright) NIL) = minimumBST (Node b bleft bright)
-- successorAUX elem (Node a (Node b bleft bright) (Node c cleft cright))
-- 	| elem == a = minimumBST (Node c cleft cright)
-- 	| elem > a = successorAUX elem (Node c cleft cright)
-- 	| elem < a && elem > b = Node a (Node b bleft bright) (Node c cleft cright)
-- 	| elem < a && elem < b = successorAUX elem (Node b bleft bright)

deleteBST elem bst
	| isLeaf searchedNode = deleteLEAF elem bst
	| hasOneChild searchedNode = deleteOneChild elem bst
	| hasTwoChildren searchedNode = deleteSUCCESSOR elem bst
		where
			searchedNode = search elem bst

deleteLEAF elem (Node a (Node b bleft bright) (Node c cleft cright))
	| elem == a = NIL
	| elem == b = Node a NIL (Node c cleft cright)
	| elem == c = Node c (Node b bleft bright) NIL
	| elem > a = deleteLEAF (Node c cleft cright)
	| elem < a = deleteLEAF (Node b bleft bright)

deleteOneChild elem (Node a (Node b bleft bright) (Node c cleft cright))
	| elem == a  = (getNotNullChild (Node a (Node b bleft bright) (Node c cleft cright)))
	| elem == b = Node a (getNotNullChild (Node b bleft bright)) (Node c cleft cright)
	| elem == c = Node a (Node b bleft bright) (getNotNullChild (Node c cleft cright))
	| elem < a = deleteOneChild elem (Node b bleft bright)
	| elem > a = deleteOneChild elem (Node c cleft cright)

deleteSUCCESSOR elem (Node a left right)
	| elem == a = mergeChildrenToParent (toList left) (toList right) (Node s sleft sright)
	| elem < a = deleteSUCCESSOR elem left
	| elem > a = deleteSUCCESSOR elem right
		where
			Node s sleft sright = successorBST elem (Node a left right)

mergeChildrenToParent [] [] bst = bst
mergeChildrenToParent [] (y:ys) bst =
	mergeChildrenToParent [] ys (insert y bst)
mergeChildrenToParent (x:xs) (y:ys) bst =
	mergeChildrenToParent xs (y:ys) (insert x bst)

getNotNullChild (Node a left right)
	| left /= NIL = left
	| right /= NIL = right
	| otherwise = NIL 

hasOneChild (Node a left right)
	| left == NIL && right /= NIL = True
	| right == NIL && left /= NIL = True
	| otherwise = False

hasTwoChildren (Node a left right)
	| left /= NIL && right /= NIL = True
	| otherwise = False

isLeaf (Node a left right)
	| left == NIL && right == NIL = True
	| otherwise = False

preOrder NIL = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right

postOrder NIL = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]

inOrder NIL = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right

toList NIL = []
toList (Node a left right) = [a] ++ toList left ++ toList right

fromList [] = NIL
fromList xs = foldl (\bst x -> insert x bst) NIL xs

qsort :: (Ord a) => [a] -> [a] 
qsort [] = []
qsort (h:t) = (qsort [x| x<-t, x < h]) ++ [h] ++ (qsort [x| x<-t, x>=h ])
