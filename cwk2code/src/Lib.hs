module Lib (
    Tree,
    -- Leaf,
    -- DataNode,

    testEmptyTree_A,
    testEmptyTree_B,
    testEmptyTree_C,
    empty,
    isEmpty,

    testFindTree_A,
    testFindTree_B,
    testFindTree_C,
    testFindTree_D,
    testFindTree_E,
    findNode,

    insertNode,

    orderNodes,
    --displayAll
    removeNode,
    removeNodeIf
) where

--Leafs have no data at all. They are included for pattern matching
--Nodes have a key, an item, and 2 pointers sending you to the next nodes
--This derives Eq so that a tree can be compared to another for the purposes of testing

data Tree k i = Leaf | DataNode {
    getKey :: k, 
    getItem :: i,
    getLeft :: (Tree k i),
    getRight :: (Tree k i)
    }

empty :: Tree k i
empty = Leaf

isEmpty :: Tree k i -> Bool
isEmpty Leaf = True
isEmpty (DataNode a b c d) = False

testEmptyTree_A = empty
testEmptyTree_B = DataNode 5 "Item" empty empty
testEmptyTree_C = DataNode 5 "Item" (DataNode 4 "Left Item" empty empty) (DataNode 6 "Right Item" empty empty) 

--The standard library term "Maybe" is used to define if you don't know if there could be an ouput or not
--It is define something like: data Maybe t = Just t | Nothing

findNode :: Eq k => Ord k => Tree k i -> k -> Maybe i
findNode Leaf searchKey  = Nothing  --The key cannot be found, so return a null value
findNode (DataNode key item left right) searchKey =
    if searchKey == key then Just item                   --If the current node is the right one, return the item
    else if searchKey < key then findNode left searchKey --If the searchKey is smaller than the current key, go down the left branch
    else findNode right searchKey                         --If the searchKey is bigger than the current key, go down the right branch

testFindTree_A = empty
testFindTree_B = DataNode 5 "item" empty empty
testFindTree_C = DataNode 7 "not this item" empty empty
testFindTree_D = DataNode 4 "the top item"  (DataNode 3 "the left item" empty empty)  (DataNode 5 "item" empty empty)
testFindTree_E = DataNode 3 "the top item"  (DataNode 1 "the left item" empty empty)  (DataNode 6 "not this item" empty empty)

--Insert node

insertNode :: Eq k => Ord k => Tree k i -> k -> i -> Tree k i
insertNode Leaf key item = DataNode key item Leaf Leaf                  --If there is space in the tree, create a DataNode
insertNode (DataNode thisKey thisItem left right) key item = 
    if thisKey == key then DataNode thisKey item left right             --If it is at the node, change its value
    else if key < thisKey 
        then DataNode thisKey thisItem (insertNode left key item) right --If the node should be on the left
        else DataNode thisKey thisItem left (insertNode right key item) --If the node should be on the right

--Display ordered list of all entires

orderNodes :: Tree k i -> [(k,i)]
orderNodes Leaf = []
orderNodes (DataNode key item left right) = 
    let 
        leftAdd = orderNodes left
        middleAdd = [(key,item)]
        rightAdd = orderNodes right
    in leftAdd ++ middleAdd ++ rightAdd

--Remove nodes

removeNode :: Eq k => Ord k => Tree k i -> k -> Tree k i
removeNode Leaf removal = Leaf --If tree is empty already or cannot find the node, output empty
removeNode (DataNode key item left right) removal =
    if removal == key
        then if (isEmpty left) && (isEmpty right) then Leaf
        else if (isEmpty left) --If going down the right-hand path, looking for the lowest node
                then let (newTree,replace) = removeNode_extra right (isEmpty left)
                    in DataNode (getKey replace) (getItem replace) left newTree
                else let (newTree,replace) = removeNode_extra left (isEmpty left)
                    in DataNode (getKey replace) (getItem replace) newTree right
    else if removal < key --If searching down the left side
        then DataNode key item (removeNode left removal) right
        else DataNode key item left (removeNode right removal) 

--Takes the tree and the direction it should go, and outputs the new child tree and the replacement node
removeNode_extra :: Tree k i -> Bool -> (Tree k i, Tree k i) 
removeNode_extra (DataNode key item left right) lesser =
    if lesser --Looking for the lowest node
    then 
        if isEmpty left --Found the lowest node
            then (right, DataNode  key item Leaf Leaf) --Outputs the right hand side which will be placed on the left of the parent of the deleted node, and outputs the replacement node 
            else let (result1,result2) = removeNode_extra left lesser
                in (DataNode key item result1 right, result2)

    else --Looking for the highest node
        if isEmpty right --Found the highest node
            then (left, DataNode key item Leaf Leaf) --Outputs the left hand side which will be placed on the right of the parent of the deleted node, and outputs the replacement node 
            else let (result1,result2) = removeNode_extra right lesser
                in (DataNode key item left result1, result2)

removeNodeIf :: Eq k => Ord k => Tree k i -> (k -> Bool) -> Tree k i 
removeNodeIf Leaf cond = Leaf 
removeNodeIf (DataNode key item left right) cond = 
    let --Visit left first, then right, then node
        left_side = removeNodeIf left cond
        right_side = removeNodeIf right cond
        thisNode = DataNode key item left_side right_side
    in 
        if (cond key) then removeNode thisNode key else thisNode
