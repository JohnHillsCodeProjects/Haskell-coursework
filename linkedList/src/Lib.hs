module Lib (
    List,
    emptyList,
    isEmptyList,
    emptyTestList_A,
    emptyTestList_B,
    emptyTestList_C,

    findingList,
    findListNode,
    
    insertListNode,
    listAllNodes,
    removeListNode,
    removeListNodeIf
) where 

data List k i = Leaf | DataNode {
    getKey :: k,
    getItem :: i,
    getNext :: (List k i)
}

emptyList :: List k i
emptyList = Leaf 

isEmptyList :: List k i -> Bool
isEmptyList Leaf = True
isEmptyList (DataNode k i n) = False 

emptyTestList_A = emptyList
emptyTestList_B = DataNode 6 "item" emptyList
emptyTestList_C = DataNode 6 "item" (DataNode 4 "second item" (DataNode 9 "last item" emptyList))

findListNode :: Eq k => List k i -> k -> Maybe i 
findListNode Leaf searchKey = Nothing
findListNode (DataNode key item next) searchKey = if key == searchKey then Just item else findListNode next searchKey

findingList = DataNode 6 "item" (DataNode 17 "item" (DataNode 5 "item" (DataNode 6 "item" emptyList)))

insertListNode :: Eq k => List k i -> k -> i -> List k i 
insertListNode Leaf newKey newItem = DataNode newKey newItem emptyList 
insertListNode (DataNode key item next) newKey newItem = 
    if key == newKey 
    then DataNode key newItem next 
    else DataNode key item (insertListNode next newKey newItem)

listAllNodes :: List k i -> [(k,i)]
listAllNodes Leaf = []
listAllNodes (DataNode key item next) = [(key,item)] ++ (listAllNodes next)

removeListNode :: Eq k => List k i -> k -> List k i
removeListNode Leaf searchKey = Leaf
removeListNode (DataNode key item next) searchKey = 
    if key == searchKey then next 
    else DataNode key item (removeListNode next searchKey)

removeListNodeIf :: List k i -> (k -> Bool) -> List k i
removeListNodeIf Leaf cond = Leaf 
removeListNodeIf (DataNode key item next) cond = 
    if (cond key) then removeListNodeIf next cond 
    else DataNode key item (removeListNodeIf next cond)