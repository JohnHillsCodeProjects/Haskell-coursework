module Dictionary (
    Dict,
    empty_D,
    isEmpty_D,
    findEntry,
    addEntry,
    allEntries,
    removeEntry,
    removeEntryIf,

    normalDictionary,
    findingDict
) where

import Lib

data Dict k i = Dict (Tree k i)

normalDictionary = Dict (insertNode empty 5 "item")

findingDict = Dict (insertNode (insertNode (insertNode empty 5 "item") 3 "item") 4 "item")

empty_D :: Dict k i
empty_D = Dict (empty)

isEmpty_D :: Dict k i -> Bool
isEmpty_D (Dict tree) = isEmpty tree

findEntry :: Eq k => Ord k => Dict k i -> k -> Maybe i
findEntry (Dict tree) key = findNode tree key

addEntry :: Eq k => Ord k => Dict k i -> k -> i -> Dict k i
addEntry (Dict tree) key item = Dict (insertNode tree key item)

allEntries :: Dict k i -> [(k, i)]
allEntries (Dict tree) = orderNodes tree

removeEntry :: Eq k => Ord k => Dict k i -> k -> Dict k i 
removeEntry (Dict tree) key = Dict (removeNode tree key)

removeEntryIf :: Eq k => Ord k => Dict k i -> (k -> Bool) -> Dict k i
removeEntryIf (Dict tree) cond = Dict (removeNodeIf tree cond) 

