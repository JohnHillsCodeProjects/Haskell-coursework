import Test.HUnit
import Test.QuickCheck
import Data.Maybe --For the isJust function
import Data.List --For the sort function

import Lib  

import Dictionary

--Testing empty

testEmptyTree_D = empty

emptyTests::Test
emptyTests = TestList [
    TestCase (assertBool "Testing empty A (actually empty)"           (isEmpty testEmptyTree_A)),
    TestCase (assertBool "Testing empty B (just a single DataNode)"   (not (isEmpty testEmptyTree_B))),
    TestCase (assertBool "Testing empty C (an actual tree)"           (not (isEmpty testEmptyTree_C))),
    TestCase (assertBool "Testing empty D (created in the test file)" (isEmpty testEmptyTree_D))
     ]

--Testing findNode

findTests::Test
findTests = TestList [
    TestCase (assertBool "Testing find A (empty tree)"                                (not ( isJust (findNode testFindTree_A 5) ))),
    TestCase (assertBool "Testing find B (one node, contains the search key)"         ( isJust (findNode testFindTree_B 5) )),
    TestCase (assertBool "Testing find C (one node, doesn't contain the search key)"  (not ( isJust (findNode testFindTree_C 5) ))),
    TestCase (assertBool "Testing find D (full tree, contains the search key)"        ( isJust (findNode testFindTree_D 5) )),
    TestCase (assertBool "Testing find E (full tree, doesn't contain the search key)" (not ( isJust (findNode testFindTree_E 5) )))
    ]

--Testing insertNode

generateNewTree :: Tree Int String -> [Int] -> Tree Int String --Creates a tree out of a list of keys 
generateNewTree tree [] = empty
generateNewTree tree [key] = insertNode tree key "inside the tree" 
generateNewTree tree (key:rest) = insertNode (generateNewTree tree rest) key "inside the tree"

checkingTree :: Tree Int String -> [Int] -> Bool
checkingTree tree [] = True
checkingTree tree [key] = isJust(findNode tree key) 
checkingTree tree (key:rest) = 
    if isJust(findNode tree key) --If the key can be found in the tree
    then checkingTree tree rest  --Then keep searching through the list of keys
    else False                   --Else, the key in the list wasn't added, therefore insertNode failed

insertTests :: [Int] -> Property
insertTests generated_keys = 
    not (null generated_keys) ==> --Exclude empty arrays since then insertNode wouldn't be tested
    let keys = removeClones generated_keys
    in property (checkingTree (generateNewTree empty keys) keys)

removeClones :: [Int] -> [Int]
removeClones [] = []
removeClones [h] = [h]
removeClones (h:t) = 
    let x = if elem h t then [] else [h]
    in x ++ removeClones t
    
removeClonesTest::Test
removeClonesTest = TestList [
    TestCase (assertEqual "Removing clones A" [4,3,8,5,6,7] (removeClones [4,3,8,5,6,7])),
    TestCase (assertEqual "Removing clones B" [1,2,3,4]     (removeClones [1,1,2,2,2,3,4,4])),
    TestCase (assertEqual "Removing clones C" [1,2]         (removeClones [2,1,2,1,2])),
    TestCase (assertEqual "Removing clones D" [6,8,3,5,4]   (removeClones [5,3,4,5,6,8,4,3,5,4]))
    ]

--Testing overwriting 

overWriteTest::Test
overWriteTest =
    let tree = generateNewTree empty [5,3,6,8,2,1,4,9,10]
    in TestList [
        TestCase (assertEqual "Overwrite test A (top node)"      "New item" (fromJust (findNode (insertNode tree 5  "New item") 5 ))),
        TestCase (assertEqual "Overwrite test A (branched node)" "New item" (fromJust (findNode (insertNode tree 3  "New item") 3 ))),
        TestCase (assertEqual "Overwrite test A (end node)"      "New item" (fromJust (findNode (insertNode tree 10 "New item") 10)))
        ]

--Testing orderNodes

pairsFromList :: [Int] -> [(Int,String)]
pairsFromList [] = []
pairsFromList [h] = [(h,"inside the tree")]
pairsFromList (h:t) = [(h,"inside the tree")] ++ pairsFromList t

orderNodesTests :: [Int] -> Property
orderNodesTests keys = 
    not (null keys) ==>
    property (orderNodes (generateNewTree empty keys) == pairsFromList (sort (removeClones keys)))

-- displayAllTests :: IO()
-- displayAllTests = do
--     displayAll (generateNewTree empty [0,-1,0,-3,5,1,4,7]) -- [-3,-1,0,1,4,5,7]
--     putStrLn "" --Line break
--     displayAll (generateNewTree empty [0,2,8,-1,4,1,0,-2]) -- [-2,-1,0,1,2,4,8]
--     putStrLn "" --Line break
--     displayAll (generateNewTree empty [3,1,-3,-1,7,5,7,1]) -- [-3,-1,1,3,5,7]  
--     putStrLn "" --Line break
--     displayAll (generateNewTree empty [4,5,-2,6,3,7,-1,8]) -- [-2,-1,3,4,5,6,7,8]    

removeTest::Test
removeTest = 
    let 
        keys = [6,1,2,4,3,10,11,8,7,9]
        tree = generateNewTree empty (reverse keys)
        node = insertNode empty 0 "item"

        removeTestTree_A = removeNode tree 11 --Remove a leaf node
        checkA = delete 11 keys
        removeTestTree_B = removeNode tree 1  --Remove a node one child
        checkB = delete 1 keys
        removeTestTree_C = removeNode tree 8  --Remove a node with 2 children
        checkC = delete 8 keys
        removeTestTree_D = removeNode tree 6  --Remove the root node
        checkD = delete 6 keys
        removeTestTree_E = removeNode tree 0  --Remove a node that isn't there

        just_a_node = removeNode node 0  --Remove node that is the only one

    in TestList [
        TestCase (assertBool "Testing remove tree A for removed node" (not (isJust (findNode removeTestTree_A 11)))),
        TestCase (assertBool "Testing remove tree A for other nodes"  (checkingTree removeTestTree_A checkA)),

        TestCase (assertBool "Testing remove tree B for removed node" (not (isJust (findNode removeTestTree_B 1)))),
        TestCase (assertBool  "Testing remove tree B for other nodes" (checkingTree removeTestTree_B checkB)),

        TestCase (assertBool "Testing remove tree C for removed node" (not (isJust (findNode removeTestTree_C 8)))),
        TestCase (assertBool "Testing remove tree C for other nodes"  (checkingTree removeTestTree_C checkC)),

        TestCase (assertBool "Testing remove tree D for removed node" (not (isJust (findNode removeTestTree_D 6)))),
        TestCase (assertBool "Testing remove tree D for other nodes"  (checkingTree removeTestTree_D checkD)),

        TestCase (assertBool "Testing remove tree E for removal node" (not (isJust (findNode removeTestTree_E 0)))),
        TestCase (assertBool "Testing remove tree E for other nodes"  (checkingTree removeTestTree_E keys)),

        TestCase (assertBool "Testing remove tree F for only node" (not (isJust (findNode just_a_node 0)))),
        TestCase (assertBool "Testing remove tree F is empty"      (isEmpty just_a_node))
    ]
        

removeIf_keys :: [Int] -> (Int -> Bool) -> ([Int],[Int]) 
--Seperates the keys based on the condition (in tree, removed from tree). Therefore cond = (false, true)
removeIf_keys [] cond = ([],[])
removeIf_keys (h:t) cond = 
    let 
        this_add = if (cond h) then ([],[h]) else ([h],[]) 
        next_add = removeIf_keys t cond
    in (fst this_add ++ fst next_add, snd this_add ++ snd next_add)

isWhole :: Float -> Bool --Tests to see if the input is a whole number (needed for condition_C)
isWhole num = num == fromInteger (round num)

condition_A :: Int -> Bool --Remove if lower than 3
condition_A a = a < 3  
condition_B :: Int -> Bool --Remove if a multiple of 4
condition_B a = (a `mod` 4) == 0
condition_C :: Int -> Bool --Remove if a square number (False for all negative numbers)
condition_C a = if a < 0 then False else isWhole (sqrt (fromIntegral a)) 

checkingTree_NOT :: Tree Int String -> [Int] -> Bool --The exact opposite of the 
checkingTree_NOT tree [] = True
checkingTree_NOT tree [key] = isNothing (findNode tree key) 
checkingTree_NOT tree (key:rest) = 
    if isNothing (findNode tree key) --If the key cannot be found
    then checkingTree_NOT tree rest  --Move on to check that the rest cannot be found
    else False                        

removeIf_error_1::Test
removeIf_error_1 = 
    let 
        testTree_A = generateNewTree empty [7,3,13,1,5,9,11]
    in TestList [
        TestCase (assertBool "If checkingTree_NOT works" (checkingTree_NOT testTree_A [0,2,4,6,8,10,12,14])),
        TestCase (assertBool "Check condition_C test 1"  (condition_C 0)),
        TestCase (assertBool "Check condition_C test 2"  (condition_C 4)),
        TestCase (assertBool "Check condition_C test 3"  (not (condition_C 7))),
        TestCase (assertBool "Check condition_C test 4"  (not (condition_C (-4))))
    ]

removeIfTests_A :: [Int] -> [Int] -> [Int] -> Property
removeIfTests_A keys1 keys2 keys3 =
    --There should be many nodes in the test tree to ensure that removeNodeIf truly works
    not (null keys1 || null keys2 || null keys3) ==> 
    let 
        keys = removeClones (keys1 ++ keys2 ++ keys3)
        tree = generateNewTree empty keys 

        removeIfTree_A = removeNodeIf tree condition_A
        (check_A,check_NOT_A) = removeIf_keys keys condition_A

        testA = (checkingTree removeIfTree_A check_A) && (checkingTree_NOT removeIfTree_A check_NOT_A)
    in property(testA) 

removeIfTests_B :: [Int] -> [Int] -> [Int] -> Property
removeIfTests_B keys1 keys2 keys3 =
    --There should be many nodes in the test tree to ensure that removeNodeIf truly works
    not (null keys1 || null keys2 || null keys3) ==> 
    let 
        keys = removeClones (keys1 ++ keys2 ++ keys3)
        tree = generateNewTree empty keys 

        removeIfTree_B = removeNodeIf tree condition_B
        (check_B,check_NOT_B) = removeIf_keys keys condition_B

        testB = (checkingTree removeIfTree_B check_B) && (checkingTree_NOT removeIfTree_B check_NOT_B)
    in property(testB) 

removeIfTests_C :: [Int] -> [Int] -> [Int] -> Property
removeIfTests_C keys1 keys2 keys3 =
    --There should be many nodes in the test tree to ensure that removeNodeIf truly works
    not (null keys1 || null keys2 || null keys3) ==> 
    let 
        keys = removeClones (keys1 ++ keys2 ++ keys3)
        tree = generateNewTree empty keys 

        removeIfTree_C = removeNodeIf tree condition_C
        (check_C,check_NOT_C) = removeIf_keys keys condition_C

        testC = (checkingTree removeIfTree_C check_C) && (checkingTree_NOT removeIfTree_C check_NOT_C)
    in property(testC) 

-------------------------------------------------------------
--Dictionary using the BST tests
-------------------------------------------------------------

emptyDictTests::Test
emptyDictTests = let emptyTestDictionary = empty_D in
    TestList [
        TestCase (assertBool "Empty dict"     (isEmpty_D emptyTestDictionary)),
        TestCase (assertBool "Non-empty dict" (not (isEmpty_D normalDictionary)))
    ]

findDictTests :: Test
findDictTests = TestList [
    TestCase(assertBool "Search empty dict"                 (not (isJust (findEntry empty_D 5)))),
    TestCase(assertBool "Search dict for existent item"     (isJust (findEntry findingDict 4))),
    TestCase(assertBool "Search dict for non-existent item" (not (isJust (findEntry findingDict 6))))
    ]

checkingDict :: Dict Int String -> [Int] -> Bool 
checkingDict dict [] = True
checkingDict dict [h] = isJust(findEntry dict h)
checkingDict dict (h:t) = 
    if isJust(findEntry dict h)
    then checkingDict dict t 
    else False

generateNewDict :: Dict Int String -> [Int] -> Dict Int String
generateNewDict dict [] = empty_D
generateNewDict dict [h] = addEntry dict h "inside the dict"
generateNewDict dict (h:t) = addEntry (generateNewDict dict t) h "inside the dict"

addDictTests :: [Int] -> Property
addDictTests keys = 
    not (null keys) ==>
    let dict = generateNewDict empty_D keys
    in property (checkingDict dict keys)

overwriteDictTest::Test
overwriteDictTest =
    let tree = generateNewDict empty_D [5,3,6,8,2,1,4,9,10]
    in TestList [
        TestCase (assertEqual "Overwrite test A (first entry)"  "New item" (fromJust (findEntry (addEntry tree 10 "New item") 10))),
        TestCase (assertEqual "Overwrite test A (middle entry)" "New item" (fromJust (findEntry (addEntry tree 8  "New item") 8))),
        TestCase (assertEqual "Overwrite test A (last entry)"   "New item" (fromJust (findEntry (addEntry tree 5  "New item") 5)))
    ]

dictTestPairs :: [Int] -> [(Int,String)]
dictTestPairs [] = []
dictTestPairs (h:t) = [(h,"inside the dict")] ++ dictTestPairs t

allEntriesTest :: [Int] -> Property
allEntriesTest keys = 
    not (null keys) ==>
    property ( allEntries (generateNewDict empty_D keys) == dictTestPairs (sort (removeClones keys)) )

removeDictTest :: Test
removeDictTest = 
    let 
        keys = [0,5,9,-4,4,2,-1,8,-3,13,12,3,7,10,6]
        dict = generateNewDict empty_D keys
        removeDictTest_A = removeEntry dict 6
        check_A = delete 6 keys
        removeDictTest_B = removeEntry dict (-3)
        check_B = delete (-3) keys
        removeDictTest_C = removeEntry dict 0
        check_C = delete 0 keys
    in TestList [
        TestCase (assertBool "Remove dict A (first entry)"   (not (isJust(findEntry removeDictTest_A 6)))),
        TestCase (assertBool "Remove dict A (other entries)" (checkingDict removeDictTest_A check_A)),
        TestCase (assertBool "Remove dict B (middle entry)"  (not (isJust(findEntry removeDictTest_B (-3))))),
        TestCase (assertBool "Remove dict B (other entries)" (checkingDict removeDictTest_B check_B)),
        TestCase (assertBool "Remove dict C (last entry)"    (not (isJust(findEntry removeDictTest_C 0)))),
        TestCase (assertBool "Remove dict C (other entries)" (checkingDict removeDictTest_C check_C))
    ]

checkingDict_NOT :: Dict Int String -> [Int] -> Bool --The exact opposite of checkingDict
checkingDict_NOT dict [] = True
checkingDict_NOT dict [key] = isNothing (findEntry dict key) 
checkingDict_NOT dict (key:rest) = 
    if isNothing (findEntry dict key) --If the key cannot be found
    then checkingDict_NOT dict rest  --Move on to check that the rest cannot be found
    else False    

removeIfDictTest_A :: [Int] -> [Int] -> [Int] -> Property
removeIfDictTest_A keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = keys1 ++ keys2 ++ keys3
        dict = generateNewDict empty_D keys
        testDict = removeEntryIf dict condition_A
        (check,check_NOT) = removeIf_keys keys condition_A
    in
        property((checkingDict testDict check) && (checkingDict_NOT testDict check_NOT) )

removeIfDictTest_B :: [Int] -> [Int] -> [Int] -> Property
removeIfDictTest_B keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = keys1 ++ keys2 ++ keys3
        dict = generateNewDict empty_D keys
        testDict = removeEntryIf dict condition_B
        (check,check_NOT) = removeIf_keys keys condition_B
    in
        property((checkingDict testDict check) && (checkingDict_NOT testDict check_NOT) )

removeIfDictTest_C :: [Int] -> [Int] -> [Int] -> Property
removeIfDictTest_C keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = keys1 ++ keys2 ++ keys3
        dict = generateNewDict empty_D keys
        testDict = removeEntryIf dict condition_C
        (check,check_NOT) = removeIf_keys keys condition_C
    in
        property((checkingDict testDict check) && (checkingDict_NOT testDict check_NOT) )
        

main = do
    putStrLn("\nTree tests\n")

    runTestTT emptyTests
    runTestTT findTests
    quickCheck insertTests
    runTestTT overWriteTest
    quickCheck orderNodesTests
    runTestTT removeTest
    runTestTT removeIf_error_1
    quickCheck removeIfTests_A
    quickCheck removeIfTests_B
    quickCheck removeIfTests_C

    putStrLn("\nDictionary tests\n")

    runTestTT emptyDictTests
    runTestTT findDictTests
    quickCheck addDictTests
    runTestTT overwriteDictTest
    quickCheck allEntriesTest
    runTestTT removeDictTest
    quickCheck removeIfDictTest_A
    quickCheck removeIfDictTest_B
    quickCheck removeIfDictTest_C

    return()
