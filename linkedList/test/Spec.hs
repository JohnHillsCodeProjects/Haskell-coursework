import Test.HUnit
import Test.QuickCheck
import Data.Maybe --For the isJust function
import Data.List --For the sort function

import Lib

emptyTestList_D = emptyList

emptyListTests :: Test
emptyListTests = TestList [
    TestCase (assertBool "Empty list test A (empty)"                    (isEmptyList emptyTestList_A)),
    TestCase (assertBool "Empty list test B (just a single DataNode)"   (not (isEmptyList emptyTestList_B))),
    TestCase (assertBool "Empty list test C (an actual list)"           (not (isEmptyList emptyTestList_C))),
    TestCase (assertBool "Empty list test D (created in the test file)" (isEmptyList emptyTestList_D))
    ]

findListTests :: Test
findListTests = TestList [
    TestCase (assertBool "Search empty list"             (not (isJust (findListNode emptyList 5)))),
    TestCase (assertBool "Search for existant entry"     (isJust (findListNode findingList 5))),
    TestCase (assertBool "Search for non-existant entry" (not (isJust (findListNode findingList 7))))
    ]

checkingList :: List Int String -> [Int] -> Bool
checkingList list [] = True
checkingList list [h] = isJust(findListNode list h)
checkingList list (h:t) = 
    if isJust(findListNode list h)
    then checkingList list t 
    else False

generateNewList :: List Int String -> [Int] -> List Int String
generateNewList list [] = list
generateNewList list [h] = insertListNode list h "inside the list"
generateNewList list (h:t) = generateNewList (insertListNode list h "inside the list") t 

insertListTests :: [Int] -> Property
insertListTests keys = 
    not(null keys) ==>
    property(checkingList (generateNewList emptyList keys) keys) 

listTestPairs :: [Int] -> [(Int,String)]
listTestPairs [] = []
listTestPairs (h:t) = [(h,"inside the list")] ++ listTestPairs t

removeClones_list :: [Int] -> [Int] -> [Int] --current rest --> output
removeClones_list current [] = current --If no more to remove, output current
removeClones_list current (h:t) = 
    let x = if elem h current then [] else [h]
    in removeClones_list (current ++ x) t

allListNodesTests :: [Int] -> Property 
allListNodesTests keys =
    not (null keys) ==>
    let 
        check_keys = removeClones_list [] keys
        list = generateNewList emptyList keys
    in property( (listAllNodes list) == (listTestPairs check_keys) )

removeListTest :: Test
removeListTest = 
    let 
        keys = [0,5,9,-4,4,2,-1,8,-3,13,12,3,7,10,6]
        list = generateNewList emptyList keys
        removeListTest_A = removeListNode list 6
        check_A = delete 6 keys
        removeListTest_B = removeListNode list (-3)
        check_B = delete (-3) keys
        removeListTest_C = removeListNode list 0
        check_C = delete 0 keys
    in TestList [
        TestCase (assertBool "Remove list A (first entry)"   (not (isJust(findListNode removeListTest_A 6)))),
        TestCase (assertBool "Remove list A (other entries)" (checkingList removeListTest_A check_A)),
        TestCase (assertBool "Remove list B (middle entry)"  (not (isJust(findListNode removeListTest_B (-3))))),
        TestCase (assertBool "Remove list B (other entries)" (checkingList removeListTest_B check_B)),
        TestCase (assertBool "Remove list C (last entry)"    (not (isJust(findListNode removeListTest_C 0)))),
        TestCase (assertBool "Remove list C (other entries)" (checkingList removeListTest_C check_C))
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

checkingList_NOT :: List Int String -> [Int] -> Bool
checkingList_NOT list [] = True
checkingList_NOT list [h] = isNothing(findListNode list h)
checkingList_NOT list (h:t) = 
    if isNothing(findListNode list h)
    then checkingList_NOT list t 
    else False

removeIfListTests_att1 :: Test 
removeIfListTests_att1 =
    let
        keys = [0,5,9,-4,4,2,-1,8,-3,13,12,3,7,10,6]
        list = generateNewList emptyList keys 
        testList_A = removeListNodeIf list condition_A
        testList_B = removeListNodeIf list condition_B
        testList_C = removeListNodeIf list condition_C
        (check_A,check_NOT_A) = removeIf_keys keys condition_A
        (check_B,check_NOT_B) = removeIf_keys keys condition_B
        (check_C,check_NOT_C) = removeIf_keys keys condition_C
    in TestList [
        TestCase (assertBool "RemoveIf list A (remaining nodes)" (checkingList     testList_A check_A)),
        TestCase (assertBool "RemoveIf list A (removed nodes)"   (checkingList_NOT testList_A check_NOT_A)),
        TestCase (assertBool "RemoveIf list B (remaining nodes)" (checkingList     testList_B check_B)),
        TestCase (assertBool "RemoveIf list B (removed nodes)"   (checkingList_NOT testList_B check_NOT_B)),
        TestCase (assertBool "RemoveIf list C (remaining nodes)" (checkingList     testList_C check_C)),
        TestCase (assertBool "RemoveIf list C (removed nodes)"   (checkingList_NOT testList_C check_NOT_C))
        ]

removeIfListTests_att2 :: Test 
removeIfListTests_att2 =
    let
        keys = [0,0,0]
        list = generateNewList emptyList keys 
        testList_A = removeListNodeIf list condition_A
        testList_B = removeListNodeIf list condition_B
        testList_C = removeListNodeIf list condition_C
        (check_A,check_NOT_A) = removeIf_keys keys condition_A
        (check_B,check_NOT_B) = removeIf_keys keys condition_B
        (check_C,check_NOT_C) = removeIf_keys keys condition_C
    in TestList [
        TestCase (assertBool "RemoveIf list A (remaining nodes)" (checkingList     testList_A check_A)),
        TestCase (assertBool "RemoveIf list A (removed nodes)"   (checkingList_NOT testList_A check_NOT_A)),
        TestCase (assertBool "RemoveIf list B (remaining nodes)" (checkingList     testList_B check_B)),
        TestCase (assertBool "RemoveIf list B (removed nodes)"   (checkingList_NOT testList_B check_NOT_B)),
        TestCase (assertBool "RemoveIf list C (remaining nodes)" (checkingList     testList_C check_C)),
        TestCase (assertBool "RemoveIf list C (removed nodes)"   (checkingList_NOT testList_C check_NOT_C))
        ]

removeIfListTest_auto_A :: [Int] -> [Int] -> [Int] -> Property
removeIfListTest_auto_A keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = removeClones_list [] (keys1 ++ keys2 ++ keys3)
        --putStrLn keys
        list = generateNewList emptyList keys 
        testList = removeListNodeIf list condition_A
        (check,check_NOT) = removeIf_keys keys condition_A
        -- putStrLn check
        -- putStrLn check_NOT
    in
        property( (checkingList testList check) && (checkingList_NOT testList check_NOT) )

removeIfListTest_auto_B :: [Int] -> [Int] -> [Int] -> Property
removeIfListTest_auto_B keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = removeClones_list [] (keys1 ++ keys2 ++ keys3)
        list = generateNewList emptyList keys 
        testList = removeListNodeIf list condition_B
        (check,check_NOT) = removeIf_keys keys condition_B
    in 
        property( (checkingList testList check) && (checkingList_NOT testList check_NOT) )

removeIfListTest_auto_C :: [Int] -> [Int] -> [Int] -> Property
removeIfListTest_auto_C keys1 keys2 keys3 = 
    not (null keys1 || null keys2 || null keys3) ==>
    let
        keys = removeClones_list [] (keys1 ++ keys2 ++ keys3)
        list = generateNewList emptyList keys 
        testList = removeListNodeIf list condition_C
        (check,check_NOT) = removeIf_keys keys condition_C
    in 
        property( (checkingList testList check) && (checkingList_NOT testList check_NOT) )

main = do
    putStrLn("\nLinked list tests\n")

    runTestTT emptyListTests
    runTestTT findListTests
    quickCheck insertListTests
    quickCheck allListNodesTests
    runTestTT removeListTest
    runTestTT removeIfListTests_att1
    runTestTT removeIfListTests_att2
    quickCheck removeIfListTest_auto_A
    quickCheck removeIfListTest_auto_B
    quickCheck removeIfListTest_auto_C

    return ()