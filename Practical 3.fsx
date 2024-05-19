(*
    Author: Thandokuhle Zulu
    Date: 07/03/2024
    Practical 3


*)
//tree data type
type Tree<'a> =
    |EmptyTree
    |NoChildren of 'a
    |LeftOnly of 'a * Tree<'a>
    |RightOnly of 'a * Tree<'a>
    |TwoChildren of 'a * Tree<'a> * Tree<'a>


//Map function for lists
let rec listMap func list =
    (function  
        |[] -> []
        |a::rest -> func a::listMap func rest
     )list

listMap (fun a -> a + 2) [0; 2; 3; 4]
listMap (fun b -> (b, b)) ["yo"; "lo"; "hi"; "ho"]
listMap (fun c -> if c % 2 = 0 then "even!" else "odd!") [9; 0; 2; 1; 4]


//Tree Map function for tree structures

let rec treeMap func tree =
    (function
        |EmptyTree ->
             EmptyTree
        |NoChildren (a)  -> 
            NoChildren(func a)
        |LeftOnly(a,remainingtree) -> 
            LeftOnly(func a, treeMap func remainingtree)
        |RightOnly(a, remainingtree) ->
             RightOnly(func a, treeMap func remainingtree)
        |TwoChildren(a,remainingTreeL,remainingTreeR)-> 
            TwoChildren(func a,treeMap func remainingTreeL,treeMap func remainingTreeR)
    )tree

let myTree =TwoChildren(21,TwoChildren(20,LeftOnly(19,NoChildren(18)),NoChildren(22)),RightOnly(30,NoChildren(25)))
treeMap (fun x -> x + 1) myTree
treeMap (fun y -> y % 10) myTree

//filter Function

let rec listFilter pFunc list =
    (function
        |[]-> []
        |a::rest -> if pFunc a then
                        a::listFilter pFunc rest
                    else
                        listFilter pFunc rest
    )list


listFilter (fun x -> x % 2 = 0) [9;0;2;1;0]
listFilter (fun y -> y % 2 = 1) [9;0;2;1;0]
listFilter (fun z -> z < 22) [9;0;2;1;0]

// some functions for trees
let rec treeInsert v tree =
    (function
        |EmptyTree  -> NoChildren v

        |TwoChildren(x,treeL, treeR)->
            if v = x then 
                TwoChildren(v, treeL, treeR)
            elif v < x then 
                TwoChildren(x,treeInsert v treeL, treeR)
            else
                TwoChildren(x,treeL,treeInsert v treeR)

        |LeftOnly(x, treeL)->
            if v = x then 
                LeftOnly(x,treeL)
            elif v < x then 
                TwoChildren(x, treeInsert v (LeftOnly(v, EmptyTree)), treeL)
            else
                TwoChildren(x, treeL, treeInsert v (NoChildren v))

        |RightOnly(x, treeR)->
            if v = x then  
                RightOnly(x , treeR)
            elif v > x then
                TwoChildren(x, treeInsert v (RightOnly(v, EmptyTree)) ,treeR)
            else 
                TwoChildren(x, treeInsert v (NoChildren v),treeR)
            
        |NoChildren (x) -> 
            if v = x then   
                NoChildren(x)
            elif v < x then
                LeftOnly(x, treeInsert v (NoChildren v))    
            else
                RightOnly(x, treeInsert v (NoChildren v))    
    )tree

//NLR traverse function
let rec treeNLR =
    function
        |EmptyTree -> []
        |NoChildren x -> [x]
        |LeftOnly (x,xl) -> [x] @ (treeNLR xl)
        |RightOnly (x,xr) -> [x] @ (treeNLR xr)
        |TwoChildren(x,xl,xr)-> ([x] @ (treeNLR xl)) @ (treeNLR xr)

//function to create a tree from list
let listToTree list =
    let rec makeTree outputTree =
        (function
            |[] -> outputTree
            |a::rest-> makeTree (treeInsert a outputTree) rest
        )
    makeTree EmptyTree list

let treeFilter pFunc tree =
    listToTree(listFilter pFunc (treeNLR tree))
    (*we are building the tree from (filtering the ( NLR order-list) )*)

treeFilter (fun x -> x % 2 = 0) myTree

//fold function
let rec listFoldL func acc list =
   ( function
        |[] -> acc
        |a::rest -> listFoldL func (func acc a) rest
   )list

let rec listFoldR func acc list =
    (function
        |[] -> acc
        |a::rest -> func (listFoldR func acc rest) a
    )list

listFoldL (fun n _ -> n+1) 0 [5; 6; 9; 2; 1; 0; 3] 

(function
| [ ] -> None
| h::t -> Some(listFoldL (fun c x -> if x > c then x else c) h t)
) [8; 2; 6; -7;  5]

listFoldL (fun s x -> $"{s}{x}") "" ["hi"; "ho"; "yo"; "lo"]
listFoldR (fun s x -> $"{s}{x}") "" ["hi"; "ho"; "yo"; "lo"]

//tree Fold in Node-Left_Right NLR

let rec treeFoldNLR f acc tree =
    (function
        |EmptyTree -> acc
        |NoChildren(a) -> f acc a
        |LeftOnly(a,tree) -> treeFoldNLR f (f acc a) tree
        |RightOnly(a, tree) -> treeFoldNLR f (f acc a) tree
        |TwoChildren(a, treeL, treeR) -> treeFoldNLR f (treeFoldNLR f (f acc a) treeL)treeR 
    )tree


let tree1 =TwoChildren(23,TwoChildren(20,LeftOnly(17,NoChildren(18)),NoChildren(24)),RightOnly(30,NoChildren(25)))
let result = treeFoldNLR (fun x _ -> x+1) 0 tree1
let rec treeFoldLNR f acc tree =
    (function
        |EmptyTree -> acc
        |NoChildren(a) -> f acc a
        |LeftOnly(a,tree)-> f (treeFoldLNR f acc tree) a
        |RightOnly(a, tree) -> treeFoldLNR f (f acc a) tree
        |TwoChildren(a,treeL,treeR)-> treeFoldLNR f ( f (treeFoldLNR f acc treeL) a ) treeR
    )tree

// using fold to create treeNLR
let result2 = treeFoldLNR (fun x _ -> x+1) 0 tree1
let treeNLR_F tree = treeFoldNLR (fun acc x -> acc @ [x]) [] tree
treeNLR_F tree1
let treeLNR_F t = treeFoldLNR (fun acc x ->  acc @ [x]) [] t 
//using fold to create a reverse list function
treeLNR_F tree1
let reverse list = listFoldL(fun s x -> x::s) list

//using fold to create a list to tree function

let listToTree_F xs = listFoldL (fun s x -> treeInsert x s) EmptyTree xs

// treeExists function that returns true if a value that meets the supplied predicate exists in the tree
let rec treeExists pfunc tree =
    treeNLR tree |>
    function
        |[]-> false
        |a::rest -> if pfunc a then true else treeExists pfunc (listToTree rest)

treeExists (fun x -> x % 2 = 0) myTree

let myTree2 =TwoChildren(23,TwoChildren(21,LeftOnly(19,NoChildren(19)),NoChildren(21)),RightOnly(31,NoChildren(25)))

treeExists (fun x -> x % 2 = 0) myTree2

//listForAll
let listForAll pfunc list = 
    let rec falseExist list =
        (function 
                |[] -> true
                |a::rest -> if pfunc a = false then false else falseExist rest
        )list
    
    (function
        |[]-> false
        |_::_ -> falseExist list
    )list
    


listForAll (fun x -> x % 2 = 0) [2;4;3]
listForAll (fun z -> z < 22) [2;44;3]
listForAll (fun x -> x % 2 = 0) [2;4;3]

//listPartition function that returns a 2-tuple of lists containing value which pass the predicate, and values which do not

let listPartition pfunc list = 
    let rec failPartition pfunc list =
        (function
        |[] -> []
        |a::rest -> if pfunc a = false then a::failPartition pfunc rest else failPartition pfunc rest
        )list

    (listFilter pfunc list, failPartition pfunc list)

listPartition (fun z -> z < 22) [2;44;3]
listPartition (fun x -> x % 2 = 0) [2;4;3]

// functtion to transform specified index------
let rec MapIndex f list index =
        (function
        |(_,[]) -> []
        |(0,a::rest) -> f a::MapIndex f rest -1
        |(_,a::rest) -> a::MapIndex f rest (index - 1)
        )(index,list)

//-----------------------------------------------------
//listMapIndexed   
let listMapIndexed f list =
    let rec mappingWithIndex func list index =
       (function
        |[]-> []
        |a::rest -> func index a ::mappingWithIndex func rest (index+1)
       )list
    mappingWithIndex f list 0

listMapIndexed (fun x y -> (x , y)) [2;4;3;5;8;5;4] 
listMapIndexed (fun x y -> (x , y)) ["yes";"no"] 
//
let rec listCollect f list=
       ( function
            |[] -> []
            |a::rest -> f a @ listCollect f rest
       )list
    
     
    
listCollect (fun e-> e::[e+1;e+2]) [3;74]

//treeExcept
let treeExcept  t0 t1 = 
    let rec checkIfExist output list1 list2 =
        match list1 with
            |[] -> output
            |a::rest-> if treeExists (fun x -> x = a) list2 then 
                           checkIfExist output rest list2 
                       else 
                            checkIfExist (a::output) rest list2
          
    
    checkIfExist [] (treeLNR_F t0) t1 |> listToTree

treeExcept myTree tree1

let BST = TwoChildren(8,TwoChildren(3,NoChildren(1),TwoChildren(6,NoChildren(4),NoChildren(7))),RightOnly(10,LeftOnly(14,NoChildren(13)))) //expected to pass
treeLNR_F BST
let BST1 = TwoChildren(8,TwoChildren(3,NoChildren(1),TwoChildren(6,NoChildren(9),NoChildren(7))),RightOnly(10,LeftOnly(14,NoChildren(13))))//expected to fail
let invalidBST1 = TwoChildren(8,TwoChildren(3,NoChildren(1),TwoChildren(6,NoChildren(9),NoChildren(7))),RightOnly(10,LeftOnly(14,NoChildren(13))))
let BST2 = TwoChildren(5,TwoChildren(3,NoChildren(2),NoChildren(4)),TwoChildren(8,NoChildren(7),NoChildren(9)))//expected to pass

let invalidBST2 = TwoChildren(5,TwoChildren(3,NoChildren(2),NoChildren(4)),TwoChildren(8,NoChildren(7),TwoChildren(9,NoChildren(10),NoChildren(6))))
let singleNode = NoChildren(10)
//isBST

let rec isBST tree=
     match tree with
        | EmptyTree -> true  //if empty tree -> true
        | NoChildren _ -> true//if noChildren -> true
        | LeftOnly (a, left) -> 
            not (treeExists (fun x -> x >= a) left) && isBST left
        | RightOnly (a, right) -> 
            not (treeExists (fun x -> x <= a) right) && isBST right
        | TwoChildren (a, left, right) ->
            not (treeExists (fun x -> x >= a) left) &&
            not (treeExists (fun x -> x <= a) right) &&
            isBST left && isBST right

isBST BST
isBST BST1
isBST  invalidBST1 
isBST EmptyTree
isBST  invalidBST2
isBST singleNode

// 
let howManyICanGet c r e =
    let chocolates = c / r  // Calculate number of chocolates you can buy
    let eatAndWrappers = chocolates / e  // Chocolates eaten + remaining wrappers
    let wrappersToChoc = (eatAndWrappers / r) + (eatAndWrappers % r)  // Wrappers converted to chocolates

    let iCanGet x y z =
        x + y + z  // Total chocolates

    iCanGet chocolates eatAndWrappers wrappersToChoc




howManyICanGet 12 4 4
howManyICanGet 6 2 2


