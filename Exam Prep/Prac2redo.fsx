//SIMPLE LIST FUNCTIONS
//===================================
//parametric polymorphic sum type of list data type
type MyList<'data> =
    |Empty
    |Elements of 'data * MyList<'data>
//created a list 
let mylist = Elements(80,Empty)
//function to add an element to the list
let push v l = Elements(v , l)
let mylist2 = push 90 mylist
let built_in_list = [80;90;60;70;50;40;30;20;10]
let myFiveElementList = Elements(10,Elements(20,Elements(30,Elements(40,Elements(50,Empty)))))
let reverse list =
    //inner function to revers list
    let rec reversal list acc = 
        match list with
            |Empty -> acc// if list empty return acc
            |Elements(a, nested)-> reversal nested (push a acc) // if not empty cal reversal again with the new list = nested and push the head of the list to the acc
    reversal list Empty//calling the inner function
let rec builtinrevers list = 
    let rec reverse acc remaining = 
        match remaining with
            |[]-> acc
            |a::rest -> reverse (a::acc) rest
    reverse [] list
let builtInReversedList = builtinrevers built_in_list//
let reverse2 list = 
    let rec reversal output =
        function
        |Empty -> output
        |Elements(a, nested) -> reversal (push a output ) nested
    reversal Empty list

let reversedList = reverse2  mylist2// testing the function reverse
//testing the function builtInreversal
// type Error<'a> =
//     |Error
//     |Value of 'a


let listtake n list =
    let rec take output count workingList =
        match (count,workingList) with
            |(0, _) -> reverse(output)
            |(_,Empty) -> Empty
            |(_,Elements(a, nested)) -> 
                take (push a output)(count - 1) nested
    take Empty n list
listtake 3 myFiveElementList

let built_in_listtake n l =
    let rec takeElements acc count listRemaining =
        match (count,listRemaining) with
        |(0, _) -> acc
        |(_, []) -> []
        |(_,a::nested) -> takeElements (a::acc)(count - 1)nested
    takeElements [] n l
built_in_listtake 3 built_in_list

//Trees
//Tree data type : this is a data type that specifies the structure of a tree using sum types.

type Tree<'a> = 
    //so will set it up how a tree would look like , visulize the structure as you build the type
    |EmptyTree
    |TwoChildren of 'a * Tree<'a> * Tree<'a> // creates a parent + two children type
    |RightOnly of 'a * Tree<'a> //creates a Parent with Right child node
    |LeftOnly of 'a * Tree<'a> //creates a Parent with left child node
    |NoChildren of 'a // crates a node only

EmptyTree
TwoChildren(2,NoChildren(1),NoChildren(4))
RightOnly(2,EmptyTree)
let test1 = TwoChildren(2.5,TwoChildren(2,NoChildren(1),NoChildren(4)),TwoChildren(4,NoChildren(6),NoChildren(8)))

//function to help us structure all neft subtree
let leftSub =
    function 
    |EmptyTree  -> EmptyTree
    |RightOnly _ -> EmptyTree
    |NoChildren _ -> EmptyTree
    |LeftOnly (_,tree) -> tree
    |TwoChildren(_,tree,_) -> tree

leftSub test1

//function to c=view the right subtree
let rightSub = 
    function
        |EmptyTree -> EmptyTree
        |LeftOnly _ -> EmptyTree
        |NoChildren _ -> EmptyTree
        |RightOnly (_,tree) -> tree
        |TwoChildren(_,_,tree) -> tree
rightSub test1


//***************************building 1 tree with blocks**********
let leftSubtree = TwoChildren(9,NoChildren 8,NoChildren 9)

let rightSubtree = TwoChildren(15,NoChildren 12,NoChildren 16)

let test2 = TwoChildren(10,leftSubtree,rightSubtree)

//********************************************************

// function for Node-Left-Right traversal
let rec treeNLR =
//remembe
    function
    |EmptyTree -> []
    |NoChildren x -> [x]
    |RightOnly (x,tree) ->  [x] @ treeNLR tree // 
    |LeftOnly(x , tree) ->  [x] @ treeNLR tree 
    |TwoChildren(x,leftTree,rightTree)-> [x] @ treeNLR leftTree @ treeNLR rightTree 

treeNLR test2

//function fro traversing the tree using left-node-right traversal

let rec treeLNR =
    function
    |EmptyTree -> []
    |NoChildren x -> [x]
    |RightOnly (x, tree) -> [x] @ treeLNR tree
    |LeftOnly(x,tree) -> treeLNR tree @ [x]
    |TwoChildren(x , lTree,rTree) -> treeLNR lTree @ [x] @ treeLNR rTree 
    //knowing the structure is important
treeLNR test2

// //tree insert function, to insert a value at the right position on the tree
// let rec treeInsert v tree=
//     match tree with
//     |EmptyTree -> EmptyTree
//     |TwoChildren(x,lTree,rTree) -> 
//         if x = v then
//             TwoChildren(v,lTree,rTree)
//         elif x<v then
//             TwoChildren(x, treeInsert v lTree, rTree)
//         else
//             TwoChildren(x,lTree, treeInsert v rTree)
//     |LeftOnly(x,tree) -> 
//         if x = v then 
//             LeftOnly(x, tree)
//         elif x < v then
//             TwoChildren(x, treeInsert v tree,)
//         else

//SIMPLE LIST FUNCTIONS
//===================================
//parametric polymorphic sum type of list data type
// type MyList<'data> =
//     |Empty
//     |Elements of 'data * MyList<'data>
//created a list 
//function to drop first n elements, notice we only returning the remaining list, not using an accummalator but just removing items as we count down
let rec listDrop n list =
    match (n,list) with 
        |(0,remaining) -> remaining
        |(_,[]) -> []
        |(_,_::rest) -> listDrop (n - 1) rest

listDrop 3 built_in_list

// create a listInsert Function which pushes a value in the right position of a sorted list
type Option =
    |None
    |Some of int

let unsorted =[8; 4; 3; 1; 6; 1]
let listSort list =
    match list with
    | [] ->[]
    | _ -> List.sort list

let listInsert v list=
    //inner recursive function
    let rec insert output sortedl =
        match sortedl with
        |[] -> output
        |a::rest -> if a > v then  
                        builtinrevers(rest @ (v::a::output))// if a is greater than v, then insert v before and leave if statement  
                    else 
                        insert (a::output) rest

    insert []  (listSort <|list)

listInsert 5 unsorted 
let lists = [[2;3;4;2];[4;4;2;1;4];[3;54;2;14]]
let listMerge lists =
    //inner recursive method to concatonate the lists
    let rec flatten output l =
        match l with
        |[] -> output
        |a::rest -> (flatten (a @ output) rest) |> listSort 
    flatten [] lists

listMerge lists

// // Example MyList values
// let list1 = Elements (1, Elements (2, Elements (3, Empty)))
// let list2 = Elements (4, Elements (5, Empty))
// let list3 = Elements (6, Empty)
// let list4 = Empty

// // List of MyList values
// let listOfMyLists = [list1; list2; list3; list4]
// // let genList = myFiveElementList , mylist
// let genFlatten lists =
//     let rec flatten output l =
//         match l with    
//         |[] -> output
//         |a::rest-> flatten ( a @ output) rest
//     flatten [] lists
// genFlatten listOfMyLists


//completing some functions
let listAverage x =
    match x with
    |[] -> 0.0
    |_ -> List.average x

listAverage [1]

type myList<'a> =
    |Something of 'a
    |Element of 'a * myList<'a>

let rec map1 f x =
    match x with
    |Empty -> Empty
    |Elements(x,rest) -> Elements(f x,map1 f rest)

map1 (fun x -> x + 1) myFiveElementList
let rec map mapf x =
    match x with
    |Something(x) -> Something(mapf x)
    |Element(x,rest) -> Element(mapf x,map mapf rest)

// let rec filter pfunc x =
//     match x with
//     |Something xa -> if pfunc xa then Some(Something (xa)) else None 
//     |Element(xa,rest) -> if pfunc xa then 
//                                             (match filter pfunc rest with 
//                                             |Some rest -> Some(Element(x,rest))
//                                             |None -> Some(Something a)
//                                             )
//                                           else 
//                                             filter pfunc rest

let rec fold f acc dS =
    match dS with
    |Something x -> f acc x
    |Element(x , rest)-> fold f (f acc x) rest


let list3 = Element(2,Element(3,Something 3))
map (fun x -> x + 1) list3
fold (fun state v-> 1 + state) 0 list3

let CopyAndPaste start numElem atIdx list =
    let rec copy startat copyabout output l =
        match (startat,copyabout,l) with
        |(_,_,[])-> output
        |(0,_,a::rest)-> copy -1 (copyabout - 1)(a::output) rest//start copying
        |(_,0,a::rest)-> a::output//stop copying)
        |(_,_,_::rest)-> copy (startat - 1)(copyabout)(output)rest
    
    let rec paste at l copy output =
        match (at,l)with
        |(_,[]) -> builtinrevers(output)
        |(0,a::rest)->paste -1 rest copy (copy@a::output)
        |(_,a::rest)-> paste (at - 1) rest copy (a::output)
     
    paste atIdx list (copy start numElem [] list) [] 


CopyAndPaste 2 1 1 [2;3;4;5;6]