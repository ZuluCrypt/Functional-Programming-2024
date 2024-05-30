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

