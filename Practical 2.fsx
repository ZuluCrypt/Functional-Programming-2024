(*Title:    F# Practical 2
  Author:   Thandokuhle Zulu
  Date:     2024/02/27 - 2024/03/05
  
  Week 3 practical, in the following practical we looked at ways of working with different recursive data types
  , the focus was on how to work with lists and trees*)

(************************           WARM UP           **********************************************************)
//funtion to check if a number is a prime number
    (*algorithm:
        1-if number is less then or equal to 1 or number divisible by x then it is a prime
        2-if number can divide by itself then its a prime
        3- if number that can divide number is not found recal the function until you find it 
        *)
let rec checkPrime number x = 
    if number <= 1 || number % x = 0 then   
        false
    elif x >= number then
        true
    else 
        checkPrime number (x + 1)
// tests for checkPrime
( checkPrime 17 2 = false
, checkPrime 25 2 = false
, checkPrime 99 2 = false
, checkPrime 97 2 = true
, checkPrime 2 2 = true
, checkPrime 1 2 = false
, checkPrime 0 2 = false
, checkPrime -5 2 = false
, checkPrime -39 2 = false)

// adjusted function to control user input and improve code to have an inner function dealing
//with the recursion
let betterCheckPrime number =
    let rec checkPrimeCandidate x = 
        if  number <= 1 || number % x = 0 then  
            false 
        elif x >= number then
            true
        else    
            checkPrimeCandidate (x + 1)

    if number <= 1 then
        false
    else 
        checkPrimeCandidate 2

//teste for betterCheckPrime    
( betterCheckPrime 17  = false
 , betterCheckPrime 25  = false
 , betterCheckPrime 99  = false
 , betterCheckPrime 97  = true
 , betterCheckPrime 0  = false
 , betterCheckPrime -5 = false
 , betterCheckPrime -39  = false
 )




(**   lISTS    **)
//  list recursive data type defining a recursive list
(**#######################Type creation###########################*)
type MyList<'data> = 
    | Empty
    | Element of 'data * MyList<'data>
(*################################################################**)

//some lists
let mylist = Element(80,Empty)
let stringList = Element ("hi",Element("hello", Empty))
let list1 = Element ("Scorpion", Element("Tiger ", Empty))

// construction function to push values into the list
let push value list = Element(value, list) 
// its equivalent to saying Element(80,Empty)


//function to reverse a list
let reverse list =
    //inner recursive function 
    let rec reversal output list =
        (function
            |Empty -> output
            |Element (x, remaining) -> 
                reversal (push x output) remaining
        )list
    reversal Empty list

reverse stringList

// built-In list reversing function
let listReverse list =
    let rec reversal output someList = 
        (function
            | [] -> output
            | a::tail -> reversal (a::output) tail// the action happens here we have an accumalator which gets a value added to it everytime a recursive call is made
            )someList
    reversal [] list


listReverse [1;2;3]
//list take is a function to take a specific number of values from another list
let rec listTake n l =

    (function
        | (0, _) -> [] // if we match 0 by unit(something) give me []
        | (_, []) -> []// if we match something and empty list [] give me []
        | (_, h::t) ->  h::(listTake (n-1) t)(*if we match some by 2-elemet list
                                                then cons h to a new list to the
                                                results of the next list which is 
                                                n-1 and tail of the previous list*)
    ) (n, l)
listTake 2 [1;2;3;4;2;3;42]

(**####################################    Trees      ###############################################*)

(*in this section we look at trees which represent a Binary search tree*)

(************************************tree type definition*******************************)
type Tree<'a> =
    |EmptyTree
    |TwoChildren of 'a * Tree<'a> * Tree<'a>
    |LeftOnly of 'a * Tree<'a>
    |RightOnly of 'a * Tree<'a>
    |NoChildren of 'a

//some trees
let tree1 =TwoChildren(21,TwoChildren(20,LeftOnly(19,NoChildren(18)),NoChildren(22)),RightOnly(30,NoChildren(25)))

EmptyTree

RightOnly(2,EmptyTree)


//function to trancate throu the tree (left)
let left =
    function
        |EmptyTree -> EmptyTree
        |RightOnly  _-> EmptyTree
        |NoChildren _ -> EmptyTree
        |LeftOnly(_,t)-> t
        |TwoChildren (_,t,_) -> t



// function to trancate throu the tree  (Right) function
let right = 
    function
        |EmptyTree -> EmptyTree
        |LeftOnly _ -> EmptyTree//effectively we can use _ instead of (_,_)
        |NoChildren _ -> EmptyTree
        |RightOnly(_,t)-> t
        |TwoChildren(_,_,t) -> t

//test for the left and right
right tree1
left tree1

//Node Left Right (pre-order trancate)
let rec treeNLR =
    function
        |EmptyTree -> []
        |NoChildren x -> [x]
        |LeftOnly (x,xl) -> [x] @ (treeNLR xl)
        |RightOnly (x,xr) -> [x] @ (treeNLR xr)
        |TwoChildren(x,xl,xr)-> ([x] @ (treeNLR xl)) @ (treeNLR xr)

//some test
treeNLR tree1


(************PRACTICAL BELOW****************)


//some functions---------------------------------------------------------------
let rec length  =
    function
        | Empty -> 0
        | Element(a,remaining)-> 1 + length remaining

let rec Built_in_Length  =
    function 
        | []-> 0
        | a::remaining -> 1 + Built_in_Length remaining

// length [1;2;3;6;3;2;5;6;3;]
Built_in_Length [1;2;3;6;3;2;5;6;3;]

let rec lastElement =
    function
        |Empty -> Empty
        |Element(a,Empty) -> a
        |Element(_,tail) -> lastElement tail
//----------------------------------------------------------------------------


// [1] listdrop
let rec listDrop n list =
    
    (function
        | (0, _) -> list// if we match 0 by unit(something) give me the list
        | (_, []) -> []// if we match something and empty list [] give me []
        | (_, _::t) -> listDrop (n-1) t (*the recursive call will remove an item*
                                        from list everytime its called effectively
                                        decreasing the list*)
    ) (n, list)//list drop

listDrop 3 [3;4;2;3;1;2;42] //test


//-------------------------------------------------------------------------------------------
//sortlist function aids me in sorting a list
let rec sortList list =
    let rec insert x = 
        function
            | [] -> [x]
            | h::t -> if x <= h then x :: h :: t else h :: insert x t
        
    (function
        | [] -> [] //empty list return empty
        | h::t -> insert h (sortList t)//some items on list return insert with head and
        )list

sortList  [3;4;2;3;1;2;42] 
//-----------------------------------------------------------------------------------------

//[2] list insert
// inserts an element to a list in the right place
let listInsert v list =
    (function
        |(_,[]) -> []// if we match something and empty list return empty list
        |(a, x::tail) -> 
            (a::list) //adding value to list 
                |> sortList // sorting the list
    )(v,list)

listInsert 5  [3;4;2;3;1;2;42]

//[3] insert function for trees
// insert function for tree
let rec insert v tree =
    match tree with
        |EmptyTree  -> NoChildren v

        |TwoChildren(x,treeL, treeR)->
            if v = x then 
                TwoChildren(v, treeL, treeR)
            elif v < x then 
                TwoChildren(x,insert v treeL, treeR)
            else
                TwoChildren(x,treeL,insert v treeR)

        |LeftOnly(x, treeL)->
            if v = x then 
                LeftOnly(x,treeL)
            elif v < x then 
                TwoChildren(x, insert v (LeftOnly(v, EmptyTree)), treeL)
            else
                TwoChildren(x, treeL, insert v (NoChildren v))

        |RightOnly(x, treeR)->
            if v = x then  
                RightOnly(x , treeR)
            elif v > x then
                TwoChildren(x, insert v (RightOnly(v, EmptyTree)) ,treeR)
            else 
                TwoChildren(x, insert v (NoChildren v),treeR)
            
        |NoChildren (x) -> 
            if v = x then   
                NoChildren(x)
            elif v < x then
                LeftOnly(x, insert v (NoChildren v))    
            else
                RightOnly(x, insert v (NoChildren v))    
      
//some tests
let tree3 = insert 2 (TwoChildren(1,LeftOnly(1,NoChildren 3),RightOnly(4,NoChildren 5)))
let tree4 = insert 2 (TwoChildren(45,NoChildren(20),NoChildren(50)))
let tree2 = insert 2 tree1


//[4] tree LNR 
let rec treeLNR =
    function
        |EmptyTree -> []
        |NoChildren x -> [x]
        |LeftOnly (x,xl) -> (treeLNR xl) @ [x] //remember if we want to go left first we recurse till there first
        |RightOnly (x,xr) -> [x] @ (treeLNR xr)  
        |TwoChildren(x,xl,xr)-> (treeLNR xl) @ ([x]  @ (treeLNR xr))//
//some tests
treeNLR tree3
treeNLR tree4
treeLNR tree3
// LeftOnly(2,EmptyTree)


//some lists
let list15 = [5;3;5;4;9;5;1]
let list2 = [55;25;829;43;23;543]
let list4 = [477;4672;83;8732]
let list3 = [list15;list2;list4]

(**End of prac**)




(*Homework below*)

(*########################################Homework#############################*)
//[1]  list merge
//merges lists, sorts it and removes duplicates

//function to remove duplicates in a list
let rec filterDuplicates list =
    //helper function to check for duplicates
    
    let rec hasDuplicates a = 
        function
            | [] -> false
            | x :: xs -> x = a || hasDuplicates a xs
     

    (function
        |[] -> []
        |a::remaining -> if hasDuplicates a remaining then 
                            filterDuplicates  remaining
                         else
                            a::filterDuplicates  remaining
    )list

let listMerge lists =
    let rec concat list =
        (function
            |[]-> []
            |head::tail -> head @ concat tail // concatonate the first list to the rest of the list
        )list
    
    concat lists |> sortList |> filterDuplicates


listMerge list3



//[2]treeMerge
let trees =  [treeNLR tree3;treeNLR tree4;treeNLR tree1]

let rec treeMerge list = 
    let rec buildTree output sortedList =
        (function
            |[] -> output
            |a::remaining -> buildTree ( insert a output) remaining
            //recursively call function while inserting the fist item in the list
        )sortedList

    listMerge list |> buildTree EmptyTree 

//some tests
treeLNR (treeMerge trees)