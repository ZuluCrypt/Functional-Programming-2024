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


//Prac 2
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
