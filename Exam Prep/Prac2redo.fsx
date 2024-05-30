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

