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
let reverse list =
    //inner function to revers list
    let rec reversal list acc = 
        match list with
            |Empty -> acc// if list empty return acc
            |Elements(a, nested)-> reversal nested (push a acc) // if not empty cal reversal again with the new list = nested and push the head of the list to the acc
    reversal list Empty//calling the inner function
let builtinrevers list = 
    let rec builtInreversal someList acc =
        match someList with
            |[]-> acc
            |a::rest -> builtInreversal rest (a::acc)
    builtInreversal list []

let reversedList = reverse  mylist2// testing the function reverse
let builtInReversedList = builtinrevers built_in_list// testing the function builtInreversal