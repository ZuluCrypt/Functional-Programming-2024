// in this practical i will be looking at higher order functions and how to use them

type myList<'a> =
    |Empty
    |Elements of 'a * myList<'a>
//******************Auxillary functions********************************
let push v l = Elements(v,l)//push
let rec length list =
    match list with
    |[] -> 0
    |_::rest -> 1 + length rest 
    //length for built in list
let rec length2 list =
    match list with
    |Empty -> 0
    |Elements(_,nested) -> 1 + length2 nested
    //length for generic list
let reverse list=
    let rec rev output l =
        match l with
        |Empty -> output
        |Elements(a,rest)-> rev (push a output) rest
    rev Empty list
let reverse2 list =
    let rec reversing output l =
        match l with
        |[] -> output
        |a::rest -> reversing (a::output) rest
    reversing [] list
    
reverse2 [1;2;3]
//********************************************************************************
//List related Higher order functions
//list.Map, dirrect mapping for we usingthe same structure to create the new list
let rec B_listMap func =
    function
    |[] -> []
    |a::rest -> func a :: B_listMap func rest

B_listMap (fun x -> x + 2) [2;5;3;2;5]

//list map for generic list, Direct mapping, Using the same datastructure
let rec genListMap func=
        function
        |Empty -> Empty
        |Elements(a,rest) -> Elements(func a, (genListMap func rest))

//Tail recusive and accumalator version(Good for bigger data, doesnt reach stack overflow)
let rec genListMap2 func =
    let rec map output=
        function
        |Empty-> reverse(output)
        |Elements(a,rest) -> map (push (func a) output) rest
    map Empty 
  

genListMap2 (fun x -> x + 2) (Elements(2,Elements(3,Empty)))

//filter Functions, direct filtering FOR BUILT_IN LIST
let rec filter pFunc =
    function
    |[] -> []
    |a::rest -> if pFunc a then 
                    a::filter pFunc rest 
                else 
                    filter pFunc rest

//tail recursive filter for built_IN LIST
let filter2 pFunc =
    let rec filterList output =
        function
        |[] -> reverse2(output)
        |a::rest -> if pFunc a then filterList (a::output) rest else filterList output rest
    filterList []  

filter (fun x -> x < 2) [2;5;1;0;5;4]
filter2 (fun x -> x < 2) [2;5;1;0;5;4]


let rec count digit =
    if digit < 0 then
        count (digit * -1)
    elif digit < 10 then
        1
    else 
        1 + count(digit / 10)

count 55376753

let last digit = digit % 10 
    
type ElementOrNothing<'a> =
    |Nothing
    |Element of 'a
let firsAndLast v digit=
    let length = count digit
    let rec findFirst output l =
        match l with
        |[] -> Nothing 
        |a::rest -> if a = v then Element(output) else findFirst (output + 1) rest

    let rec findLast output l =
        match l with
        |[] -> Nothing
        |a::rest -> if a = v then Element(output) else findLast (output - 1) rest 
    //set up environment to return a tuple
    let findIndexes list =
        match list with
        |[] -> (Nothing,Nothing)
        |_-> (findFirst 0 list, findLast (length - 1) list)

    // put evry digit in a list
    let rec buildlist number output =
        if number  < 0 then
            buildlist (digit * -1) output
        elif number < 10 then
            number::output
        else
            buildlist (number / 10 )(last number::output)

    findIndexes (buildlist digit []) 

firsAndLast 3 35553553
   

fun f x -> f x

fun c d -> d(d c + ".")

let rec count2 =
    function
    |digit when digit < 10 -> 1
    |digit when digit < 0 -> count2(digit * -1) 
    |digit when digit > 10 -> 1 + count2(digit / 10)


count2 666

let rec dropElements list no =
    match (no,list) with
    |(0,_) -> list
    |(_,[]) -> []
    |(_, _::rest) -> dropElements rest (no - 1)

dropElements [1;2;3;4] 2
