
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

type MyList2<'a>=
    |Empty2
    |Elements of 'a * MyList2<'a>

let push2 v l = Elements(v,l)

let one v = Elements(v,Empty2)

let rec length2 = 
    function
    |Empty2 -> 0
    |Elements(_,rest) -> 1 + length2 rest
let q9 v list = 
    let rec insert i output l =
        match (i,list) with
        |(_,Empty2) -> output
        |(0,Elements(_, rest)) -> insert (-1) (push2 v output) rest
        |(_,Elements(a, rest)) -> insert (i - 1) (push2 a output) rest
    insert ((length2 list)/2) (Empty2) (list)

let list = Elements(1,Elements(2,Empty2))
q9 3 list