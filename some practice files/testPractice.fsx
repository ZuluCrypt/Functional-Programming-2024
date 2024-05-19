type genericList<'a> =
    |Empty
    |Data of 'a * genericList<'a>

let rec length list =
    match list with
        |Empty -> 0
        |Data(a, rest) -> 1 + length rest


let first list =
    match list with 
        |Empty -> None
        |Data(a,rest) -> Some(a)


let rec last list =
    match list with 
        |Empty -> None
        |Data(a,Empty) -> Some a
        |Data(a, rest ) -> last rest
let list = Data(3,(Data(2,Empty)))
length list
first list
last list

let  f list =
    let rec tupleMe acc list =
        match list with 
            |[] -> acc
            |[x] -> (x)::acc
            |x::y::xs -> tupleMe ((x::y)::acc)) xs

    tupleMe [] list

f [3;4;5;6;7;8]

let rec last digit =
    if digit < 0 then
        last (digit * -1)
    else 
        digit  % 10

last 65452

let rec first digit =
    if digit < 0 then
        first (digit * -1)
    elif digit < 10 then    
        digit
    else 

        first (digit / 10) 


first 2437

let rec indexAt digit index =
    if digit < 0 then   
        indexAt (digit * -1) index
    elif index = 0 then
        last(digit)
    else 
        indexAt (digit/10) (index - 1)

indexAt 5752 3