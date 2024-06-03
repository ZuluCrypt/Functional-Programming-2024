open System
// will use a list to store numeral
type Numeral =
    |I
    |V
    |X
    |L
    |C

type Converted =
    |Arabic of int
    |Roman of Numeral list
//**************auxillary functions*************************
let reverseList x =
    let rec reverse output l=
        match l with
        |[] -> output
        |a::rest -> reverse (a::output) rest
    reverse [] x

//*********************************************************


//oppperational functions**********************************
//VALUE OF
let valueOf =
    function
    |I -> 1
    |V -> 5
    |X -> 10
    |L -> 50
    |C -> 100


let additive x =
    let rec split value acc =
        if value = 0 then 
            acc
        elif value >= 100 then
            split (value - 100) (C::acc)
        elif value >= 50 then
            split (value - 50) (L::acc)
        elif value >= 10 then
            split (value - 10) (X::acc)
        elif value >= 5 then
            split (value - 5) (V::acc)
        else
            split (value - 1) (I::acc)        
    if x <= 0 then 
        []
    else 
        split x [] |> reverseList

// additive 23


let rec addlist list acc =
    let rec add =
        function
        |[]-> 0
        |a::rest -> a + add rest
        
    match list with
    |[]-> add(acc)
    |a::rest -> addlist rest ((valueOf<|a)::acc)

let rec value (list:Numeral list)=
    function 
    |[] -> None
    |_::_-> Some(addlist(list)) 

let eval x =
    Roman(additive x)
   
let result x  = String.replicate (String.length x) x
result "moo"
eval 2    
eval 573
//*****************************************