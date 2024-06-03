(*
    type Result<'a,'b>=
        |Error of 'b
        |Ok of 'a
*)
let q7 =
    function
    | Error _ -> None
    | Ok x -> Some x

// Test cases
let result1 = q7 (Ok "hi")      // Expected: Some "hi"
let result2:option<int> = q7 (Error 600) 

let rec q8 pfunc fmap =
    function
    |[] -> []
    |a::rest -> if pfunc a then (fmap a)::q8 pfunc fmap rest else a::q8 pfunc fmap rest

q8 (fun x -> x<10) (( * ) 2) [8;9;10;11;12]
q8 (( = ) "no") (String.replicate 3) ["y";"u";"no";"like"]
let rec length =
    function
    |[]-> 0
    |_::rest -> 1 + length rest
let reverse =
    let rec reversing output=
        function
        |[]-> output
        |a::rest -> reversing (a::output) rest
    reversing [] 
let rec q8 fmap list =
    let length = length list
    

    let rec isEven output l=
        let reverse = reverse l
        match (l,reverse) with 
        |([],[]) -> output
        |(_,[]) -> []
        |([],_)-> []
        |(a::rest , b::_) -> isEven ((fmap a b)::output) rest 

    let rec isOdd output l =
        let rec duplicateMid output l mid =
             
            match (mid,l) with
            |(_,[]) -> output
            |(0,a::rest) -> duplicateMid (a::a::output) rest (-1)
            |(_,a::rest) -> duplicateMid (a::output) rest (mid - 1) 

        let newList = duplicateMid [] l ((length l) / 2  )
        isEven [] newList


    if length = 0 then 
        []
    elif length % 2 = 0 then
        isEven [] list
    else
        isOdd [] list