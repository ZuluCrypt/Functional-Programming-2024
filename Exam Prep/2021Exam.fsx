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
let reverse list =
    let rec reversing output=
        function
        |[]-> output
        |a::rest -> reversing (a::output) rest
    reversing [] list
let rec q9 fmap list =
    let len = length list
    

    let rec isEven output l=
        //split the list half way
        let rec split i l acc =
            match (i,l) with
            |(0,a::rest) -> split (-1) rest (acc)
            |(_,[]) -> acc
            |(_,a::rest) -> split (i - 1) (rest) (a::acc)

        let mid = length l
        let list1 = split mid l []
        let list2 = split mid (reverse l) []
        let rec AddLists l1 l2 output =
            match (l1,l2) with
            |([],[]) -> output
            |(_,[]) -> output
            |([],_) -> output
            |(a::rest,b::nested) -> AddLists rest nested ((fmap a b)::output)
        AddLists list1 list2 []

    let rec isOdd output l =
        let rec duplicateMid output l mid =
             
            match (mid,l) with
            |(_,[]) -> output
            |(0,a::rest) -> duplicateMid (a::a::output) rest (-1)
            |(_,a::rest) -> duplicateMid (a::output) rest (mid - 1) 

        let newList = duplicateMid [] l ((length l) / 2  )
        isEven [] newList


    if len = 0 then 
        []
    elif (len % 2) = 0 then
        isEven [] list
    else
        isOdd [] list

q9 (+) [34;69;50;49;11;78]

let sos l =
    let pfunc x = x < 100
    let rec squareList list =
        match list with
        |[] -> []
        |a::rest -> (a*a)::squareList rest

    let rec filter acc list =
        (function
        |[] -> acc
        |a::rest -> if pfunc a then filter (a::acc) rest else filter acc rest
        )list
    let rec AddList l =
        match l with
        |[] -> 0
        |a::rest -> a + AddList rest 
    filter [] (squareList l) |> AddList

sos [2;3;50]