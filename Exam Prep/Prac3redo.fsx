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

//tree Functions
