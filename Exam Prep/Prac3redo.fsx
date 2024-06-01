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
let rec reverse2 =
    function
    |[] -> []
    |a::rest -> a::reverse2 rest

let test = reverse2 [1;2;3]
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