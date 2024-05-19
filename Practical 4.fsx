(*
    Name: Thandokuhle Zulu
    Title: Practical 4: focus on converting partial functions to total functions
    Date: 13/03/2024
*)

type Result<'a, 'b> =
    |Error of 'a
    |Result of 'b
//introduction to built in functions
List.last // this function allows for extracting the last element of a list
(* List.init is a function that takes in an index and brings back that specific number of
elements specified by the index applyiung the function to each elemenent*)
List.init 4 (fun v -> v + 5)
List.init 6 (fun x -> $"Item #{x}" )
List.init 1 (fun v -> v)

List.last [1]

List.last [1;4;3;4]

(*function that:
    a. accepts a list;
    b. gets the last number in the list;
    c. creates a new list of that size with the values 1…size, and returns it.
It might look something like this:*)

// let toyExample xs = 
//     List.init (List.last xs) (fun x -> x + 1)

//toyExample [5;2;6]
//toyExample []// get an error when trying to apply with an empty list
//toyExample [4;5;-1], error is produced when trying to use -1 as a length specifier

//all these built-in functions are pure all, i get exceptions and errors when i give the unexpected inputs

//Converting built-in functions to total function



let rec listLast =
    function
    |[x] -> Some x
    |[] -> None
    |_::t -> listLast t

List.tryLast [-1]




let listInit length initializer =
    let rec doInit acc n =
        if n = length then
            List.rev acc
        else
            doInit (initializer n:: acc) (n + 1)
    
    if length < 0 then  
        None
    else
        Some (doInit [] 0)




// let toyExample xs =
//     (function
//         |None -> None
//         |Some a -> listInit a (fun x -> x + 1)
//         (*we get the output as an Option type in this case Some value because the other
//         case is a some type*)
//     )(List.tryLast xs) //this parameter will either return None or Some (output)

// toyExample [2]

//COmpositions and piping
let x = 4
let f = fun a -> a + 3
let g = fun v -> v * 2
let h = f >> g

x |> f |> g
f x |>  g
x |> (f >> g)
x |> h
h x



//the  bind function : this function only calls the next function if the returned value is satisfactory
let bind f = 
    function    
        |None -> None
        | Some x -> f x

// we can used our toyExample with piping
//for exapmple



let toyExample xs =
    List.last xs
    |> fun n -> List.init n (fun x -> x + 1) 




let toyExampleB xs = 
    List.tryLast xs // depending on this output (None / Some value) we will call the next function
    |> bind( fun n -> listInit n (fun x -> x + 1) )
    (*this higher-order function allows us to precheck if the input in acceptable
    and will get used*)
// Success!! ToyExampleB is now Total
toyExampleB [1]




let map f =
    function    
        |None -> None
        |Some n -> Some (f n)  
        (*transformation happens here, we changing the value transforming
        it to an Option<'a> type output
        *)
listInit 5 (fun x -> x * 3)
|> map(List.filter (fun x -> x % 2 = 0))




// we might need a function to give out a normal output "a Non-Option<'a>"
let defaultValue normalVal =
    function
        |None -> normalVal
        |Some v -> v


//Homework below


List.average [6.0;4.0;5.0;8.0]
let  listAverage list=
    let rec calcAverage acc result inputList =
        match inputList with    
            |[] -> Result (result/acc)
            |a::rest -> calcAverage (1.0 + acc) (a + result) rest 

    (function
        |[]-> Error "Empty list" 
        |_::_-> calcAverage (0.0) (0.0) list
    )list //listAverage

listAverage []
listAverage [6.0;4.0;5.0;8.0]
List.averageBy (fun x -> x + 2.0) [6.0;4.0;5.0;8.0]



let listAverageBy f list =
    let rec calcAverage acc result inputList=
        match inputList with    
            |[] -> Some (result/acc)
            |a::rest -> calcAverage (1.0 + acc) (f a + result) rest

    (function
        |[] -> None
        |_::_ -> calcAverage (0.0) (0.0) list 
    )list // listAverageBy
    
listAverageBy (fun x -> x + 2.0) []
listAverageBy (fun x -> x + 2.0) [6.0;4.0;5.0;8.0]



List.insertAt 2 4 [4;5;6;7;8]

let listInsertAt index value list =
    let rec insertAt index acc inputList =
        (function
            |(_,[]) -> Result acc
            |(0,a::rest) -> insertAt (-1) (value::a::acc) rest 
            |(_,a::rest) -> insertAt (index - 1) (a::acc) rest
        )(index,inputList)


    if index < 0 || index > List.length list then
        Error "index out of bounds"
    else
        insertAt index [] (List.rev list)
        //listInsertAt
listInsertAt 2 4 []
listInsertAt 2 4 [4525;22;8;553;55;5]


//max by
List.maxBy (fun (_,y,_) -> y) [("Yusuf", 60, 5.4);("Bill", 30, 3.2);("Xky", 83, 2.9)]
let listMaxBy projFunc list = //projFunc = projection function
    match list with
        |[] -> Error("Empty list")
        |_-> Result(List.maxBy projFunc list)//ListmaxBy

listMaxBy (fun v -> v + 1 ) []



type NonEmptyList<'a> =
    |Something of 'a
    |Element of 'a * NonEmptyList<'a> 


    

//Map
let rec mapL f list =
    (function
        |Something a -> Something(f a)
        |Element(a,rest) -> Element(f a, mapL f rest )
    )list //mapL
let transformed = mapL (fun x -> x + 2)(Something(1))




//filter
let rec filter pFunc list =
    (function 
        |Something a -> if pFunc a then Some(Something a) else None
        |Element(a, rest ) -> 
                            if pFunc a then
                                (match filter pFunc rest with
                                | Some filteredRemaining -> Some (Element (a, filteredRemaining))
                                | None -> Some (Something a)
                                )
                            else 
                                filter pFunc rest
                                                        
    )list 
let filtered = filter (fun x -> x > 5 ) (Element(3,Something(3)))




//fold
let rec fold f state list =
    (function
        |Something a -> f state a
        |Element(a,rest ) -> fold f (f state a) rest    
    )list //fold

fold (fun x y -> 1 + x) 0 (Element(3,Something(3)))





//copypaste
let copypaste list start num idx =
    let len = List.length list//list length 
    let rec copyItems output copyFrom start_from end_at =
        (function
            |(_,_,[]) -> output
            |(_,0,_) -> output
            //if we done copying the numbers we want give back the output
            |(0,_,a::rest) -> copyItems (a::output) rest 0 (end_at - 1)
            //if the index we start from is 0 we start coping
            |(_,_,a::rest)-> copyItems output rest (start_from - 1) end_at
            // we were trying to copy items
        )(start_from,end_at,copyFrom)

    let rec listIndex copiedFrom topaste at_index acc =
        (function
            |(_,[],_) -> acc
            |(0,a::rest,[])-> listIndex rest topaste 0 (a::acc)
            |(0,_,a::rest) -> listIndex copiedFrom rest 0 (a::acc)
            |(_,a::rest,_)-> listIndex rest topaste (at_index - 1) (a::acc)
        )(at_index,copiedFrom,topaste) 
    
    match list with
            
        |[] -> None
        |_ -> if start < 0 || num > len || start > len then 
                None
              elif(idx < 0 || idx > len ) then
                None
              else 
                  Some (listIndex 
                            list 
                            (copyItems [] list start num |> List.rev) 
                            idx 
                            [] |> List.rev
                        )
                     //copypaste
    

copypaste [45;54;4;54;] 1 4 4




//MakeOracle
let MakeOracle (word:string) =
    let len = String.length word
    let rec count (str:string) i counter char =
        if i >= len then    
            (if counter = 0 then None else Some counter)
        elif str.[i] = char then
            count  str (i + 1) (counter + 1)char
        else 
            count str (i + 1) counter char 
    count word 0 0
let oracle  = MakeOracle "Hello, world! This is me."

(oracle 'H', 
oracle 'h' ,
oracle 'e', 
oracle ' ' ,
oracle 'z') 

