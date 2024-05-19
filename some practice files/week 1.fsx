
type intList = 
    |Empty
    |Data of int * intList

let rec sumListLength data = 
    (function 
        | Empty -> 0
        | Data (a,rest) -> 1 + sumListLength rest
        )data
//biult-in listLength
let rec listLength list =
    (function   
        |[] -> 0
        |a::rest -> 1 + listLength rest
        )list

let list = Data ( 103, Data ( 400, Data ( 900, Empty ) ) )

sumListLength list
listLength [233;423;23;2;4;1;34]
let rec lastElement data = 
    (function
        |Empty -> -1
        |Data(a, Empty)-> a
        |Data(_,rest)-> lastElement rest
    )data

//built in last element
let rec lastOnList data = 
    let len = (listLength data ) - 1
    let rec lastItem n list  =
        (function
            |(_,[]) -> []
            |(0,_) -> list
            |(_,_::rest)-> lastItem (n-1) rest 
        )(n,list)
    lastItem len data 
lastElement list
lastOnList [33;4;2;5;2]

let firstElement list =
    (function 
        |Empty -> -1
        |Data(a,_)-> a
    )list
let firstNum list =
    (function 
        |[] -> -1
        |a::rest -> a
    )list

firstNum [22;4;2;4;2;55]
firstElement list
let emptyList = [1,2 ,3]

let cons = fun element list -> Data(element, list)

type genericList<'anything> =
    |Empty 
    |Data of 'anything * genericList<'anything>

let genericSwap (a , b ) = ( b , a)

//length


// Define the GenericList type


// Define the lastElement function
let List2 = Data("Hello", Data("400", Data("last", Empty)))

// Call the genericListLength function (if defined)
// genericListLength List2

// Call the lastElement function
