//this practicall introduces us to the ability of converting built in functions to total functions

//function to find the last element in a list
let last x  =
    match x with
    |[] -> None
    |_-> Some(List.last x)


List.last [2;3;4]
last [4]

let rec listLast =
    function
    | [x] ->Some x
    | [] ->None
    | _::t -> listLast t

listLast [2;3;4]

let listInit length initializer =
    if length < 0 then
        None
    else
        Some(List.init length initializer)

listInit -1 (fun x -> x + 2)
List.init -1 (fun x -> x + 2)

List.tryLast