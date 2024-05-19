//Problem x
type NoteOrItem<'data> = 
    | Sorry
    | Item of 'data

let divide digit divisor =
    if divisor = 0 then 
        Error "cant divide by zero"
    else 
        Ok (digit / divisor)

divide 10  2

//problem xii
let rec foldl s_func output list =
    (function 
        | [] -> output
        | a::remaining -> foldl s_func (s_func output a) remaining
    )list


foldl (fun acc x -> x::acc) [] [1; 2; 3; 4; 5]
// Expected output: [5; 4; 3; 2; 1]

let concatenatedString = foldl (fun acc x -> acc + x) "" ["Hello"; " "; "world"; "!"]

let rec foldR f output list =
    (function
        |[]-> output
        |a::remaining -> f a (foldR f output remaining)
    )list

//foldR(fun acc x -> x::acc) [] [1; 2; 3; 4; 5]
// Expected output: [5; 4; 3; 2; 1]

//let concatenatedString = foldR (fun acc x -> acc + x) "" ["Hello"; " "; "world"; "!"]
foldl (fun state a -> a::state) [] [1;2;3;4;5]
foldR (fun state a -> state::a) [] [1;2;3;4;5]