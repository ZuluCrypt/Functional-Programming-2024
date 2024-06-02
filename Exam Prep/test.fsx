
type MyList2<'a>=
    |Empty2
    |Elements of 'a * MyList2<'a>

let push2 v l = Elements(v,l)

let one v = Elements(v,Empty2)

let rec length2 = 
    function
    |Empty2 -> 0
    |Elements(_,rest) -> 1 + length2 rest
let q9 v list = 
    let rec insert i output l =
        match (i,list) with
        |(_,Empty2) -> output
        |(0,Elements(_, rest)) -> insert (-1) (push2 v output) rest
        |(_,Elements(a, rest)) -> insert (i - 1) (push2 a output) rest
    insert ((length2 list)/2) (Empty2) (list)

let list = Elements(1,Elements(2,Empty2))
q9 3 list