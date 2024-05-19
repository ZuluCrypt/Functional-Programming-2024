(fun d -> (d 5) + 8) >> (fun a -> a - 5)

(fun a -> printf"hello") 2

type Result<'a,'b> =
    |Ok of 'a
    |Error of 'b
let q7 (t:Result<'a,'b>) : Option<'a>=
    match t with
    |Ok a -> Some a
    |Error _ -> None

q7 (Ok "hi")

let result :Option<int> = q7 (Error 600)