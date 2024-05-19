//############################################### calculator types  ################################

type NoteOrItem<'a> = // didnt really use this type
    |Note of 'a
    |Item of 'a

type stack =
    |Empty
    |Element of int * stack

type StackItem =
    |Integer of int // ++
    |Add //++
    |Sub//++
    |Mul//++
    |Div//++
    |Mod
    |Negate//++
    |Delay
    |Copy
    |Zero
    |Paste
//##################################################################################################
// ++ == new functionss


//##################################################    calc methods   ###############################
let push value remaining = Element(value,remaining)
//add
let AddValues stack =
    (function
        |Element(a,Element(b,rest)) -> push (a+b) rest
        |_ -> failwith "not enough values to add"
    )stack
//sub
let SubValues stack=
    (function   
        |Element(a,Element(b,rest )) -> push (a-b) rest
        |_-> failwith "not enough values to subtract"
    )stack
//div
let DivValues stack =
    (function
        |Element(a,Element(b,rest))-> if b = 0 then None else Some (push (a/b) rest)
        |_-> None  
    )stack
//multiply
let MulValues stack =
    (function
        |Element(a,Element(b,rest))->  push (a*b) rest
        |_-> failwith "not enough values to Multiply"  
    )stack
//mod
let ModValues stack =
    (function 
        |Element(a,Element(b,rest))-> push (a % b) rest 
        |_-> failwith "not enough values to Multiply" 
    )stack
//negate
let NegateValue stack =
    (function
        |Element(a,rest)-> push (a * -1) rest
        |_-> failwith "Missing value"
    )stack


//copy  (coming soon)
//paste  (coming soon)
//zero  (coming soon)
//delay  (coming soon)
//############################################################################################





//###########################################Control##########################################
//matches the operations
let operate (stack:stack) (item:StackItem) =
    (function
        |Integer(value) -> push value stack //if we find int push to stack
        |Add -> AddValues <| stack
        |Sub -> SubValues <| stack
        |Mul -> MulValues <| stack
        |Mod -> ModValues <| stack
        |Div -> DivValues <| stack
        |Negate -> NegateValue <| stack
        )item

//Evaluate function
let evaluate items = 
    let rec evalstack stack items =
        (function
            |[] -> stack
            |item::rest -> 
                let newStack = operate stack item
                evalstack newStack rest     
        )items
    evalstack Empty items
//###############################################################################################


 
// ########################################  Some tests #########################################
let stack = evaluate [Integer(5); Integer(0); Div] 
let negate = evaluate [Integer(5); Negate]
//##############################################################################################