//############################################### calculator types  ################################

type NoteOrItem<'a> = // didnt really use this type
    |Note of 'a
    |Item of 'a

type stack =
    |Empty
    |Element of int * stack

type StackItem =
    |Integer of int // ++
    |Add 
    |Sub
    |Mul
    |Div
    |Mod
    |Negate
    |Delay  //coming soon
    |Copy   //coming soon
    |Zero   //coming soon
    |Paste  //coming soon
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
        |Element(a,Element(b,rest))-> if b = 0 then failwith "div by zero" else push (a/b) rest
        |_-> failwith "not enough values to divide"  
 
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


let rec copy original:stack =
    match original with
    |Empty ->Empty
    |Element(value,rest)-> Element(value,copy rest)

let rec paste (copiedStack :stack) (pasteTo  :stack) :stack =
    match copiedStack with
    |Empty-> pasteTo
    |Element(value, rest)-> paste rest (Element(value,pasteTo))
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
        |Copy -> copy <| stack
        //Paste
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
let stack = evaluate [Integer(311); Integer(55); Integer(45); Sub ; Add] 
let negate = evaluate [Integer(5); Negate]
//##############################################################################################

let stack2 = evaluate [evaluate[integer(311)]]