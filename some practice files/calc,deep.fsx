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
//two modes the stack can be in
type Mode = 
    |Immediate 
    |Delayed

let getStack (s,_) = s
let getMode (_,m) = m
let ImmediateEval =
   function
        |Add::Integer x::Integer y::rest -> Integer(x + y)::rest 
        |Sub::Integer x::Integer y::rest -> Integer(x - y)::rest 
        |Mul::Integer x::Integer y::rest -> Integer(x * y)::rest 
        |Div::Integer x::Integer y::rest -> Integer(x / y)::rest 
        |Negate::Integer x::rest -> Integer(-x)::rest 
        |Mod::Integer x::Integer y::rest -> Integer(x + y)::rest 
        |Zero::_-> []
        |x -> x

let DelayedEval () =
    function
        |Add::Integer x::Integer y::rest -> Integer(x + y)::rest 
        |Sub::Integer x::Integer y::rest -> Integer(x - y)::rest 
        |Mul::Integer x::Integer y::rest -> Integer(x * y)::rest 
        |Div::Integer x::Integer y::rest -> Integer(x / y)::rest 
        |Negate::Integer x::rest -> Integer(-x)::rest 
        |Mod::Integer x::Integer y::rest -> Integer(x + y)::rest 
        |Zero::_-> []
        |x -> x

// ++ == new functionss
let eval (stack, mode) =
  function
  | Delay ->(stack,Delayed)
  | otherInstruction ->
                if mode = Immediate then
                    (ImmediateEval (otherInstruction::stack), Immediate)
                else
                    (otherInstruction::stack, Delayed)
    
        
let stack =  [Integer(311); Integer(55); Integer(45); Sub ; Add] 

eval (stack, Immediate)