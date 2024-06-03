//Section D : Design
//designing a calculator

//stack
// type Stack<'a> =
//     |EmptyStack
//     |Elements of 'a * Stack<'a>
//stack proved to be hard to implement and match certain types so ill use a list instead
//Operations
type Opperation =
    |NewStack
    |InputNum of int
    |InputSym of string
    |Zero
    |Negate
    |Add
    |Subtract
    |Multiply
    |Divide
    |Modulo
    |Copy
    |ToggleDelay
    |Paste
    |Show
    |InputNum of int
    

//Integers

type Result<'a,'b> =
    |Error of 'b
    |Result of 'a


let rec Eval  =
    let output = []
    function
    |[] -> []
    |NewStack::_ -> []
    |InputNum x::rest -> InputNum x::rest
    //|InputSym xy -> xy::stack
    |Zero::rest -> []
    |Negate::InputNum(x)::rest -> Eval(InputNum(x * -1)::rest)
    |Add::InputNum x::InputNum y::rest -> Eval(InputNum(x + y)::rest)
    |Subtract::InputNum x::InputNum y::rest -> Eval(InputNum(x - y)::rest)
    |Multiply::InputNum x::InputNum y::rest -> InputNum(x * y)::rest
    |Divide::InputNum x::InputNum y::rest -> InputNum(x / y)::rest
    |Modulo::InputNum x::InputNum y::rest -> InputNum(x%y)::rest
    |Copy::rest-> rest @ output 
    |Paste::rest -> output @ rest
    |Show::a::_ -> [a]  

Eval [Subtract;Add;InputNum 1;InputNum 2;InputNum 2]
// Eval [NewStack]
// Eval [Subtract;1;2]
// Eval [Negate;1]
// Eval [Multiply ;2;3]
// Eval [Divide 4;2]
// Eval 