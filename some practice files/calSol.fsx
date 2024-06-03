type StackItem
  = Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Modulo
  | Value of int
  | ToggleDelay
  | Copy
  | Paste
  | Zero

type Mode
  = Immediate
  | Delayed

let immediateEval copied =
  function
  | Add::Value x::Value y::rest -> 
    (Value (x + y)::rest, copied, None)
  | Subtract::Value x::Value y::rest -> 
    (Value (x - y)::rest, copied, None)
  | Multiply::Value x::Value y::rest ->
    (Value (x * y)::rest, copied, None)
  | Divide::Value x::Value y::rest ->
    if y = 0 then
      (Value x::Value y::rest, copied, Some "HEY! No division by zero! Naughty!")
    else
      (Value (x / y)::rest, copied, None)
  | Negate::Value x::rest ->
    (Value (-x)::rest, copied, None)
  | Modulo::Value x::Value y::rest ->
    if y = 0 then
      (Value x::Value y::rest, copied, Some "HEY! No division by zero! Naughty!")
    else
      (Value (x % y)::rest, copied, None)
  | Zero::_ ->
    ([], copied, None)
  | Copy::rest ->
    (rest, rest, None)
  | Paste::rest ->
    (copied @ rest, copied, None)
  | op::x -> (x, copied, Some $"HEY! What do you mean by {op}?")
  | [] -> ([], copied, None)

let eval (stack, mode, copied) =
  function
  | ToggleDelay ->
    (stack, (if mode = Immediate then Delayed else Immediate), copied)
  | otherInstruction ->
    if mode = Immediate then
      let (evaluated, newCopyStack, issue) = immediateEval copied (otherInstruction::stack)
      (evaluated, Immediate, newCopyStack)
    else
      (otherInstruction::stack, Delayed, copied)

eval ([], Immediate, []) Add
eval ([Value 1; Value 2], Immediate, []) Add
eval ([Value 1; Value 2], Immediate, []) Copy
eval ([Value 1; Value 2], Immediate, [Value 1; Value 2]) Paste