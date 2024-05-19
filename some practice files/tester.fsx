// code for length function
let rec length n = 
    if n < 0 then
        length (n * -1)
    elif n < 10 then
        1
    else 
        1 + length(n / 10) (* this is where we actually are counting for the function returns one
                            everytime so we recurse threw the digit removing a digit everytime whilst counting 
                            the times we move, uoltimatly finding the lenght *)
//what to do when we want to reverse a  string
let rec multiplier n =
    if n <= 1 then 
        1
    else 
        10 * multiplier(n - 1)
// when dealing with negative inputs we have to return the number at the end only without the sign, so we:
let rec last n =
    if n >= 0 then 
        n % 10 
    else 
        last (n * -1)
last -888
//reverse function
let rec reverse n = 
    if n < 0 then
        -1 * reverse(n * -1) (* efectively if we pipe in a negative number, we want to recall the function with its poisitive opposite. but the (-1 * rever..) allows us to restore the number when the recursion restors*) 
    elif n =  0 then
        0
    else 
        last n * ( n|> (length >> multiplier)) + reverse (n / 10) 

let palindrome  n = if n = n|> reverse then true else false
let palindrome  n = n = reverse n

7/3

let rec numberToHex n = 

    if n < 0 then 
        " "
    elif n % 16 = 0 then  
        "0" + numberToHex (n/16)
    elif n % 16 = 1 then  
        "1" + numberToHex (n/16)
    elif n % 16 = 2 then  
        "2" + numberToHex (n/16)
    elif n % 16 = 3 then  
        "3" + numberToHex (n/16)
    elif n % 16 = 4 then  
        "4" + numberToHex (n/16)
    elif n % 16 = 5 then  
        "5" + numberToHex (n/16)
    elif n % 16 = 6 then  
        "6" + numberToHex (n/16)
    elif n % 16 = 7 then  
        "7" + numberToHex (n/16)
    elif n % 16 = 8 then  
        "8" + numberToHex (n/16)
    elif n % 16 = 9 then  
        "9" + numberToHex (n/16)
    elif n % 16 = 10 then  
        "10" + numberToHex (n/16)
    elif n % 16 = 11 then  
        "A" + numberToHex (n/16)
    elif n % 16 = 12 then  
        "B" + numberToHex (n/16)
    elif n % 16 = 13 then  
        "C" + numberToHex (n/16)
    elif n % 16 = 14 then  
        "D" + numberToHex (n/16)
    elif n % 16 = 15 then  
        "E" + numberToHex (n/16)
    else 
        ""

numberToHex 10



let rec numberToHex n = 
    if n < 0 then 
        " "
    elif n % 16 = 0 then  
        "0" + numberToHex (n/16)
    elif n % 16 = 1 then  
        "1" + numberToHex (n/16)
    elif n % 16 = 2 then  
        "2" + numberToHex (n/16)
    elif n % 16 = 3 then  
        "3" + numberToHex (n/16)
    elif n % 16 = 4 then  
        "4" + numberToHex (n/16)
    elif n % 16 = 5 then  
        "5" + numberToHex (n/16)
    elif n % 16 = 6 then  
        "6" + numberToHex (n/16)
    elif n % 16 = 7 then  
        "7" + numberToHex (n/16)
    elif n % 16 = 8 then  
        "8" + numberToHex (n/16)
    elif n % 16 = 9 then  
        "9" + numberToHex (n/16)
    elif n % 16 = 10 then  
        "A" + numberToHex (n/16)
    elif n % 16 = 11 then  
        "B" + numberToHex (n/16)
    elif n % 16 = 12 then  
        "C" + numberToHex (n/16)
    elif n % 16 = 13 then  
        "D" + numberToHex (n/16)
    elif n % 16 = 14 then  
        "E" + numberToHex (n/16)
    elif n % 16 = 15 then  
        "F" + numberToHex (n/16)
    else 
        ""
numberToHex 2


let rec numberToBin digit = 
    let f x = x / 2
    if digit % 2 = 0 then 
        "0" + numberToBin(f digit)
    elif digit % 2 <> 0 then 
        "1" + numberToBin(f digit)
    else 
        ""

numberToBin 323