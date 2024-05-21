// this is a file containing practice problems for the exam

//max
let max a b =
     if a > b then a else b
    
max 1 2

//abs
let abs a = 
    if a < 0 then a * -1 else a
abs -2
abs 1

//if less then 3 return true
(fun a -> if a < 3 then true else false )<| 2


let GCD a b = 

    let rec findGCD num1 num2=
        if num2 = 0 then
            num1
        else  
            let rem = num1 % num2
            findGCD (num2)(rem)

    if a >= b || b = 0 then 
        a
    elif a < 0 then 
        findGCD (a * -1) b
    elif b < 0 then 
        findGCD a (b * -1)
    else 
        findGCD a b

GCD 25 5

let power digit exponent =
    //inner power function
    let rec findPow b e acc =
        match (b,e) with
            |(a,0) -> acc * a
            |(a,e) -> findPow a (e - 1) (a * a)
    findPow digit exponent 0

power 2 3

let SumEvenandOdd n f =
    let rec checker n evenSum oddSum =
        match n with
            |0 -> (evenSum,oddSum)
            |_ -> if n % 2 = 0 then 
                    checker (n - 1) (f n evenSum) oddSum
                  else 
                    checker (n - 1) evenSum (f n oddSum)  
    checker n 0 0


