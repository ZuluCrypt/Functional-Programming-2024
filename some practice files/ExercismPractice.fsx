module ValentinesDay

// TODO: please define the 'Approval' discriminated union type
type Approval = 
    |Yes
    |No
    |Maybe
// TODO: please define the 'Cuisine' discriminated union type
type Cuisine =
    |Korean
    |Turkish
// TODO: please define the 'Genre' discriminated union type
type Genre =
    |Crime
    |Horror
    |Romance
    |Thriller
// TODO: please define the 'Activity' discriminated union type

type Activity =
    |BoardGame
    |Chill
    |Movie of Genre
    |Restaurant of Cuisine
    |Walk of int

let rateActivity (activity: Activity): Approval = 
    (function 
        |BoardGame -> No
        |Chill -> No
        |Movie(answer) ->
            if answer = Romance then Yes else No
        |Restaurant(answer) -> 
            if answer = Korean then Yes else Maybe
        |Walk(km) -> 
            if km < 3 then Yes elif km < 5 then Maybe else No
    )activity

rateActivity(Restaurant Turkish)

let successRate (speed: int):float =
    if speed = 0 then
        0.0/100.0
    elif speed = 1 || speed = 2 ||speed = 3|| speed = 4 then 
        100.0/100.0  
    elif speed = 5 ||speed = 6 ||speed = 7 || speed = 8 then
        90.0/100.0
    elif speed = 9 then 
        80.0/100.0
    else 
        77.0/100.0

let productionRatePerHour (speed: int):float =
     (successRate(speed) * 211.0) * float(speed)
let workingItemsPerMinute (speed: int): int =
  int (float (productionRatePerHour(speed) / 60.0))


productionRatePerHour(6)
successRate 6
workingItemsPerMinute 6


let factors number =   
        
    let rec checkPrime output primeCandidate number = 
        if number % primeCandidate = 0 then
            checkPrime (primeCandidate::output) (primeCandidate) (number / primeCandidate)
        elif primeCandidate >= number then    
            output
        else 
            checkPrime (output) (primeCandidate + 1) (number)

    
    let rec reversal output input = 
            (function
                |[] -> output
                |a::rest -> reversal (a::output)rest
            )input
   

    checkPrime [] 2 number |> reversal []

let factors2 number =   
    let rec checkPrime output primeCandidate number = 
        if number % primeCandidate = 0 then
            checkPrime (primeCandidate::output) primeCandidate (number / primeCandidate)
        elif primeCandidate * primeCandidate > number then    
            number::output
        else 
            checkPrime output (primeCandidate + 1) number

    checkPrime [] 2 number

factors 625
 
let rec reversal output input = 
        (function
            |[] -> output
            |a::rest -> reversal (a::output)rest
        )input

reversal [][5;3;2;2]