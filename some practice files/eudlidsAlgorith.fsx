//this is a simple algorithm to finding the factor of a large number
let rec factor a b =   
        if (a % b = 0) then
            b 
        else
            factor b (a % b)
            
factor 32769 77