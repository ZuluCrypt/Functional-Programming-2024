// in this practical i will be looking at higher order functions and how to use them

type Tree<'a> =
    |EmptyTree
    |NoChildren of 'a 
    |LeftOnly of 'a * Tree<'a>
    |RightOnly of 'a * Tree<'a>
    |TwoChildren of 'a * Tree<'a> * Tree<'a>

//******************Auxillary functions********************************

//***************************building 1 tree with blocks**********
let leftSubtree = TwoChildren(9,NoChildren 8,NoChildren 9)

let rightSubtree = TwoChildren(15,NoChildren 12,NoChildren 16)

let test2 = TwoChildren(10,leftSubtree,rightSubtree)

//********************************************************

//tree mapping function, direct mapping -> iusing the same data structure

//mapTree
let rec mapTree func =
    function 
    |EmptyTree -> EmptyTree
    |NoChildren n -> NoChildren(func n)
    |LeftOnly(n,l)-> LeftOnly(func n, mapTree func l)
    |RightOnly(n,r) -> RightOnly(func n,mapTree func r)
    |TwoChildren(n,l,r) -> TwoChildren(func n, mapTree func l,mapTree func r )

mapTree (fun x -> x + 2) test2

//tree mapping uising tail recursion and accumalator


