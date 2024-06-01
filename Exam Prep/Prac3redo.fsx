// in this practical i will be looking at higher order functions and how to use them

type Tree<'a> =
    |EmptyTree
    |NoChildren of 'a 
    |LeftOnly of 'a * Tree<'a>
    |RightOnly of 'a * Tree<'a>
    |TwoChildren of 'a * Tree<'a> * Tree<'a>

//******************Auxillary functions********************************
let rec filter pfunc =
    function
    |[] -> []
    |a::rest -> if pfunc a then a::filter pfunc rest else filter pfunc rest

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

//tree mapping uising tail recursion and accumalator,Turned out to be more complicated then i thought
//TreeFilter, cant create one because the structure will have floating nodes
// treeInsert
let rec insertTree v =
    function
    | EmptyTree -> NoChildren v
    | NoChildren n -> 
        if n = v then
            NoChildren v
        elif n > v then
            RightOnly (n, NoChildren v)
        else
            LeftOnly (n, NoChildren v)
    | LeftOnly (n, l) -> 
        if v = n then
            LeftOnly (v, l)
        elif v > n then
            TwoChildren (n, l, insertTree v EmptyTree)
        else
            LeftOnly (n, insertTree v l)
    | RightOnly (n, r) -> 
        if n = v then
            RightOnly (v, r)
        elif v < n then
            TwoChildren (n, insertTree v EmptyTree, r)
        else
            RightOnly (n, insertTree v r)
    | TwoChildren (n, l, r) -> 
        if v = n then
            TwoChildren (v, l, r)
        elif v > n then
            TwoChildren (n, l, insertTree v r)
        else
            TwoChildren (n, insertTree v l, r)
            
insertTree 6 test2

//tree NLR
let rec treeNLR =
    function
    |EmptyTree -> []
    |NoChildren x -> [x]
    |LeftOnly (x,l) -> [x] @ treeNLR l
    |RightOnly(x,r) -> [x] @ treeNLR r
    |TwoChildren(x,l,r) -> [x] @ treeNLR l @ treeNLR r

treeNLR test2
//tree LNR
let rec treeLNR =
    function
    |EmptyTree -> []
    |NoChildren x -> [x]
    |LeftOnly(x,l) -> treeLNR l @ [x]
    |RightOnly(x,r)-> [x] @ treeLNR r
    |TwoChildren(x,l,r) -> treeLNR l @ [x] @ treeLNR r
treeLNR test2

//list to tree, takes in a list and creates a tree

let listToTree list =
    let rec makeTree tree =
        function
        | []  -> tree
        | h::t -> makeTree (insertTree h tree) t
    makeTree EmptyTree list

listToTree [15;12]

let treeFilter pfunc tree=
    treeNLR tree |> filter pfunc |> listToTree

