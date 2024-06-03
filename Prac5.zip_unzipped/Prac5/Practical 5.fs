(*
    Author : Thandokuhle Zulu
    Title  : Designing a program using domain-specific language
    Date   : 19/03/2024

    Quick discription : 

        in this practical, we will:
        ● create our first “real” functional program, which can be run from the
        command-line;

        ● design a fully-working puzzle game

*)
//Problem domain is creating a soduku game -> 

//Step 1: create a data structure to represent the game

type Digit=
    |One
    |Two
    |Three
    |Four
    |Five
    |Six
    |Seven
    |Eight
    |Nine
    
(*When you are solving Sudoku, it is very common to write 
down all the numbers that a block could be, hence why we would create a type
to represent all those possibilities*)

type Cell =
    |EmptyCell
    |FixedValue of Digit
    |Definitely of Digit
    |MaybeTwo   of Digit * Digit
    |MaybeThree of Digit * Digit * Digit
    |MaybeFour  of Digit * Digit * Digit * Digit 
    |MaybeFive  of Digit * Digit * Digit * Digit * Digit 
    |MaybeSix   of Digit * Digit * Digit * Digit * Digit * Digit 
    |MaybeSeven of Digit * Digit * Digit * Digit * Digit * Digit * Digit 
    |MaybeEight of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit 
    |MaybeNine  of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit 

// we will create a type to represent a cluster of 9 cells

type Cluster =
    |Cluster of Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell
//This can represent any grouping of nine cells , either a row, column or block

type GameView = 
    |Blocks  of  Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    |Rows    of  Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    |Columns of  Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster

//Step 2: create a data structure to represent the possible operations of the game

type UserAction =
    |EnterDigit of Digit
    |Right
    |Left 
    |Down
    |Up
    |Quit 
    |StartGame

//a structure to just keep track of user movements
type Game =
    |SettingUp of (int * int) * GameView //colum and row = (int * int)
    |Solving   of (int * int) * GameView
    |Solved    of GameView

//Step 3: combine step 1 -> step 2 -> Step 3

 //creating a shallow language (tool / function) to extract the cel in a particular poistiona nd binding it to its appropriate name/positional name
let first (Cluster(x,_,_,_,_,_,_,_,_))= x
let second (Cluster(_,x,_,_,_,_,_,_,_))= x
let third (Cluster(_,_,x,_,_,_,_,_,_))= x
let fourth (Cluster(_,_,_,x,_,_,_,_,_))= x
let fifth  (Cluster(_,_,_,_,x,_,_,_,_))= x
let sixth (Cluster(_,_,_,_,_,x,_,_,_))= x
let seventh (Cluster(_,_,_,_,_,_,x,_,_))= x
let eighth (Cluster(_,_,_,_,_,_,_,x,_))= x
let ninth (Cluster(_,_,_,_,_,_,_,_,x))= x
let mapNine fn (a,b,c,d,e,f,g,h,i)=
        (fn a, fn b, fn c, fn d, fn e, fn f,fn g ,fn h, fn i)
        
let straightenBlock x y z f0 f1 f2 =
        Cluster (f0 x, f1 x, f2 x, 
                f0 y,f1 y, f2 y,
                f0 z,f1 z,f2 z)

let BlockBlock x y z f0 f1 f2 =
        Cluster (f0 x, f0 y, f0 z, 
                f1 x,f1 y, f1 z,
                f2 x,f2 y,f2 z)               
let viewAsRows board =
    match board with
            |Rows _ -> board
            |Blocks (a,b,c,d,e,f,g,h,i) ->
                

                Rows(   straightenBlock a b c first second third,
                        straightenBlock a b c fourth fifth sixth,
                        straightenBlock a b c seventh eighth ninth,
                        straightenBlock d e f first second third,
                        straightenBlock d e f fourth fifth sixth,
                        straightenBlock d e f seventh eighth ninth,
                        straightenBlock g h i first second third,
                        straightenBlock g h i fourth fifth sixth,
                        straightenBlock g h i seventh eighth ninth
                    )
                // Rows (Cluster(first a, second a, third a, first b, second b, third b, first c , second c, third c),
                //       Cluster(fourth a, fifth a , sixth a ,fourth b , fifth b , sixth b, fourth c , fifth c , sixth c),
                //       Cluster(seventh a , eighth a, ninth a, seventh b, eighth b, ninth b, seventh c, eighth c, ninth c),
                //       Cluster(first d, second d, third d, first e, second e, third e, first f , second f, third f),
                //       Cluster(fourth d, fifth d , sixth d ,fourth e , fifth e , sixth e, fourth f , fifth f , sixth f),
                //       Cluster(seventh d , eighth d, ninth d, seventh e, eighth e, ninth e, seventh f, eighth f, ninth f),
                //       Cluster(first g, second g, third g, first h, second h, third h, first i , second i, third i),
                //       Cluster(fourth g, fifth g , sixth g ,fourth h , fifth h , sixth h, fourth i , fifth i , sixth i),
                //       Cluster(seventh g , eighth g, ninth g, seventh h, eighth h, ninth h, seventh i, eighth i, ninth i)
                //     )
            |Columns(a,b,c,d,e,f,g,h,i)->
                
                Rows(   Cluster <| mapNine first (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine second (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine third (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i),
                        Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
                    )
//testBoard to let us test our functions
let testBoard =
    let newCluster =
        Cluster (Definitely One, Definitely Two, Definitely Three, Definitely Four, Definitely Five,Definitely Six,Definitely Seven,Definitely Eight, Definitely Nine)
    
    Blocks( newCluster,newCluster,newCluster
           ,newCluster,newCluster,newCluster
           ,newCluster,newCluster,newCluster
          )
let newBoard =
    let emptyCluster =
        Cluster (EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell)
    
    Blocks(emptyCluster,emptyCluster,emptyCluster,
           emptyCluster,emptyCluster,emptyCluster,
           emptyCluster,emptyCluster,emptyCluster
          )
// Define fixed values for the clusters

// Create a board with some fixed values

    

let viewAsColumns board =
    match board with
        |Columns _ -> board
        |Blocks (a,b,c,d,e,f,g,h,i)->
            Columns (straightenBlock a d g first fourth seventh,
                    straightenBlock a d g second fifth eighth,
                    straightenBlock a d g third sixth ninth,
                    straightenBlock b e h first fourth seventh,
                    straightenBlock b e h second fifth eighth,
                    straightenBlock b e h third sixth ninth,
                    straightenBlock c f i first fourth seventh,
                    straightenBlock c f i second fifth eighth,
                    straightenBlock c f i third sixth ninth
                    )
        |Rows(a,b,c,d,e,f,g,h,i)->
            Columns (Cluster <| mapNine first (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine second (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine third (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i),
                    Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
                    )
let fullTestBoard =
    let C1 = Cluster (Definitely Eight ,Definitely Two ,Definitely Four ,Definitely Nine, Definitely Five ,Definitely Three ,Definitely Six,Definitely Seven ,Definitely One) 
    let C2 = Cluster (FixedValue Six,Definitely Three ,Definitely Five ,FixedValue Eight ,Definitely One ,Definitely Seven ,Definitely Nine ,FixedValue Two,Definitely Four)
    let C3 =Cluster (Definitely Seven ,Definitely One,FixedValue Nine ,Definitely Six ,Definitely Two ,Definitely Four ,FixedValue Eight ,Definitely Five ,Definitely Three)
    let C4  =Cluster (Definitely Five ,Definitely Eight ,FixedValue Seven ,Definitely Two ,Definitely Nine ,Definitely One ,Definitely Three ,Definitely Four ,Definitely Six) 
    let C5  =Cluster (Definitely One ,Definitely Four ,Definitely Two ,Definitely Seven ,FixedValue Three ,Definitely Six ,Definitely Five ,Definitely Eight ,Definitely Nine) 
    let C6 =Cluster (Definitely Three ,FixedValue Nine ,Definitely Six ,Definitely Four ,Definitely Eight ,FixedValue Five ,Definitely Two ,Definitely One ,Definitely Seven) 
    let C7 =  Cluster (Definitely Two ,Definitely Six ,Definitely One ,Definitely Five ,Definitely Four ,FixedValue Nine ,Definitely Seven ,Definitely Three ,Definitely Eight) 
    let C8 =  Cluster (Definitely Four ,Definitely Seven ,Definitely Eight ,FixedValue Three ,Definitely Six ,Definitely Two ,Definitely One ,FixedValue Nine ,Definitely Five) 
    let C9 =  Cluster (EmptyCell,Definitely Five ,Definitely Three ,Definitely One ,Definitely Seven ,FixedValue Eight ,Definitely Four ,Definitely Six ,Definitely Two )
    
    let row1 = Rows( C1,C2,C3
        ,C4,C5,C6
        ,C7,C8,C9
        )
    viewAsColumns row1
    
 
let viewAsBlocks board = 
    match board with    
    | Blocks _ -> board // If already in Blocks format, return as is 
    | Rows _ -> board
    |Columns (a,b,c,d,e,f,g,h,i)->
                        Blocks(
                              BlockBlock a b c first second third,
                              BlockBlock d e f first second third,
                              BlockBlock g h i first second third,
                              BlockBlock a b c fourth fifth sixth,
                              BlockBlock d e f fourth fifth sixth,
                              BlockBlock g h i fourth fifth sixth,
                              BlockBlock a b c seventh eighth ninth,
                              BlockBlock d e f seventh eighth ninth,
                              BlockBlock g h i seventh eighth ninth
                              )

// few test
viewAsRows fullTestBoard |> ignore// remove ignore to test 
viewAsColumns fullTestBoard |> ignore// remove ignore to test 
viewAsBlocks fullTestBoard |> ignore // remove ignore to test               
        

//function to create cell B based on cell A
let addLeft digit cell =
    match cell with 
        |FixedValue a -> cell
        |EmptyCell -> Definitely digit
        |Definitely a -> MaybeTwo (digit , a)
        |MaybeTwo (a, b) -> MaybeThree (digit, a, b)
        |MaybeThree(a,b,c) -> MaybeFour (digit ,a,b,c)
        |MaybeFour(a,b,c,d) ->MaybeFive(digit,a,b,c,d)
        |MaybeFive (a,b,c,d,e)->MaybeSix(digit,a,b,c,d,e)
        |MaybeSix (a,b,c,d,e,f)-> MaybeSeven(digit,a,b,c,d,e,f)
        |MaybeSeven(a,b,c,d,e,f,g)-> MaybeEight(digit ,a,b,c,d,e,f,g)
        |MaybeEight(a,b,c,d,e,f,g,h) -> MaybeNine(digit,a,b,c,d,e,f,g,h)
        |MaybeNine _ -> cell
//function to remove left most digit
let trimLeft cell =
    match cell with 
        |EmptyCell -> cell
        |FixedValue _ -> cell
        |Definitely _ -> EmptyCell
        |MaybeTwo   (_,a) -> Definitely a
        |MaybeThree (_,a,b)-> MaybeTwo (a,b)
        |MaybeFour  (_,a,b,c)-> MaybeThree (a,b,c)
        |MaybeFive  (_,a,b,c,d)-> MaybeFour (a,b,c,d)
        |MaybeSix   (_,a,b,c,d,e)-> MaybeFive (a,b,c,d,e)
        |MaybeSeven (_,a,b,c,d,e,f)-> MaybeSix (a,b,c,d,e,f)
        |MaybeEight (_,a,b,c,d,e,f,g)-> MaybeSeven (a,b,c,d,e,f,g)
        |MaybeNine  (_,a,b,c,d,e,f,g,h)-> MaybeEight (a,b,c,d,e,f,g,h)

//function to allow us to see the left most digit
let peek cell =
    match cell with
        |EmptyCell -> None
        |FixedValue a -> Some a
        |Definitely a -> Some a
        |MaybeTwo   (a,_)-> Some a
        |MaybeThree (a,_,_)-> Some a
        |MaybeFour  (a,_,_,_)-> Some a
        |MaybeFive  (a,_,_,_,_)-> Some a
        |MaybeSix   (a,_,_,_,_,_)-> Some a
        |MaybeSeven (a,_,_,_,_,_,_)-> Some a
        |MaybeEight (a,_,_,_,_,_,_,_)-> Some a
        |MaybeNine  (a,_,_,_,_,_,_,_,_)-> Some a
//function to check if a digit exists in a cell
//some auxillary total functions
let OptionMap f x=
    function
    |None -> None
    |Some x -> f x
let bind f=
    function
    |None -> None
    |Some a ->  f a
let defaultValue dVal=
    function
    |None -> dVal
    |Some a -> a
//------
let exists x cell =
    let rec realExists remaining =
        peek remaining 
        |> Option.map (fun v -> v = x || realExists (trimLeft remaining))
        |> defaultValue false
        //check if a specific digit already exists in the cell
    (function
        |FixedValue v -> v = x // if some fix value v then  check if its the same value
        |_ -> realExists cell//else check if does this value exist in that cell
    )cell

//insert , adds a digit in the correct position 
let insert x cell =
    let rec doInsert remaining =
        peek remaining
        |> Option.map (fun item -> if item < x then 
                                                        addLeft item (doInsert (trimLeft remaining))
                                                    elif item = x then
                                                        remaining
                                                    else    
                                                        addLeft x remaining
                                )|> defaultValue (Definitely x)

    (function
        |FixedValue _ -> cell
        |_-> doInsert cell
    )cell

//remove
let remove digit cell=
    let rec removeMatch remaining =
        peek remaining 
        |> Option.map (fun item -> if item = digit then
                                                        trimLeft remaining
                                                    else
                                                        addLeft item (removeMatch (trimLeft remaining))
                                            
        )|> defaultValue cell
    
    (function
        |FixedValue _ -> cell
        |_ -> removeMatch cell
    )cell

//toggle to remove number if repeated
let toggle digit cell=
    if exists digit cell then   
        remove digit cell
    else
        insert digit cell

let rec withCell board col row func =
    let withCluster (Cluster (a,b,c,d,e,f,g,h,i)) =
        if row = 0 then Cluster (func a,b,c,d,e,f,g,h,i)
        elif row = 1 then Cluster (a, func b ,c,d,e,f,g,h,i)
        elif row = 2 then Cluster (a,b,func c,d,e,f,g,h,i)
        elif row = 3 then Cluster (a,b,c,func d,e,f,g,h,i)
        elif row = 4 then Cluster (a,b,c,d,func e,f,g,h,i)
        elif row = 5 then Cluster (a,b,c,d,e,func f,g,h,i)
        elif row = 6 then Cluster (a,b,c,d,e,f,func g,h,i)
        elif row = 7 then Cluster (a,b,c,d,e,f,g,func h,i)
        elif row = 8 then Cluster (a,b,c,d,e,f,g,h,func i)
        else Cluster (a,b,c,d,e,f,g,h,i)

    (function//if rows or blocks found we first view as columns
        |Rows _ -> withCell (viewAsColumns board) col row func
        |Blocks _ -> withCell (viewAsColumns board) col row func
        |Columns (a,b,c,d,e,f,g,h,i) -> 
            if col = 0 then   Columns(withCluster a,b,c,d,e,f,g,h,i)
            elif col = 1 then Columns(a,withCluster b,c,d,e,f,g,h,i)
            elif col = 2 then Columns(a,b,withCluster c,d,e,f,g,h,i)
            elif col = 3 then Columns(a,b,c,withCluster d,e,f,g,h,i)
            elif col = 4 then Columns(a,b,c,d,withCluster e,f,g,h,i)
            elif col = 5 then Columns(a,b,c,d,e,withCluster f,g,h,i)
            elif col = 6 then Columns(a,b,c,d,e,f,withCluster g,h,i)
            elif col = 7 then Columns(a,b,c,d,e,f,g,withCluster h,i)
            elif col = 8 then Columns(a,b,c,d,e,f,g,h,withCluster i)
            else board
    )board

//evaluate function with helper functions
let handleDirection (col , row) board  action =
    let min a b = if a < b then a else b
    let max a b = if a > b then a else b
    (function
        |Left ->  ( (max 0 (col - 1), row) , board)
        |Right -> ( (min 8 (col + 1), row) , board)
        |Up ->    ( (col , max 0 (row - 1)), board)
        |Down ->  ( (col , min 8 (row + 1)), board)
        |_ ->     ( (col , row) , board)
    )action


let fixDigit n col row board =
    withCell board col row (fun _ -> FixedValue n)

let considerDigit n col row board =
    withCell board col row (fun c -> toggle n c)


// helper functions to solve the game

    
let clusterFold fn start (a,b,c,d,e,f,g,h,i) =
    (fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h (fn i start)))))))))

let GameViewToCluster board = // deconstruction from type gameview to type cluste * cluster ....
    match board with
    | Rows (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)
    | Columns (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)
    | Blocks (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)

let ConfirmClusters (Cluster (a,b,c,d,e,f,g,h,i)) =
    let containOneDigitvalue = // check if does the cell contains one digit
        function
        | Definitely _ -> true
        | FixedValue _ -> true
        | _ -> false

    let updateCounts  (a,b,c,d,e,f,g,h,i) num =
         // Convert number to zero-based index
        if num = Some One then  (a+1,b,c,d,e,f,g,h,i)
        elif num = Some Two then  (a,b+1,c,d,e,f,g,h,i)
        elif num = Some Three then  (a,b,c+1,d,e,f,g,h,i)
        elif num = Some Four then  (a,b,c,d+1,e,f,g,h,i)
        elif num = Some Five then  (a,b,c,d,e+1,f,g,h,i)
        elif num = Some Six then  (a,b,c,d,e,f+1,g,h,i)
        elif num = Some Seven then  (a,b,c,d,e,f,g+1,h,i)
        elif num = Some Eight then  (a,b,c,d,e,f,g,h+1,i)
        elif num = Some Nine then  (a,b,c,d,e,f,g,h,i+1)
        else  (a,b,c,d,e,f,g,h,i) // Default case, return unchanged tuple
    
    clusterFold (fun item (definiteCells, counts) -> (definiteCells && containOneDigitvalue item, updateCounts counts ( peek item )) )(true, (0,0,0,0,0,0,0,0,0)) (a,b,c,d,e,f,g,h,i) 
    = (true, (1,1,1,1,1,1,1,1,1))

let reviewState board =
    let confirmAll = GameViewToCluster board |> clusterFold (fun item state -> if ConfirmClusters item then state + 1 else state) 0  
    if confirmAll = 9 then 
        true 
    else 
        false
let confirmBlocks board = reviewState <|viewAsBlocks board // checking for duplicates in viewAsBlocks mode
let confirmRows board = reviewState <|viewAsRows board// checking for duplicates in viewAsRows mode
let confirmColumnsa board =reviewState <|viewAsColumns board // checking for duplicates in viewAsColumns mode   



let rec evaluate io game =
    let UserAction = io game // impure function , should only be called once

    let g =
        (function
            |(StartGame , SettingUp (position, board)) -> 
                Solving (position , board)// if w e "Start Gmae " but still setting up then we solve the game

            |(StartGame , Solved _) -> 
                SettingUp ((0 , 0), newBoard) // if we told to "Start the game" but it is solve we set up a new board

            |(_,Solved _) -> 
                 game//if we told anything else but the game is solved then we dont do anythingh
            |(EnterDigit n, SettingUp((col,row), board))-> 
                SettingUp((col,row),considerDigit n col row board)//if we given a digit to enter and wwe still setting up the we should "fix " the digit as part of the puzzle

            | (EnterDigit n, Solving ((col, row), board)) ->
                if (confirmBlocks board && confirmRows board && confirmColumnsa board ) then Solved board else Solving ((col, row), considerDigit n col row board)//if a digit is entered and we still solving the puzzle then we toggle it

            |(direction , SettingUp (position, board)) ->
                SettingUp (handleDirection position board direction)

            |(direction , Solving (position,board)) ->
                if (confirmBlocks board && confirmRows board && confirmColumnsa board ) then Solved board else Solving (handleDirection position board direction) //
        )(UserAction,game)
    if UserAction = Quit then
        ()
    else 
        evaluate io g


// and now, the least exciting part of the whole thing.

let printAt column row digit =
    System.Console.CursorLeft <- column
    System.Console.CursorTop <- row
    (function
    | None -> ()
    | Some One -> printf "1"
    | Some Two -> printf "2"
    | Some Three -> printf "3"
    | Some Four -> printf "4"
    | Some Five -> printf "5"
    | Some Six -> printf "6"
    | Some Seven -> printf "7"
    | Some Eight -> printf "8"
    | Some Nine -> printf "9"
    ) digit

let printBlock cell col row =
    (function
    | Definitely v ->
        printAt (col + 2) (row + 1) (Some v)
    | FixedValue v ->
        let previous = System.Console.ForegroundColor
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printAt (col + 2) (row + 1) (Some v)
        System.Console.ForegroundColor <- previous
    | _ ->
        let rec printDigits count remaining =
            printAt (col + (count % 3) * 2) (row + (count / 3)) (peek remaining)
            (function
            | EmptyCell ->
                ()
            | Definitely _ ->
                ()
            | _ ->
                printDigits (count+1) (trimLeft remaining)
            ) remaining
        printDigits 0 cell
    ) cell

let printRow (Cluster (a,b,c,d,e,f,g,h,i)) rowStart =
    printBlock a 2 rowStart
    printBlock b 10 rowStart
    printBlock c 18 rowStart
    printBlock d 26 rowStart
    printBlock e 34 rowStart
    printBlock f 42 rowStart
    printBlock g 50 rowStart
    printBlock h 58 rowStart
    printBlock i 66 rowStart

let printBoard board message =
    System.Console.Clear () // 4, 12, 20
    printfn $"""
┏━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┓
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┗━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┛
{message}
"""
    let (Rows (a,b,c,d,e,f,g,h,i)) = viewAsRows board
    printRow a 1
    printRow b 5
    printRow c 9
    printRow d 13
    printRow e 17
    printRow f 21
    printRow g 25
    printRow h 29
    printRow i 33

let showCursor (boardColumn, boardRow) =
    System.Console.CursorVisible <- true
    //System.Console.CursorSize <- 100
    System.Console.CursorLeft <- 4 + 8 * boardColumn
    System.Console.CursorTop <- 2 + 4 * boardRow

let output game =
    // this entire function ... is a side-effect!!
    (function
    | Solved board ->
        System.Console.ForegroundColor <- System.ConsoleColor.Green
        printBoard board "Congratulations!  The puzzle is solved.  Press <Enter> to set up a new one."
        System.Console.ResetColor ()
    | SettingUp (position, board) ->
        printBoard board """Use  ← → ↑ ↓  keys to move around.  Type a digit to set up the puzzle.
Press <Enter> to begin solving, and <Q> to quit."""
        showCursor position
    | Solving (position, board) ->
        printBoard board """Use  ← → ↑ ↓  keys to move around.  Type a digit to add or remove it from consideration.
Press <Q> to quit."""
        showCursor position
    ) game
    game

let rec input game =
    let read = System.Console.ReadKey true
    if read.Key = System.ConsoleKey.Enter then
        StartGame
    elif read.Key = System.ConsoleKey.LeftArrow then
        Left
    elif read.Key = System.ConsoleKey.RightArrow then
        Right
    elif read.Key = System.ConsoleKey.UpArrow then
        Up
    elif read.Key = System.ConsoleKey.DownArrow then
        Down
    elif read.KeyChar = '1' then
        EnterDigit One
    elif read.KeyChar = '2' then
        EnterDigit Two
    elif read.KeyChar = '3' then
        EnterDigit Three
    elif read.KeyChar = '4' then
        EnterDigit Four
    elif read.KeyChar = '5' then
        EnterDigit Five
    elif read.KeyChar = '6' then
        EnterDigit Six
    elif read.KeyChar = '7' then
        EnterDigit Seven
    elif read.KeyChar = '8' then
        EnterDigit Eight
    elif read.KeyChar = '9' then
        EnterDigit Nine
    elif read.Key = System.ConsoleKey.Q then
        Quit
    else
        (output >> input) game

[<EntryPoint>]
let main _ =
    evaluate (output >> input) (SettingUp ((0,0),  fullTestBoard))
    0



