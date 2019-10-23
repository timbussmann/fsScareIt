// top level module declaration so module code doesn't have to be indented
module Game

open Board;

exception InvalidInputException of string
   
let scare (board: Boardslot[,]) ((y, x): int * int) (direction: Direction) (moveCard: bool) =
    let (ty, tx) = match direction with
                    | Up -> (y-1, x)
                    | Down -> (y+1, x)
                    | Left -> (y, x-1)
                    | Right -> (y, x+1)
    let maxY = board.GetLength(0) - 1
    let maxX = board.GetLength(1) - 1
    if ty < 0 || tx < 0 || ty > maxY || tx > maxX 
        then raise (InvalidInputException("out of bounds"))
    let source = board.[y, x]
    let target = board.[ty, tx]
    match (source, target) with
        | (Card(Elefant,_),Card(Dog,_)) -> ()
        | (Card(Dog,_), Card(Cat,_)) -> ()
        | (Card(Cat,_), Card(Mouse,_)) -> ()
        | (Card(Mouse,_), Card(Elefant,_)) -> ()
        | _ -> raise (InvalidInputException(source.ToString() + "cannot scare" + target.ToString()))
    if moveCard
        then
            board.[ty, tx] <- source
            board.[y, x] <- Empty
        else 
            board.[ty, tx] <- Empty
    compact board

//https://stackoverflow.com/questions/57509105/create-sequence-from-multidimensional-array-in-f
let getAllElements (b: 'a[,]) : (seq<'a>) =
    seq { for x in b do yield (x :?> 'a) }

let score (board: Boardslot[,]) (players: seq<Player>) =
    let getPoints card animal color = match card with
                                        | Empty -> 0
                                        | Card(a, c) -> (if a = animal then 1 else 0) + (if c = color then 1 else 0)
    let calculateScore { Animal = animal; Color = color } = Seq.fold (fun s card -> s + getPoints card animal color) 0 (getAllElements board)
    Seq.map (fun p -> (p, calculateScore p)) players

let hasEmptyColumns (board: Boardslot[,]) : bool =
    let numberOfColumns = board.GetLength(1)
    let columns = Array.zeroCreate numberOfColumns
    for i in 0..(numberOfColumns - 1) do
        let column = board.[*, i]
        columns.[i] <- column
    Seq.exists (fun column -> Seq.exists (fun x -> x <> Empty) column |> not) columns

let canScare cards : bool =
    match cards with
    | (Card(Elefant,_),Card(Dog,_)) -> true
    | (Card(Dog,_), Card(Cat,_)) -> true
    | (Card(Cat,_), Card(Mouse,_)) -> true
    | (Card(Mouse,_), Card(Elefant,_)) -> true
    | _ -> false

let canMove (cardWithNeighbors: (Boardslot * seq<Boardslot>)) : bool =
    Seq.exists (fun neighbor -> canScare (fst cardWithNeighbors, neighbor)) (snd cardWithNeighbors)

let hasNoMoreMoves (board: Boardslot[,]) : bool =
    let cardsWithNeighbors = 
        seq { for y in 0..board.GetLength(0) - 1 do
                for x in 0..board.GetLength(1) - 1 do
                    yield (board.[y, x], getNeighbors y x board) }
    not (Seq.exists (fun card -> canMove card) cardsWithNeighbors)

let gameFinished (board: Boardslot[,]) : bool =
    hasEmptyColumns board || hasNoMoreMoves board
    