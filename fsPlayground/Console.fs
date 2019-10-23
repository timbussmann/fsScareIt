module Console

open System
open Board
open Game

let originalForegroundColor = Console.ForegroundColor

let printCard boardslot =
    let animalCharacter animal =
        match animal with
        | Animal.Cat -> 'C'
        | Animal.Dog -> 'D'
        | Animal.Elefant -> 'E'
        | Animal.Mouse -> 'M'
    let mapColor c =
        match c with
        | CardColor.Blue -> ConsoleColor.Cyan
        | CardColor.Orange -> ConsoleColor.Red
        | CardColor.Violet -> ConsoleColor.Magenta
        | CardColor.Yellow -> ConsoleColor.Yellow
    match boardslot with
    | Card(animal, color) -> 
        Console.ForegroundColor <- (mapColor color)
        printf " %c " (animalCharacter animal)
        Console.ForegroundColor <- originalForegroundColor
    | Empty -> printf " - "

let printBoard (board: Boardslot[,]) =
    for i in 0 .. board.GetLength(0) - 1 do
        Array.ForEach<Boardslot>(board.[i, *], fun c -> printCard c)
        printfn ""
    ()

let move sourceX sourceY direction board =
    let d = match direction with
            | "U" -> Direction.Up
            | "D" -> Direction.Down
            | "L" -> Direction.Left
            | "R" -> Direction.Right
            | _ -> raise (InvalidInputException("Invalid direction"))
    let x = 1 + System.Int32.Parse sourceX
    let y = 1 + System.Int32.Parse sourceY
    try
        scare board (y, x) d true
    with
        | InvalidInputException(m) -> printfn "Error! %s" m; board
        | e -> printfn "%s" e.Message; board

[<EntryPoint>]
let main args =
    let mutable board = createBoard
    let player1 = { Animal=Elefant; Color=Violet }
    let player2 = { Animal=Cat; Color=Blue }
    let mutable activePlayer = player1
    while not <| gameFinished board do
        printBoard board
        printfn "active player: %A" activePlayer
        let input = Console.ReadLine().Split()
        match Array.toList input with
        | y :: x :: direction :: xs -> board <- (move x y direction board)
        | "exit" :: _ ->    Console.WriteLine("bye!")
                            exit 0 
        | _ -> Console.WriteLine("type 'help'")
        activePlayer <- if activePlayer = player1 then player2 else player1
        ()
    printfn "game completed"
    printfn "score: %A" (score board [|player1; player2|])
    0