module Board

type CardColor = 
    | Blue
    | Orange
    | Violet
    | Yellow

type Animal =
    | Mouse
    | Cat
    | Dog
    | Elefant

type Direction =
    | Up
    | Right
    | Down
    | Left

type Boardslot = 
    | Card of Animal * CardColor
    | Empty

type Player = { Animal: Animal; Color: CardColor }

let deck = 
    // create all possible combinations
    let cards = [| for color in [ Blue; Orange; Violet; Yellow; ] do
                    for animal in [ Mouse; Cat; Dog; Elefant; ] do
                        yield Card(animal, color) |]
    // the deck contains 32 cards, each card twice
    Array.append cards cards

let shuffle (input: seq<Boardslot>) : Boardslot[] =
    // Fisher–Yates shuffle
    let deck = Seq.toArray input //seems to clone
    let random = System.Random()
    for i in List.rev [1 .. deck.Length-1] do
        let randomCard = random.Next (0, i+1)
        let x = deck.[i]
        deck.[i] <- deck.[randomCard]
        deck.[randomCard] <- x
    deck

let createBoard : Boardslot[,] =
    let cards = shuffle deck
    let chunks = Array.chunkBySize 8 cards
    array2D chunks

let compact (board: Boardslot[,]) =
    let collapse y x =
        board.[y, x] <- board.[y-1, x]
        board.[y-1, x] <- Empty
    for y = (board.GetLength(0)-1) downto 1 do
        for x = 0 to (board.GetLength(1)-1) do
            match board.[y, x] with
            | Card (_,_) -> ()
            | Empty -> collapse y x
    board

let getNeighbors (y: int) (x: int) (board: Boardslot[,]) : seq<Boardslot> =
    let get y x = if (y >= 0 && x >= 0 && y < board.GetLength(0) && x < board.GetLength(1)) 
                    then board.[y, x] 
                    else Empty
    let n = [| get (y - 1) x; get (y + 1) x; get y (x - 1); get y (x + 1) |]
    Seq.filter (fun card -> card <> Empty) n

