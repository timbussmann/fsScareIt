module GameTests

open NUnit.Framework
open Board;
open Game

[<Test>]
let DeckShouldHave32Cards () =
    Assert.AreEqual(32, Board.deck.Length)

[<Test>]
let DeckShouldHaveEachCardTwice () =
    Assert.AreEqual(2, Array.where (fun x -> x = Card(Animal.Dog, CardColor.Blue)) deck |> Array.length)
    Assert.AreEqual(2, Array.where (fun x -> x = Card(Animal.Mouse, CardColor.Orange)) deck |> Array.length)
    Assert.AreEqual(2, Array.where (fun x -> x = Card(Animal.Cat, CardColor.Violet)) deck |> Array.length)
    Assert.AreEqual(2, Array.where (fun x -> x = Card(Animal.Elefant, CardColor.Yellow)) deck |> Array.length)

let BoardShouldBe4By8Matrix () =
    let board = createBoard
    Assert.AreEqual(4, board.GetLength(0))
    Assert.AreEqual(8, board.GetLength(1))

[<Test>]
let ShouldLeaveSourceWhenScaringWithoutMoving () =
    let board = array2D [
        [Card(Mouse, Orange); Card(Cat, Orange)];
        [Card(Mouse, Blue); Card(Cat, Blue)]]
    let result = scare board (0,1) Left false
    Assert.AreEqual(Card(Cat, Orange), result.[0,1])
    Assert.AreEqual(Empty, result.[0,0])

[<Test>]
let ShouldMoveSourceWhenScaringWithMoving () =
    let board = array2D [
        [Card(Mouse, Orange); Card(Cat, Orange)];
        [Card(Mouse, Blue); Card(Cat, Blue)]]
    let result = scare board (0,1) Left true
    Assert.AreEqual(Card(Cat, Orange), result.[0,0])
    Assert.AreEqual(Empty, result.[0,1])

[<Test>]
let ShouldCompactEmptySlots () =
    let board = array2D [
        [ Card(Dog, Orange) ];
        [ Card(Cat, Orange) ];
        [ Card(Mouse, Orange)]];
    let result = scare board (1, 0) Down true
    Assert.AreEqual(array2D [
        [ Empty ];
        [ Card(Dog, Orange)];
        [ Card(Cat, Orange)]
    ], result)

[<Test>]
let ShouldThrowWhenScaringOutsideOfBoard () =
    let board = array2D [
        [ Card(Dog, Violet)]]
    Assert.Throws<InvalidInputException>(fun () -> (scare board (0, 0) Up true |> ignore)) |> ignore

[<Test>]
let ShouldThrowWhenScaringInvalidTarget () =
    let board = array2D [
        [ Card(Cat, Blue); Card(Dog, Blue) ]]
    Assert.Throws<InvalidInputException>(fun() -> (scare board (0,0) Right false) |> ignore) |> ignore

[<Test>]
let ShouldThrowWhenScaringEmptySlot () =
    let board = array2D [
        [ Card(Cat, Blue); Empty ]]
    Assert.Throws<InvalidInputException>(fun() -> (scare board (0,0) Right false) |> ignore) |> ignore

[<Test>]
let ShouldScoreZeroPointsForEmptySlots () =
    let board = array2D [ [ Empty ]]
    let players = [| {Animal = Mouse; Color = Blue} |]
    let result = score board players
    Assert.AreEqual(0, Seq.exactlyOne result |> snd)
    
[<Test>]
let ShouldScoreOnePointForEachMatchingAnimal () =
    let board = array2D [ [ Card(Elefant, Orange) ]]
    let player1 = { Animal = Elefant; Color = Violet }
    let player2 = { Animal = Mouse; Color = Violet }
    let result = score board [| player1;player2 |]
    Assert.AreEqual(1, Seq.item 0 result |> snd)
    Assert.AreEqual(0, Seq.item 1 result |> snd)

[<Test>]
let ShouldScoreOnePointForEachMatchingColor () =
    let board = array2D [ [ Card(Cat, Orange) ]]
    let player1 = { Animal = Elefant; Color = Violet }
    let player2 = { Animal = Mouse; Color = Orange }
    let result = score board [| player1;player2 |]
    Assert.AreEqual(0, Seq.item 0 result |> snd)
    Assert.AreEqual(1, Seq.item 1 result |> snd)

[<Test>]
let ShouldScoreTwoPointsForEachMatchingAnimalAndColor () =
    let board = array2D [ [ Card(Elefant, Orange) ]]
    let player1 = { Animal = Elefant; Color = Orange }
    let player2 = { Animal = Mouse; Color = Violet }
    let result = score board [| player1;player2 |]
    Assert.AreEqual(2, Seq.item 0 result |> snd)
    Assert.AreEqual(0, Seq.item 1 result |> snd)

[<Test>]
let ShouldNotEndGameWhenMovesPossible () =
    let board = array2D [ 
        [ Card(Elefant, Orange) ];
        [ Card(Mouse, Violet) ]]
    Assert.IsFalse(gameFinished board)

[<Test>]
let ShouldEndGameWhenColumnIsEmpty () =
    let board = array2D [ 
        [ Card(Elefant, Orange); Empty ];
        [ Card(Mouse, Violet); Empty ]]
    Assert.IsTrue(gameFinished board)

[<Test>]
let ShouldEndGameWhenNoMoreMovesPossible () =
    let board = array2D [ 
        [ Card(Elefant, Orange); Card(Cat, Blue) ];
        [ Card(Cat, Violet); Card(Elefant, Violet) ]]
    Assert.IsTrue(gameFinished board)