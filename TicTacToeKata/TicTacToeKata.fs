namespace TicTacToeKata

(*

X always goes first.
Players alternate placing Xs and Os on the board
Players cannot play on previously played position
Player wins when:
    One player has three in a row, horizontally, vertically or diagonally
    All nine squares are filled.
    If a player is able to draw three Xs or three Os in a row, that player wins.
If all nine squares are filled and neither player has three in a row, the game is a draw.

*)

    module TicTacToe =

        type Player = X | O

        type Axis = One | Two | Three

        type PlayOutcome = {result: bool; player: Player}

        type Location = {x: Axis; y: Axis}

        type Movement = {player: Player; location: Location}
        type Movements = Movement list

        let moveExists movements movement =
            movements |> List.exists (fun m -> m.location = movement.location )
        let isPlayerSameAsLast movements movement =
            (movements |> List.last).player  <> movement.player
        let play (movement:Movement) (movements:Movements) =
            match movement, movements with
            | {player = O}, [] -> false
            | {player = X}, [] -> true
            | m, ms -> isPlayerSameAsLast ms m && not (moveExists ms m )




    module TicTacToeTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Swensen.Unquote.Extensions
        open TicTacToe

        [<Test>]
        let ``First player cannot be O`` () =
             (play {player = O; location = {x = One; y = One }} []) =! false

        [<Test>]
        let ``First player must be X`` () =
             (play {player = X; location = {x = One; y = One }} []) =! true

        [<Test>]
        let  ``X cannot play twice in a row`` () =
             let movements = [{player = X; location = {x= One; y = Two}}]
             let movement = {player = X; location = {x = One; y = One }}
             (play  movement movements ) =! false

        [<Test>]
        let ``O can play after X`` () =
            let movements = [{player = X; location = {x= One; y = One}}]
            let movement = {player = O; location = {x = One; y = Two }}
            (play  movement movements ) =! true

        [<Test>]
        let ``A player can't play in a previous played position`` () =
            let firstMovement = {player = X; location = {x= One; y = One}}
            let movements = [firstMovement]
            let movement = {player = O; location = {x = One; y = One }}
            (play  movement movements ) =! false
