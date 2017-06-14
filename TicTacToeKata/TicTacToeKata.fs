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

        type Status = InvalidPlayer | InvalidPlayerAndMove | InvalidMove | Accepted | XWin | YWin | Draw

        type PlayResult = Movements * Status

        let moveExists movements movement =
            movements |> List.exists (fun m -> m.location = movement.location )
        
        let isPlayerSameAsLast movements movement =
            (movements |> List.last).player = movement.player

        let isWinner movements =
            if(movements |> List.filter (fun x -> x.player = X) |> List.filter (fun x -> x.location.y = One) |> List.length = 3) then
                XWin
            else if (movements |> List.filter (fun x -> x.player = X) |> List.filter (fun x -> x.location.y = Two) |> List.length = 3) then
                XWin
            else if (movements |> List.filter (fun x -> x.player = X) |> List.filter (fun x -> x.location.y = Three) |> List.length = 3) then
                XWin
            else
                Accepted   
             

        let play (movement:Movement) (movements:Movements) =
            match movement, movements with
            | {player = O}, [] -> (movements, InvalidPlayer)
            | {player = X}, [] -> ([movement], Accepted)
            | m, ms -> match (isPlayerSameAsLast ms m, moveExists ms m)  with
                       | true, true -> (movements, InvalidPlayerAndMove)
                       | true, _ -> (movements, InvalidPlayer)
                       | _, true -> (movements, InvalidMove)
                       | _, _ -> (movements @ [movement], isWinner (movements @ [movement]))


    module TicTacToeTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Swensen.Unquote.Extensions
        open TicTacToe

        [<Test>]
        let ``First player cannot be O`` () =
             snd (play {player = O; location = {x = One; y = One }} []) =! InvalidPlayer

        [<Test>]
        let ``First player must be X`` () =
             let movement = {player = X; location = {x = One; y = One }}
             (play movement []) =! ([movement],Accepted)

        [<Test>]
        let  ``X cannot play twice in a row`` () =
             let movement = {player = X; location = {x = One; y = One }}
             let movement2 = {player = X; location = {x = One; y = Two }}
             snd (
                 []
                 |> play  movement
                 |> fst 
                 |> play movement2) =! InvalidPlayer

        [<Test>]
        let ``O can play after X`` () =
            let movement = {player = X; location = {x = One; y = One }}
            let movement2 = {player = O; location = {x = One; y = Two }}
              
            []
            |> play  movement
            |> fst 
            |> play movement2 =! ([movement; movement2],Accepted)
            

        [<Test>]
        let ``A player can't play in a previous played position`` () =
            let movement = {player = X; location = {x = One; y = One }}
            let movement2 = {player = O; location = {x = One; y = One }}
              
            []
            |> play  movement
            |> fst 
            |> play movement2 =! ([movement], InvalidMove)

        [<Test>]
        let ``X wins when he has three horizontal movements on the first row`` () =
            let movement1 = {player = X; location = {x = One; y = One }}
            let movement2 = {player = O; location = {x = One; y = Two }}
            let movement3 = {player = X; location = {x = Two; y = One }}
            let movement4 = {player = O; location = {x = Two; y = Two }}
            let movement5 = {player = X; location = {x = Three; y = One }}

            []
            |> play  movement1
            |> fst 
            |> play movement2
            |> fst 
            |> play movement3
            |> fst 
            |> play movement4
            |> fst 
            |> play movement5 =! ([movement1; movement2; movement3; movement4; movement5], XWin)

        [<Test>]
        let ``X wins when he has three horizontal movements on the second row`` () =
            let movement1 = {player = X; location = {x = One; y = Two }}
            let movement2 = {player = O; location = {x = One; y = One }}
            let movement3 = {player = X; location = {x = Two; y = Two }}
            let movement4 = {player = O; location = {x = Two; y = One }}
            let movement5 = {player = X; location = {x = Three; y = Two }}

            []
            |> play  movement1
            |> fst 
            |> play movement2
            |> fst 
            |> play movement3
            |> fst 
            |> play movement4
            |> fst 
            |> play movement5 =! ([movement1; movement2; movement3; movement4; movement5], XWin)

