#load "IntCode.fsx"
open IntCode

type Rotation = RotateLeft | RotateRight
type Orientation = Up | Right | Down | Left
type Location = { x : int; y : int }
type Robot = { location : Location; orientation : Orientation }
type State = { robot : Robot; colors : Map<Location,int64>; programState : IntCode.State }

let rotate direction robot  =
    let rotation = 
        match direction with
        | 0L -> RotateLeft
        | 1L -> RotateRight
    match rotation, robot.orientation with
    | RotateLeft, Up -> Left
    | RotateLeft, Left -> Down
    | RotateLeft, Down -> Right
    | RotateLeft, Right -> Up
    | RotateRight, Up -> Right
    | RotateRight, Right -> Down
    | RotateRight, Down -> Left
    | RotateRight, Left -> Up
    |> (fun o -> { robot with orientation = o })

let forward robot = 
    match robot.orientation with
    | Up -> { robot with location = { x = robot.location.x; y = robot.location.y - 1 } }
    | Down -> { robot with location = { x = robot.location.x; y = robot.location.y + 1 } }
    | Right -> { robot with location = { x = robot.location.x + 1; y = robot.location.y } }
    | Left -> { robot with location = { x = robot.location.x - 1; y = robot.location.y } }

let colorAt location colors =
    colors |> Map.tryFind location |> (fun c -> defaultArg c 0L)

let rec run (runner : IntCode.State -> IntCode.State) (state : State) =
    match state.programState.mode with
    | IntCode.Finished ->
        state
    | IntCode.Running -> 
        run runner { state with programState = runner state.programState }
    | IntCode.AwaitingInput -> 
        let [direction;new_color] = state.programState.output
        let new_colors = state.colors |> Map.add state.robot.location new_color
        let new_robot = state.robot |> rotate direction |> forward
        let nextProg = runner { state.programState with 
                                    mode = IntCode.Running;
                                    input = new_colors |> colorAt new_robot.location |> List.singleton 
                                    output = [] }
        let next = 
            { state with 
                robot = new_robot
                programState = nextProg 
                colors = new_colors }
        //printfn "%A" next.robot
        //printfn "%A" (next.colors |> Map.count)
        run runner next

let start p input = { robot = { location = { x = 0; y = 0 }; orientation = Up }; colors = Map.empty; programState = { IntCode.initState [input] p with mode = IntCode.Running} }
    
#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote
let aProgramState outputs = {IntCode.initState [] [] with mode = IntCode.AwaitingInput; output = outputs |> List.rev }

let t  () =
    test <@
            //Let's run through the scenario
            let one = run (fun state -> aProgramState [1L;0L]) (start [] 0L)
            let stepOne =
                one.colors |> Map.toList = [({x = 0; y = 0;}, 1L)] 
                && one.robot.orientation = Left
                && one.robot.location = { x = -1; y = 0 } 
                && one.programState.mode = IntCode.AwaitingInput

            let two = run (fun state -> aProgramState [0L;0L]) one

            let stepTwo =
                two.colors |> Map.toList = [({x = -1;y = 0;}, 0L); ({x = 0;y = 0;}, 1L)]
                && two.robot.orientation = Down
                && two.robot.location = { x = -1; y = 1 }
                && two.programState.mode = IntCode.AwaitingInput
        
            let three =
                two
                |> run (fun state -> aProgramState [1L;0L])
                |> run (fun state -> aProgramState [1L;0L])

            let stepThree = 
                three.colors |> Map.toList = [({x = -1;y = 0;}, 0L); ({x = -1;y = 1;}, 1L); ({x = 0;y = 0;}, 1L); ({x = 0;y = 1;}, 1L)] 
                && three.robot.location = { x = 0; y = 0 }
                && three.robot.orientation = Up
                && three.programState.mode = IntCode.AwaitingInput

            let four =
                three
                |> run (fun state -> aProgramState [0L;1L])
                |> run (fun state -> aProgramState [1L;0L])
                |> run (fun state -> {aProgramState [1L;0L] with mode = IntCode.Finished })

            let stepFour =
                four.colors |> Map.count = 6
                && four.colors |> Map.toList = [({x = -1; y = 0;}, 0L); ({x = -1; y = 1;}, 1L); ({x = 0; y = 0;}, 0L); ({x = 0; y = 1;}, 1L); ({x = 1; y = -1;}, 1L); ({x = 1;y = 0;}, 1L)]
                && four.robot.orientation = Left
                && four.robot.location = { x = 0; y = -1 }
                && one.programState.mode = IntCode.Finished

            stepOne && stepTwo && stepThree && stepFour
            @>

//t ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList
let r = run IntCode.run <| start p 0L
r.colors |> Map.count