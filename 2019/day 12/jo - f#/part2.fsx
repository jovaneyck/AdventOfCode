type Vector = {x : int64; y : int64; z : int64 }
type Moon = { id : int; position : Vector; velocity : Vector }

let init id x y z = { id = id; position = { x = x; y = y; z = z}; velocity = { x =0L; y = 0L; z = 0L }}

let rec pairs = 
    function
    | x :: xs -> 
        let ofX = xs |> List.map (fun y -> (x,y))
        let r = pairs xs
        List.append ofX r
    | [] -> []

let gravity one other =
    let step a b =
        if a = b then 0L
        elif a < b then 1L
        else -1L
    { x = step one.x other.x
      y = step one.y other.y
      z = step one.z other.z }

let applyGravity (id, v) moons = 
    let moon = moons |> Map.find id
    let updated =
        { moon with velocity = 
            { 
                x = moon.velocity.x + v.x 
                y = moon.velocity.y + v.y 
                z = moon.velocity.z + v.z 
            }
        }
    moons |> Map.add updated.id updated

let applyVelocity moon = 
    let v = moon.velocity
    { moon with position = { 
            x = moon.position.x + v.x 
            y = moon.position.y + v.y
            z = moon.position.z + v.z         
        } 
    }

let step moons =
    let gravityVecs =
        moons
        |> (Map.toList >> List.map snd)
        |> pairs
        |> List.collect (fun (a,b) -> [a.id, gravity a.position b.position; b.id, gravity b.position a.position])

    let updatedGravity = gravityVecs |> List.fold (fun ms v -> applyGravity v ms) moons

    updatedGravity |> Map.map (fun _ v -> applyVelocity v)

let rec takeSteps s moons =
    if s = 0 then moons
    else takeSteps (s - 1) (step moons)

let periods moons =
    let xs ms = ms |> Map.toList |> List.map (fun (id,m) -> (id, m.position.x, m.velocity.x))
    let ys ms = ms |> Map.toList |> List.map (fun (id,m) -> (id, m.position.y, m.velocity.y))
    let zs ms = ms |> Map.toList |> List.map (fun (id,m) -> (id, m.position.z, m.velocity.z))

    let rec run dim init moons gen =
        let next = step moons
        if dim next = init
        then gen
        else    
            run dim init next (gen + 1L)
    
    let x = run xs (xs moons) moons 1L
    let y = run ys (ys moons) moons 1L
    let z = run zs (zs moons) moons 1L
    [x;y;z]

let lcm x y = 
    let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
    x * y / (gcd x y)

let example = [
    init 0 -1L 0L 2L
    init 1 2L -10L -7L
    init 2 4L -8L 8L
    init 3 3L 5L -1L ]
let big_example = 
    [
        init 0 -8L -10L 0L
        init 1 5L 5L 10L
        init 2 2L -7L 3L
        init 3 9L -8L -3L ]
let input = [ init 0 -9L 10L -1L;init 1 -14L -8L 14L;init 2 1L 5L 6L;init 3  -19L 7L 8L ]

//big_example 
//|> List.map (fun m -> m.id, m) |> Map.ofList 
//|> takeSteps 100

input
|> List.map (fun m -> m.id, m) |> Map.ofList
|> periods
|> List.fold lcm 1L