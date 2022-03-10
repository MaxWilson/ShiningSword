module Domain

type PointCoord = PointCoord of x: int * y:int
type Connection = Connection of x: int * y: int
type Maze = {
    size: int * int
    connections: bool[][]
    }

let newMaze (width, height, initialConnection) = { size = (width, height); connections = Array.init (width+1) (fun _ -> Array.create (height+1) initialConnection) }

type Direction = Up | Down | Left | Right

let moveTo direction (PointCoord(x, y)) =
    match direction with
        | Up -> x, y+1
        | Down -> x, y-1
        | Left -> x-1, y
        | Right -> x+1, y
    |> PointCoord

let connectionTo direction (PointCoord(x, y)) =
    match direction with
        | Up -> x, y+1
        | Down -> x, y
        | Left -> x, y
        | Right -> x+1, y
    |> Connection
