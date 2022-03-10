module Domain

type Coord = int * int
type Maze = {
    size: int * int
    connections: bool[][]
    }

let newMaze (width, height, initialConnection) = { size = (width, height); connections = Array.init width (fun _ -> Array.create height initialConnection) }
