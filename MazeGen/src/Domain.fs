module Domain

type Coord = int * int
type Maze = {
    size: int * int
    connections: bool[][]
    }

let newMaze (width, height, initialConnection) = { size = (width, height); connections = Array.init (width+1) (fun _ -> Array.create (height+1) initialConnection) }
