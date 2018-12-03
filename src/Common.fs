module Common

let flip f x y = f y x
let random = System.Random()
let rand x = 1 + random.Next x
