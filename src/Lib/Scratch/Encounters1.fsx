#I __SOURCE_DIRECTORY__
#load "Encounters.fsx"
open Encounters
generateUpTo Ultimate |> describe
generateUpTo Easy |> describe
