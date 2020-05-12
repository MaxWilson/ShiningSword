#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"
#load @"Abstractions\Parsing.fs"
open Optics
open Optics.Operations

type Creature = { name: string }
type Model = { timer: int; creatures: Creature list }
type Update = Update of string
type IntermediateModel = { toUpdate: Update list; model: Model }

type Data =
    static member get getInner (m: 'm) = m |> read ((lens id (fun v _ -> v: 'm)) |> getInner: Lens<_,_>)
    static member set v getInner (m: 'm): 'm = m |> write ((lens id (fun v _ -> v: 'm)) |> getInner : Lens<_,_>) v
    static member over getInner f (m: 'm) = m |> over ((lens id (fun v _ -> v: 'm)) |> getInner : Lens<_,_>) f
    static member creatures(outer: Lens<Model,_>): Lens<_,_> = outer => lens (fun st -> st.creatures) (fun v st -> { st with creatures = v })
    static member creatures(outer:Lens<IntermediateModel,_>): Lens<_,_> = outer |> Data.creatures
    static member front(outer: Lens<'t list,_>): Lens<_,_> = outer => (lens (function [] -> None | h::_ -> Some h) (fun v' st -> match v' with None -> st | Some v -> v::st))
    static member name(outer: Lens<_,_>): Lens<_,_> = outer => lens (fun st -> st.name) (fun v d -> { d with name = v })

let model_ = lens (fun st -> printfn "Getting model"; st.model) (fun v st -> printfn "Setting model"; { st with model = v })
let creatures_ = lens (fun st -> st.creatures) (fun v st -> { st with creatures = v })
let name_ = lens (fun st -> printfn "Getting name"; st.name) (fun v st -> printfn "Setting name"; { st with name = v })

let model' = { toUpdate = []; model = { timer = 0; creatures = []} }
let appendCreature name lst = { name = name }::lst
model' |> over (model_ => creatures_) (appendCreature "Bob" >> appendCreature "Friday")
