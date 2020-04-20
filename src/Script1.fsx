#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Aether.fs"
#load @"Abstractions\Parsing.fs"
open Aether
open Aether.Operators

type Creature = { name: string }
type Model = { timer: int; creatures: Creature list }
type Update = Update of string
type IntermediateModel = { toUpdate: Update list; model: Model }

type Data =
    static member get getInner (m: 'm) = m |> Lens.get ((Lens.lens id (fun v _ -> v: 'm)) |> getInner)
    static member set v getInner (m: 'm): 'm = m |> Lens.set ((Lens.lens id (fun v _ -> v: 'm)) |> getInner) v
    static member over getInner f (m: 'm) = m |> Lens.over ((Lens.lens id (fun v _ -> v: 'm)) |> getInner) f
    static member creatures(outer: Lens.Lens<_,Model,Model,_>): Lens.Lens<_,_,_,_> = Lens.lens (fun st -> st.creatures) (fun v st -> { st with creatures = v }) >> outer
    static member creatures(outer: Lens.Lens<_,IntermediateModel,IntermediateModel,_>): Lens.Lens<_,_,_,_> = (Lens.lens (fun st -> st.model) (fun v st -> { st with model = v })) |> Data.creatures >> outer
    static member front(outer: Lens.Lens<_,'t list,'t list,_>): Lens.Lens<_,_,_,_> = (Lens.lens (function [] -> None | h::_ -> Some h) (fun v' st -> match v' with None -> st | Some v -> v::st)) >> outer
    static member name(outer: Lens.Lens<_,_,_,_>): Lens.Lens<_,_,_,_> = Lens.lens (fun st -> st.name) (fun v d -> { d with name = v }) >> outer

let model_ = (fun st -> printfn "Getting model"; st.model), (fun v st -> printfn "Setting model"; { st with model = v })
let creatures_ = (fun st -> st.creatures), (fun v st -> { st with creatures = v })
let name_ = (fun st -> printfn "Getting name"; st.name), (fun v st -> printfn "Setting name"; { st with name = v })

let model' = { toUpdate = []; model = { timer = 0; creatures = []} }
let appendCreature name lst = { name = name }::lst
model' |> (appendCreature "Bob" >> appendCreature "Friday") ^% (model_ >-> creatures_)
