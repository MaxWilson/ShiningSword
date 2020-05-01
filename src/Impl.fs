module Impl
open Optics
open Optics.Operations
open Arch

let generateId : GenerateId<_> =
    fun lens state ->
        let id = state |> read lens
        id, state |> over lens ((+) 1)

let insert : Insert<_,_> =
    fun idLens containerLens value state ->
        let id, state = (state |> generateId idLens)
        id, state |> over containerLens (Map.add id value)
