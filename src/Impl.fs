module Impl
open Optics
open Optics.Operations
open Arch

let generateId : GenerateId<_,_> =
    fun lens state ->
        let id = state |> read lens
        state |> over lens ((+) 1), id
