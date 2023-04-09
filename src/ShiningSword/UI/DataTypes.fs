[<AutoOpen>]
module UI.DataTypes

// These data types would go in TraitView.fs except that React FastRefresh requires only React Components be exported from the file
type Key = string list

[<AutoOpen>]
module DataBuilder =
    type DataCtx = { searchPrefix: Key; queue: Map<Key, string>; includePotentials: bool }
        with static member fresh = { searchPrefix = []; queue = Map.empty; includePotentials = false }

type TraitMsg =
    | Queue of Key
    | QueueData of Key * string
    | Unqueue of Key
    | ClearStrictlyUnder of Key // clears all subkeys. E.g. if WeaponMaster (Two Weapon) is partially selected, eliminate TwoWeapon and any weapon choices under it but not WeaponMaster itself
