#I __SOURCE_DIRECTORY__
#load @"Optics.fs"
#load @"Common.fs"

// Experimental encounter generation with the intent of making
// lots of smaller, Fighter-oriented fights and occasional bigger
// fights that are well suited for short rest classes like Fiendlocks
// and rare even bigger fights that need wizards to shoot
// multiple Fireballs/other AoEs. Completely bypasses Lanchester's
// laws in favor of trying to create different STYLES of encounter
// through sheer quantity.
// Anyway, this is just to model the probabilities.

// Each 

let eval10 f = List.init 10 (thunk1 f ())
let d n = rand n
let roll(n,d) = List.init n (thunk1 rand d) |> List.sum
let basic()= roll(10,10)
eval10 basic
let product() = roll(d 10, 10)
eval10 product
eval10 (fun () -> roll(d 10, d 10))
eval10 (fun () -> List.init (min (d 10) (d 10)) (fun _ -> min (d 10) (d 10)) |> List.sum)
let explode n =
    let rec recur sum =
        match d n with
        | v when v = n -> recur (sum + n - 1)
        | v -> sum + v
    recur 0
let explodeOrcs() = roll(explode 6, 6)
eval10 explodeOrcs
eval10 (fun() -> roll(explode 10, 10))
let print = printfn "%s"
let stat f =
    let n = 1000
    let list = List.init n (thunk1 f ()) |> List.sort
    let avg = List.averageBy float list
    let small100 = list.[100]
    let big100 = list.[900]
    let underN N =
        let count = list |> List.filter (flip (<=) N) |> List.length
        count * 100 / n
    $"{small100}-{big100} ({avg}), {underN 8}%% under 8, {underN 20}%% under 20, {underN 50}%% under 50" 
stat (fun () -> roll(d 10, d 10)) |> print
stat (fun () -> List.init (min (d 10) (d 10)) (fun _ -> min (d 10) (d 10)) |> List.sum) |> print
stat explodeOrcs

// I don't love exploding dice because I don't understand the upper bound(ish)
// behavior very well, could grow out of control. I think I still want
// the ability to say 1-100 orcs, just with a bias towards the lower numbers.
// But (d10)d(d10), even with disadvantage, doesn't have enough bias for me.
// I want an even sharper curve.
// How about 1-100: explode 10 10, capped at 100?
stat (fun () -> min 100 (roll(explode 6, 6)))
eval10 (fun () -> min 100 (roll(explode 6, 6)))
// I think that one is biased TOO strongly against the caps actually. And the 6s
// feel arbitrary. I want about 50% under 8, 20% over 20, 5% over 50.
// Let's try curving more sharply via more rolls.
stat (fun () -> min 100 (roll(roll(d 3, d 3), roll(2,6) - 1)))
eval10 (fun () -> min 100 (roll(roll(d 3, d 3), roll(2,6) - 1)))
