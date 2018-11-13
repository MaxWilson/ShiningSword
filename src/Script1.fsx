
#load @"..\.paket\load\net471\Fable.Elmish.Browser.fsx"
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

type Route
      = Search of string
      | Blog of int
      | User of string
      | Comment of string*int

let route =
    oneOf [
        map Search  (s "search" </> str)
        map Blog    (s "blog" </> i32)
        map User    (s "user" </> str)
        map (fun x y -> Comment(x,y)) (s "user" </> str </> s "comments" </> i32)
        //map Comment (s "user" </> str </> "comments" </> i32)
        ]
parse route "search/foo" Map.empty
parse route "search/foo" Map.empty

let parse x y = Elmish.Browser.UrlParser.parse x y Map.empty

parse (s "search" </> str) "search" // None
parse (s "search" </> str) "search/now" // None
parse route "user/toothpick/33" // None
parse route "blog/33" // None
parse route "user/toothpick/comments/33" // None
