module UI.Storage

open Common
open Fable.PowerPack
open Fable.Core
open JsInterop

// because we store auth state "globally" instead of in the model, we define it separately
// from UI.Types instead of using the Elmish pattern

module Auth =
  type Provider = Facebook // TODO: add Google, MSA
  (* Transitions:
     Unitialized (on app start) -> Authenticated | Unauthenticated when initialization completes
     Unauthenticated -> Authenticated when user logs in and Facebook sends a response
     Authenticated -> Authorized when WilsonData EasyAuth provides an X-ZUMO-AUTH token
     Authenticated | Authorized -> Unauthenticated when user logs out
  *)
  type AccessToken = string
  type State =
    | Uninitialized
    | Unauthenticated
    | Authenticated of Provider * AccessToken
    | Authorized of AccessToken
    | Error of string

module Facebook =
    open UI.Types
    type AuthStatus = {
        accessToken: string
        }
    type AuthResponse = {
        authResponse: AuthStatus
        status: string
        }
    let onAuth (handler : Auth.State -> unit) = function
        | { status = "connected"; authResponse = { accessToken = accessToken } } ->
            handler (Auth.State.Authenticated (Auth.Provider.Facebook, accessToken))
        | _ ->
            handler Auth.State.Unauthenticated
    [<Emit("FB.login($0)")>]
    let private FBLogin handler = jsNative
    let Login (handler : Auth.State -> unit) =
        FBLogin(onAuth handler) |> ignore
    [<Emit("FB.logout($0)")>]
    let private FBLogout handler = jsNative
    let Logout (handler : unit -> unit) =
        FBLogout(handler) |> ignore
    [<Emit("""
        window.fbAsyncInit = function() {
            FB.init({
                appId            : '2065879493471182',
                cookie         : true,
                xfbml            : true,
                version        : 'v3.2'
            });

            FB.AppEvents.logPageView();
            FB.getLoginStatus(resp => $0(resp))
        };

        (function(d, s, id){
             var js, fjs = d.getElementsByTagName(s)[0];
             if (d.getElementById(id)) {return;}
             js = d.createElement(s); js.id = id;
             js.src = "https://connect.facebook.net/en_US/sdk.js";
             fjs.parentNode.insertBefore(js, fjs);
         }(document, 'script', 'facebook-jssdk'));
    """)>]
    let private initializeFacebook(_onAuth:AuthResponse -> unit) = jsNative
    let initialize handler =
        initializeFacebook (onAuth handler)

module EasyAuth =
    open Auth
    type Response = { authenticationToken: string }
    let ofFacebook token =
        promise {
            let! resp = Fable.PowerPack.Fetch.postRecord "https://wilsondata.azurewebsites.net/.auth/login/facebook" (createObj ["access_token" ==> token])[]
            if resp.Ok then
                try
                    let! retval = resp.json<Response>()
                    return retval.authenticationToken |> Auth.State.Authorized
                with e ->
                    return Auth.State.Error <| sprintf "Could not authorize due to error: %s" (e.ToString())
            else
                return "Could not authorize" |> Auth.State.Error
        }
    let mutable authState = Uninitialized
    let withToken handler =
        let rec loop() =
            let updateAndProceed authState' = authState <- authState'; loop()
            match authState with
            | Authorized token -> handler (Ok token)
            | Authenticated(Facebook, accessToken) ->
                ofFacebook(accessToken) |> Promise.iter(updateAndProceed)
            | Error msg ->
                handler (Result.Error msg)
            | Unauthenticated ->
                Facebook.Login (updateAndProceed)
            | Uninitialized ->
                Facebook.initialize (updateAndProceed)
        loop()

let save (token:string) tag id (data: 't) =
    let url = sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id
    Fable.PowerPack.Fetch.postRecord url data [Fetch.requestHeaders [Fable.PowerPack.Fetch.Fetch_types.HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
        |> Promise.map (fun resp -> if resp.Ok then Ok() else Error "Unable to save")

let load (token:string) tag id =
    let url = sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id
    Fable.PowerPack.Fetch.fetchAs url (Thoth.Json.Decode.Auto.generateDecoder<Model.Types.Battle2.Data>()) [Fetch.requestHeaders [Fable.PowerPack.Fetch.Fetch_types.HttpRequestHeaders.Custom ("X-ZUMO-AUTH", token)]]
        |> Promise.map Ok

type CloudStorage() =
    interface DataEngine.IDataStorage with
        member this.Save (label:DataEngine.Label) data callback =
            EasyAuth.withToken (function Result.Ok token -> save token "battle" label data |> Promise.iter callback | Result.Error msg -> Error msg |> callback)
        member this.Load label callback =
            // Workaround for Fable limitation: cannot reflect over 't at this point, but we know that it's always going to be a Battle2.Data, so just hardwire that
            EasyAuth.withToken (function Result.Ok token -> load token "battle" label |> Promise.iter callback | Result.Error msg -> Error msg |> callback)
