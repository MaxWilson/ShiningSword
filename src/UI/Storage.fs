module UI.Storage

open Common
open UI.Types
open Fable
open Fable.Core
open JsInterop
open Fetch

// because we store auth state "globally" instead of in the model, we define it separately
// from UI.Types instead of using the Elmish pattern
let serialize x = "" //Thoth.Json.Encode.Auto.toString(0, x)
let deserialize<'t> x: Result<'t, string> = failwith "Not impl" // Thoth.Json.Decode.Auto.fromString<'t> x

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
    module Cache =
        let private localStorageKey = "authorizationState"
        let mutable private authState = Uninitialized
        let update state =
            Browser.WebStorage.localStorage.[localStorageKey] <- serialize state
            authState <- state
        let tryGet() =
            match authState with
            | Uninitialized ->
                match Browser.WebStorage.localStorage.getItem localStorageKey with
                | "" -> Uninitialized
                | json ->
                    match deserialize<State>(json) with
                    | Ok state -> state
                    | Result.Error _ -> Uninitialized
            | _ -> authState
        let clear err =
            Browser.WebStorage.localStorage.removeItem localStorageKey
            authState <- Error err



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
    let private initializeFacebook(_onAuth: AuthResponse -> unit) = jsNative
    let mutable private initialized = false
    let initialize handler =
        initialized <- true
        initializeFacebook (onAuth handler)
    [<Emit("""FB.getLoginStatus(resp => $0(resp))""")>]
    let private FBGetLoginStatus(_onAuth: AuthResponse -> unit) = jsNative
    let getLoginStatus handler =
        if not initialized then
            initialize handler
        else
            FBGetLoginStatus (onAuth handler)


module EasyAuth =
    open Auth
    type Response = { authenticationToken: string }
    let ofFacebook token =
        promise {
            let h = requestHeaders[HttpRequestHeaders.Custom("access_token", token)]
            let props = [RequestProperties.Method HttpMethod.POST; h]
            let! resp = Fetch.fetch "https://wilsondata.azurewebsites.net/.auth/login/facebook" props
            if resp.Ok then
                try
                    let! retval = resp.json<Response>()
                    return retval.authenticationToken |> Auth.State.Authorized
                with e ->
                    return Auth.State.Error <| sprintf "Could not authorize due to error: %s" (e.ToString())
            else
                return "Could not authorize" |> Auth.State.Error
        }
    let withToken (progressCallback: Types.ProgressCallback) handler =
        let rec loop() =
            let updateAndProceed authState' =
                Auth.Cache.update authState'
                loop()
            match Auth.Cache.tryGet() with
            | Authorized token ->
                progressCallback NotBusy
                handler (Ok token)
            | Authenticated(Facebook, accessToken) ->
                progressCallback (BusyWith "Authorizing...")
                ofFacebook(accessToken) |> Promise.iter(updateAndProceed)
            | Error _ ->
                progressCallback (BusyWith "Initializing...")
                Facebook.getLoginStatus updateAndProceed
            | Unauthenticated ->
                progressCallback (BusyWith "Logging in to Facebook...")
                Facebook.Login (updateAndProceed)
            | Uninitialized ->
                progressCallback (BusyWith "Initializing...")
                Facebook.initialize updateAndProceed
        loop()

let save progressCallback (token:string) tag id (data: 't) =
    let url = sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id
    promise {
        try
            progressCallback (BusyWith (sprintf "Saving '%s'..." id));
            let h = requestHeaders[HttpRequestHeaders.Custom("X-ZUMO-AUTH", token); HttpRequestHeaders.Custom("access_token", token)]
            let props = [RequestProperties.Method HttpMethod.POST; h; RequestProperties.Body !^ (serialize data)]
            let! resp = Fetch.fetch url props
            progressCallback NotBusy
            return if resp.Ok then Ok() else Result.Error "Unable to save"
        with err ->
            Auth.Cache.clear (err.ToString())
            progressCallback NotBusy
            return Result.Error (err.ToString())
    }

let load progressCallback (token:string) tag id =
    let url = sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id
    promise {
        try
            progressCallback (BusyWith (sprintf "Loading '%s'..." id));
            let h = requestHeaders[HttpRequestHeaders.Custom("X-ZUMO-AUTH", token); HttpRequestHeaders.Custom("access_token", token)]
            let props = [RequestProperties.Method HttpMethod.POST; h]
            let! resp = Fetch.fetch url props
            let! resp = resp.json<DataEngine.Data>()
            progressCallback NotBusy
            return Ok(resp)
        with err ->
            Auth.Cache.clear (err.ToString())
            progressCallback NotBusy
            return Result.Error (err.ToString())
    }

// abstraction for throttling updates: sending them only after a brief delay
let throttledUpdate delayMilliseconds doneSignal handler =
    let mutable v = None
    let onTick _ =
        match v with
        | Some v when v <> doneSignal -> handler v
        | _ -> handler doneSignal
    fun arg ->
        v <- Some arg
        if arg = doneSignal then handler arg
        else
            Browser.Dom.window.setTimeout(onTick, delayMilliseconds, []) |> ignore

type CloudStorage(progressCallback: ProgressCallback) =
    interface DataEngine.IDataStorage with
        member this.Save (label:DataEngine.Label) data callback =
            let progressCallback = (throttledUpdate 200 NotBusy progressCallback)
            EasyAuth.withToken progressCallback (function Result.Ok token -> save progressCallback token "battle" label data |> Promise.iter callback | Result.Error msg -> Result.Error msg |> callback)
        member this.Load label callback =
            let progressCallback = (throttledUpdate 200 NotBusy progressCallback)
            EasyAuth.withToken progressCallback (function Result.Ok token -> load progressCallback token "battle" label |> Promise.iter callback | Result.Error msg -> Result.Error msg |> callback)
