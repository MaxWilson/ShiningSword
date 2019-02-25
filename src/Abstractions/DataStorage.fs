module DataStorage
open Interaction
open Newtonsoft.Json
open System.Net.Http

let jsonConverter = Fable.JsonConverter() :> JsonConverter
let getAuthToken() = InteractionBuilder<string, string>() {
    match Globals._token with
    | Some token -> return token
    | None ->
        let! token = "Please enter a WilsonData auth token (from e.g. F12 on https://maxwilson.github.io/ThingTracker/)", Some
        Globals._token <- Some token
        return token
    }

let cl = new System.Net.Http.HttpClient()
let showRaw (token:string) tag =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s" tag)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    resp.Content.ReadAsStringAsync().Result

let list<'t> (token:string) tag =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s" tag)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    resp.Content.ReadAsStringAsync().Result |> fun v -> JsonConvert.DeserializeObject<'t list>(v, [|jsonConverter|])

let save (token:string) tag id (data: 't) =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, url)
    msg.Content <- new StringContent(JsonConvert.SerializeObject(data, [|jsonConverter|]))
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)

let load<'t> (token:string) tag id =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    let json = resp.Content.ReadAsStringAsync().Result
    if resp.IsSuccessStatusCode then Ok(JsonConvert.DeserializeObject<'t>(json, [|jsonConverter|])) else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)

let delete (token:string) tag id =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Delete, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)
