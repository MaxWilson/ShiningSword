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
let showRaw tag = InteractionBuilder<string, string>() {
    let! token = getAuthToken()
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s" tag)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    return resp.Content.ReadAsStringAsync().Result
    }

let list<'t> tag = InteractionBuilder<string, string>() {
    let! token = getAuthToken()
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s" tag)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    return resp.Content.ReadAsStringAsync().Result |> fun v -> JsonConvert.DeserializeObject<'t list>(v, [|jsonConverter|])
    }

let save tag id (data: 't) = InteractionBuilder<string, string>() {
    let! token = getAuthToken()
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, url)
    msg.Content <- new StringContent(JsonConvert.SerializeObject(data, [|jsonConverter|]))
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    return if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)
    }

let load<'t> tag id = InteractionBuilder<string, string>() {
    let! token = getAuthToken()
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    return resp.Content.ReadAsStringAsync().Result |> fun v -> JsonConvert.DeserializeObject<'t>(v, [|jsonConverter|])
    }

let delete tag id = InteractionBuilder<string, string>() {
    let! token = getAuthToken()
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Delete, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    return if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)
    }
