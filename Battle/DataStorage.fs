module DataStorage
open Newtonsoft.Json
open System.Net.Http

let serialize x = Thoth.Json.Net.Encode.Auto.toString(0, x)
let deserialize<'t> x = Thoth.Json.Net.Decode.Auto.fromString<'t> x

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
    resp.Content.ReadAsStringAsync().Result |> deserialize

let save (token:string) tag id (data: 't) =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, url)
    msg.Content <- new StringContent(serialize data)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)

let load<'t> (token:string) tag id =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Get, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    let json = resp.Content.ReadAsStringAsync().Result
    if resp.IsSuccessStatusCode then deserialize<'t> json else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)

let delete (token:string) tag id =
    let url = System.Uri(sprintf "https://wilsondata.azurewebsites.net/api/%s/%s" tag id)
    let msg = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Delete, url)
    msg.Headers.Add("X-ZUMO-AUTH", token)
    let resp = cl.SendAsync(msg).Result
    if resp.IsSuccessStatusCode then Ok() else Error(sprintf "%s: %s" (resp.StatusCode.ToString()) resp.ReasonPhrase)
