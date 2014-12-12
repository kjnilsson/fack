module Fack

open System
open System.IO
open System.Net

type Method =
    | Get
    | Post
    | Put
    | Delete
    | Options
    | Trace
    | Connect
    | Custom of string

type HttpBody =
    | Data of byte []
    | Stream of Stream

type ServerInfo =
    { Name : string
      Port : int }

type Env =
    { Method : Method
      ScriptName : string 
      PathInfo : string
      QueryString : string 
      Server : ServerInfo 
      Headers : Map<string, string>
      UrlScheme : string
      Body : HttpBody }

type Response = Response of int * Map<string,string> * HttpBody //status, headers, content
type Application = Env -> Async<Response>
type Middleware = Application -> Application

module Util =
    let encode (s: string) = System.Text.Encoding.UTF8.GetBytes s
    let decode (data: byte []) = System.Text.Encoding.UTF8.GetString data

    let inline mapQuery (q : string) =
        match q with
        | "" -> Map.empty
        | s ->
            let skip = if s.StartsWith("?") then 1 else 0
            s.[skip..].Split([|';'|])
            |> Array.choose (fun x -> 
                match x.Split('=') with
                | [|k;v|] -> Some (k, v)
                | _ -> None )
            |> Map.ofArray

module Host =
    type private Ctx = HttpListenerContext
    let private requestMethod (req : HttpListenerRequest) =
        match req.HttpMethod with
        | "GET"     -> Get     | "POST"   -> Post
        | "PUT"     -> Put     | "DELETE" -> Delete
        | "OPTIONS" -> Options | "TRACE"  -> Trace
        | "CONNECT" -> Connect | h        -> Custom h

    let private nvkMap (nvk : Collections.Specialized.NameValueCollection) =
        nvk.AllKeys
        |> Array.fold (fun s k -> Map.add k (nvk.Get k) s) Map.empty

    let inline private toEnv prefix (req : HttpListenerRequest) : Env =
        { Method = requestMethod req
          ScriptName = prefix 
          PathInfo = "/" + req.Url.AbsolutePath.Substring(prefix.Length)
          QueryString = req.Url.Query
          Server = { Name = req.Url.Host; Port = req.Url.Port }
          Headers = nvkMap req.Headers
          UrlScheme = req.Url.Scheme 
          Body = Stream req.InputStream }

    let private getContext (ctx : HttpListener) =
        Async.FromBeginEnd(ctx.BeginGetContext, ctx.EndGetContext)

    let inline private addHeader (ctx:Ctx) key value =
        ctx.Response.AddHeader (key, value)

    let inline private writeBody (b: HttpBody) (s: Stream) =
        async {
            match b with
            | Data arr -> do! s.AsyncWrite(arr, 0)
            | Stream str -> 
                str.CopyTo(s) } //TODO make async

    let inline private handleContentLength (b : HttpBody) (resp : HttpListenerResponse) =
        match b with
        | Data data ->
            resp.ContentLength64 <- int64 data.Length
        | _ -> ()

    let private handler prefix (app : Application) (ctx : Ctx) =
        async {
            let! res = ctx.Request |> toEnv prefix |> app |> Async.Catch
            match res with
            | Choice1Of2 (Response (status, headers, data)) -> 
                ctx.Response.StatusCode <- status 
                headers |> Map.iter (addHeader ctx) 
                handleContentLength data ctx.Response
                do! writeBody data ctx.Response.OutputStream
            | Choice2Of2 ex ->
                ctx.Response.StatusCode <- 500
                ctx.Response.StatusDescription <- ex.Message
            ctx.Response.Close() }

    let receive (app : Application) endpoint =
        let listener = new HttpListener()
        listener.Prefixes.Add(endpoint)
        listener.Start()
        let epu = Uri (endpoint.Replace("*", "x").Replace("+", "x") )
        let prefix = epu.AbsolutePath 
        let rec inner () =
            async {
                let! ctx = getContext listener
                Async.Start (handler prefix app ctx)
                return! inner () }
        inner () 

    let run (app: Application) endpoint =
        let cts = new System.Threading.CancellationTokenSource()
        Async.Start (receive app endpoint, cts.Token)
        { new IDisposable with
            member __.Dispose() =
                cts.Cancel() }

module Ware =
    open System.Text.RegularExpressions

    let private regex i s = 
        Regex.Match(i,s).Success

    let notFound (e: Env) =
        async { return Response (404, Map.empty, Data "not found"B) }

    let urlMap (m : (string * Application) list) (def: Application) (env : Env) =
        async {
            //doing an "express" - warning: not webscale
            match List.tryFind (fst >> regex env.PathInfo) m with
            | Some (_, app) -> return! app env
            | None -> return! def env }

    let methodMap (m : Map<Method, Application>) (def: Application) (env : Env) =
        async {
            match Map.tryFind env.Method m with
            | Some app -> return! app env
            | None -> return! def env }

