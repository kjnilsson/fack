#load "fack.fs"
open Fack
open Fack.Host

let app (e: Env) =
    async {
        return Response (200, Map.empty, Data "HELLO WORLD"B) }

open Fack.Ware
open Fack.Util

let app2 (e: Env) =
    async {
        let data = encode (sprintf "%A" e)
        return Response (200, Map.empty, Data data) }

//middleware
let mapp = urlMap (["^/one", app; "^/two", app2]) notFound

let endpoint = "http://*:4567/fack/"

run mapp endpoint
