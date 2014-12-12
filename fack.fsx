#load "fack.fs"
open Fack
open Fack.Ware
open Fack.Util


let app (e: Env) =
    async {
        return Response (200, Map.empty, Data "HELLO WORLD"B) }

let app2 (e: Env) =
    async {
        let data = encode (sprintf "%A" e)
        return Response (200, Map.empty, Data data) }

let mapp = urlMap (["^one", app; "^two", app2]) notFound

// start the receiver
//let endpoint = "http://*:4567/fack/"
let endpoint = "http://*:4567/"

run mapp endpoint


