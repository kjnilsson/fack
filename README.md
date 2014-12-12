##fack.fs
========

rack-esque fsharp utlity around HttpListener

####Installation

Either manually copy fack.fs to your project or use [paket](http://fsprojects.github.io/Paket/) to add a source file reference.

####Usage

```fsharp
#load "fack.fs"
open Fack
open Fack.Host

let app (e: Env) =
    async {
        return Response (200, Map.empty, Data "HELLO WORLD"B) }

run app "http://*:4567/"
```
