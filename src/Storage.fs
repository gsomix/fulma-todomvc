module Storage

open Fable.Core
open Fable.Import
open Thoth.Json

let inline toJson x = Encode.Auto.toString(0, x)

let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

let [<Literal>] private STORAGE_KEY = "todo-list"

let inline load<'T> (): 'T option =
    Browser.localStorage.getItem(STORAGE_KEY)
    |> unbox
    |> Core.Option.map (ofJson)

let inline save (model: 'T) =
    Browser.localStorage.setItem(STORAGE_KEY, toJson model)