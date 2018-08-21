module Storage

open Fable.Core
open Fable.Import

let [<Literal>] private STORAGE_KEY = "todo-list"

let [<PassGenerics>] load<'T> (): 'T option =
    Browser.localStorage.getItem(STORAGE_KEY)
    |> unbox
    |> Core.Option.map (JsInterop.ofJson)

let save<'T> (model: 'T) =
    Browser.localStorage.setItem(STORAGE_KEY, JsInterop.toJson model)