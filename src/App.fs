module App.View

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Fulma

type Model = {
    TodoList: TodoList.Types.Model
}

type Msg =
    | TodoListMsg of TodoList.Types.Msg
    | LoadFromStorage
    | Failure of string

let init () : Model * Cmd<Msg> =
    let todoList, cmd = TodoList.State.init()
    { TodoList = todoList }, Cmd.batch [ Cmd.map TodoListMsg cmd; Cmd.ofMsg LoadFromStorage ]

let setStorage (model: Model) : Cmd<Msg> =
    Cmd.attemptFunc Storage.save model (fun exn -> Failure (string exn))

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | TodoListMsg innerMsg ->
        let innerModel, innerCmd = TodoList.State.update innerMsg currentModel.TodoList
        let model = { currentModel with TodoList = innerModel }
        model, Cmd.map TodoListMsg innerCmd
    | Failure err ->
        Fable.Import.Browser.console.error(err)
        currentModel, Cmd.none
    | LoadFromStorage ->
        let savedModel: Model option = Storage.load ()
        let model =
            match savedModel with
            | Some model -> { currentModel with TodoList = model.TodoList }
            | None -> currentModel
        model, Cmd.none

let updateWithStorage (msg: Msg) (currentModel: Model): Model * Cmd<Msg> =
    match msg with
    | Failure _ -> currentModel, Cmd.none
    | _ ->
        let model, cmd = update msg currentModel
        model, Cmd.batch [ setStorage model; cmd ]

let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ ]
        [ Hero.body [ ]
            [ Columns.columns [ ]
                [ Column.column [ Column.Width (Screen.All, Column.IsHalf)
                                  Column.Offset (Screen.All, Column.IsOneQuarter) ]
                    [ TodoList.View.view
                        model.TodoList
                        (fun innerMsg -> dispatch <| TodoListMsg innerMsg) ] ] ] ]

Program.mkProgram init updateWithStorage view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
