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

let init () : Model * Cmd<Msg> =
    let innerModel, innerCmd = TodoList.State.init ()
    let initialModel = {
        TodoList = innerModel
    }
    initialModel, Cmd.map TodoListMsg innerCmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | TodoListMsg innerMsg ->
        let innerModel, innerCmd = TodoList.State.update innerMsg currentModel.TodoList
        let model = { currentModel with TodoList = innerModel }
        model, Cmd.map TodoListMsg innerCmd

let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ ]
        [ Hero.body [ ]
            [ Columns.columns [ ]
                [ Column.column [ Column.Width (Screen.All, Column.IsHalf)
                                  Column.Offset (Screen.All, Column.IsOneQuarter) ]
                    [ TodoList.View.view
                        model.TodoList
                        (fun innerMsg -> dispatch <| TodoListMsg innerMsg) ] ] ] ]


Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
