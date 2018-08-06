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
    | LoadTodoList
    | LoadingFailure of string

let init (): Model * Cmd<Msg> =
    let todoList, cmd = TodoList.State.init()
    { TodoList = todoList }, Cmd.batch [ Cmd.map TodoListMsg cmd; Cmd.ofMsg LoadTodoList ]

let setStorage (model: Model): Cmd<Msg> =
    Cmd.attemptFunc Storage.save model (fun exn -> LoadingFailure (string exn))

let update (msg : Msg) (curModel: Model): Model * Cmd<Msg> =
    match msg with
    | TodoListMsg innerMsg ->
        let innerModel, innerCmd = TodoList.State.update innerMsg curModel.TodoList
        { curModel with TodoList = innerModel }, Cmd.map TodoListMsg innerCmd
    | LoadingFailure err ->
        Fable.Import.Browser.console.error(err)
        curModel, Cmd.none
    | LoadTodoList ->
        let loadedModel =
            Storage.load ()
            |> Option.map (fun saved -> { curModel with TodoList = saved.TodoList })
        defaultArg loadedModel curModel, Cmd.none

let updateWithStorage (msg: Msg) (currentModel: Model): Model * Cmd<Msg> =
    match msg with
    | LoadingFailure _ -> currentModel, Cmd.none
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
                        model.TodoList (fun innerMsg -> TodoListMsg innerMsg |> dispatch) ] ] ] ]

Program.mkProgram init updateWithStorage view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
