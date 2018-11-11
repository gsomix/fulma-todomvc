module App.View

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Fulma
open Fable.Helpers.React

module Page =
    type Model = {
        TodoList: TodoList.Types.Model
    }

    type Msg =
        | TodoListMsg of TodoList.Types.Msg
        | LoadTodoList
        | LoadingFailure of string

    let init () =
        let todoList, _ = TodoList.State.init()
        { TodoList = todoList }, Cmd.batch [ Cmd.ofMsg LoadTodoList ]

    let private setStorage (model: Model) =
        Cmd.attemptFunc Storage.save model (string >> LoadingFailure)

    let update (msg : Msg) (model: Model) =
        match msg with
        | TodoListMsg msg ->
            let todoList, cmd = TodoList.State.update msg model.TodoList
            { model with TodoList = todoList }, Cmd.map TodoListMsg cmd
        | LoadingFailure err ->
            Fable.Import.Browser.console.error(err)
            model, Cmd.none
        | LoadTodoList ->
            let loadedModel =
                Storage.load ()
                |> Option.map (fun saved -> { model with TodoList = saved.TodoList })
            defaultArg loadedModel model, Cmd.none

    let updateWithStorage (msg: Msg) (model: Model) =
        match msg with
        | LoadingFailure _ -> model, Cmd.none
        | _ ->
            let model, cmd = update msg model
            model, Cmd.batch [ setStorage model; cmd ]

    let view (model: Model) (dispatch: Msg -> unit) =
        Section.section [ ]
            [ Columns.columns [ ]
                [ Column.column
                    [ Column.Width (Screen.All, Column.Is3)
                      Column.Offset (Screen.All, Column.Is2)
                    ]
                    [ Heading.h2 [ ]
                            [ str "Functional TodoMVC" ]
                      Heading.h3 [ Heading.IsSubtitle ]
                            [ str "The TodoMVC app written in F#, using Fable, Elmish and Fulma" ]
                    ]
                  Column.column
                    [ Column.Width (Screen.All, Column.Is4)
                      Column.Offset (Screen.All, Column.Is1)
                    ]
                    [ TodoList.View.view model.TodoList (TodoListMsg >> dispatch) ]
                ]
            ]

Program.mkProgram Page.init Page.updateWithStorage Page.view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
