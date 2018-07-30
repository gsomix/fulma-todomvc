module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared

open Fulma
open Fulma.Extensions

let [<Literal>] ENTER_KEY = 13.

type Model = { 
    TodoItems: list<Todo> 
    NewTodoDescription: string
}

type Msg =
    | AddTodo
    | EditNewTodoDescription of string

let init () : Model * Cmd<Msg> =
    let initialModel = 
        { TodoItems = []
          NewTodoDescription = "" }

    initialModel, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | EditNewTodoDescription str ->
        let newModel = { currentModel with NewTodoDescription = str }
        newModel, Cmd.none
        
    | AddTodo -> 
        let newItem = 
            { Description = currentModel.NewTodoDescription
              Completed = false }

        let newModel =
            { currentModel with 
                TodoItems = newItem :: currentModel.TodoItems
                NewTodoDescription = "" }

        newModel , Cmd.none

let showItems (items: list<Todo>) =
    [ for item in items -> 
        Panel.block [ ] 
            [ Checkradio.checkbox [ Checkradio.Checked item.Completed ] [ str item.Description ] ] ]

let view (model: Model) (dispatch: Msg -> unit) =
    Columns.columns [ ]
        [ Column.column [ Column.Offset (Screen.All, Column.Is3)
                          Column.Width  (Screen.All, Column.Is6) ] 
            [ Panel.panel [ ] 
                [ yield Panel.heading [ ] [ str "todos" ] 
                  yield Panel.block [ ] 
                    [ Control.div [ ] 
                        [ Input.text 
                            [ Input.Placeholder "What needs to be done?"
                              Input.Value model.NewTodoDescription 
                              Input.Props 
                                [ OnInput (fun e -> dispatch <| EditNewTodoDescription e.Value) 
                                  OnKeyDown (fun e -> if e.which = ENTER_KEY then dispatch <| AddTodo) ] ] ] ] 
                  yield! showItems (model.TodoItems)
                  yield Panel.block [ ] [ str (sprintf "%d items left" model.TodoItems.Length) ] ] ] ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
