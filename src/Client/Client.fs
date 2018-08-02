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
    TodoItems: Map<Id, Todo> 
    NewTodoDescription: string
    LastId: Id
}

type Msg =
    | AddTodo
    | DeleteTodo of Id
    | EditNewTodoDescription of string
    | ToggleCompleted of Id

let init () : Model * Cmd<Msg> =
    let initialModel = 
        { TodoItems = Map.empty
          NewTodoDescription = "" 
          LastId = 0 }
    initialModel, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | EditNewTodoDescription str ->
        let newModel = { currentModel with NewTodoDescription = str }        
        newModel, Cmd.none
        
    | AddTodo -> 
        let newId = currentModel.LastId + 1 // TODO Move computation to Id type 
        let newItem = 
            { Todo.Empty with
                Id = newId 
                Description = currentModel.NewTodoDescription }
        let newModel =
            { currentModel with 
                TodoItems = currentModel.TodoItems |> Map.add newId newItem
                NewTodoDescription = ""
                LastId = newId }
        newModel , Cmd.none

    | ToggleCompleted id -> 
        let item = Map.find id currentModel.TodoItems
        let newItem = { item with Completed = not item.Completed }
        let newModel = 
            { currentModel with 
                TodoItems = currentModel.TodoItems |> Map.add id newItem }
        newModel, Cmd.none

    | DeleteTodo id -> 
        let newModel = 
            { currentModel with 
                TodoItems = currentModel.TodoItems |> Map.remove id }
        newModel, Cmd.none
        

let showItems (items: Map<Id, Todo>) (dispatch: Msg -> unit) =
    [ for KeyValue(id, item) in items -> 
        Panel.block [ ] 
            [ Level.level [ ]
                [ Level.left [ ] 
                    [ Level.item [ ] 
                        [ Checkradio.checkbox // TODO Fix styling
                            [ Checkradio.Checked item.Completed 
                              Checkradio.OnChange (fun _ -> dispatch <| ToggleCompleted id)] 
                            [ str item.Description ] ] ]
                  Level.right [ ] 
                    [ Level.item [ ] 
                        [ Delete.delete 
                            [ Delete.OnClick (fun _ -> dispatch <| DeleteTodo id) ] [ ] ] ]
                ] ] ]

let countActive (items: Map<Id, Todo>) =
    items
    |> Map.filter (fun _ v -> not v.Completed)
    |> Map.count

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
                  yield! showItems model.TodoItems dispatch
                  yield Panel.block [ ] [ str (sprintf "%d items left" (countActive model.TodoItems)) ] ] ] ]


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
