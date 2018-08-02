namespace TodoList

module Types =
    type Todo = {
        Id: int
        Description: string
        Completed: bool
    }

    module Todo =
        let Empty = {
            Id = 0
            Description = ""
            Completed = false
        }

    type Filter = | All | Active | Completed

    type Model = {
        TodoItems: Map<int, Todo>
        NewTodoDescription: string
        IdCounter: int
        ActiveFilter: Filter
    }

    type Msg =
        // Operations on todo list
        | AddTodo
        | DeleteTodo of int
        | ToggleCompleted of int
        | ClearCompleted

        // UI messages
        | EditNewTodoDescription of string
        | ActivateFilter of Filter

module State =
    open Elmish
    open Types

    let init () : Model * Cmd<Msg> =
        let initialModel =
            { TodoItems = Map.empty
              NewTodoDescription = ""
              IdCounter = 0
              ActiveFilter = All }
        initialModel, Cmd.none

    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | AddTodo ->
            let id = currentModel.IdCounter + 1 // TODO Move computation to Id type
            let item =
                { Todo.Empty with
                    Id = id
                    Description = currentModel.NewTodoDescription }
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.add id item
                    NewTodoDescription = ""
                    IdCounter = id }
            model, Cmd.none

        | DeleteTodo id ->
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.remove id }
            model, Cmd.none

        | ToggleCompleted id ->
            let item = Map.find id currentModel.TodoItems
            let item = { item with Completed = not item.Completed }
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.add id item }
            model, Cmd.none

        | ClearCompleted ->
            let item =
                currentModel.TodoItems |> Map.filter (fun _ v -> not v.Completed)
            let model =
                { currentModel with TodoItems = item }
            model, Cmd.none

        | EditNewTodoDescription str ->
            let model = { currentModel with NewTodoDescription = str }
            model, Cmd.none

        | ActivateFilter tab ->
            let model = { currentModel with ActiveFilter = tab }
            model, Cmd.none

module View =
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fable.Import.React

    open Fulma
    open Fulma.Extensions
    open Types

    let [<Literal>] ENTER_KEY = 13.

    let filterItems (items: Map<int, Todo>) (filter: Filter) =
        match filter with
        | All -> items
        | Active -> items |> Map.filter (fun _ v -> not v.Completed)
        | Completed -> items |> Map.filter (fun _ v -> v.Completed)

    let countActive (items: Map<int, Todo>) =
        items
        |> Map.filter (fun _ v -> not v.Completed)
        |> Map.count

    let renderItem (dispatch: Msg -> unit) (item: Todo) =
        Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
            [ Level.left [ ]
                [ Checkradio.checkbox [ Checkradio.Checked item.Completed
                                        Checkradio.OnChange (fun _ -> dispatch <| ToggleCompleted item.Id) ]
                    [ str item.Description ] ]

              Level.right [ ]
                [ Delete.delete [ Delete.Modifiers [ Modifier.IsPulledRight ]
                                  Delete.OnClick (fun _ -> dispatch <| DeleteTodo item.Id) ]
                    [ ] ] ]

    let renderItems (dispatch: Msg -> unit) (items: Map<int, Todo>) =
        [ for KeyValue(_, item) in items -> Panel.block [ ] [renderItem dispatch item] ]

    let renderInput (dispatch: Msg -> unit) (placeholder: string) (value: string) =
        Control.div [ ]
            [ Input.text
                [ Input.Placeholder placeholder
                  Input.Value value
                  Input.Props
                    [ OnInput (fun e -> dispatch <| EditNewTodoDescription e.Value)
                      OnKeyDown (fun e -> if e.which = ENTER_KEY then dispatch <| AddTodo) ] ] ]

    let renderTab (tab: 'a) (activeTab: 'a) (onClick: MouseEvent -> unit) =
        Panel.tab [ Panel.Tab.IsActive (activeTab = tab)
                    Panel.Tab.Props [ OnClick onClick ] ]
            [ str (sprintf "%A" tab) ]

    let renderTabs (tabs: List<'a>) (activeTab: 'a) (onClick: 'a -> MouseEvent -> unit) =
        [ for tab in tabs -> renderTab tab activeTab (onClick tab) ]

    let renderPanel (model: Model) (dispatch: Msg -> unit) =
        let filtered = filterItems model.TodoItems model.ActiveFilter
        let renderItems = renderItems dispatch
        let renderInput = renderInput dispatch

        Panel.panel [ ]
            [ yield Panel.heading [ ]
                [ str "Todos" ]

              yield Panel.block [ ]
                [ renderInput "What needs to be done?" model.NewTodoDescription ]

              yield Panel.tabs [ ]
                [ yield! renderTabs [All; Active; Completed] model.ActiveFilter
                            (fun filter _ -> dispatch <| ActivateFilter filter) ]

              yield! renderItems filtered

              yield Panel.block [ ]
                [ Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
                    [ Level.left [ ]
                        [ Text.div [ ] [ str (sprintf "%d items left" (countActive model.TodoItems)) ] ]

                      Level.right [ ]
                        [ Button.button [ Button.Modifiers [ Modifier.IsPulledRight ]
                                          Button.Size IsSmall
                                          Button.OnClick (fun _ -> dispatch <| ClearCompleted) ]
                            [ str "Clear completed" ] ] ] ] ]

    let view (model: Model) (dispatch: Msg -> unit) =
        Hero.hero [ ]
            [ Hero.body [ ]
                [ Columns.columns [ ]
                    [ Column.column [ Column.Width (Screen.All, Column.IsHalf)
                                      Column.Offset (Screen.All, Column.IsOneQuarter) ]
                        [ renderPanel model dispatch ] ] ] ]