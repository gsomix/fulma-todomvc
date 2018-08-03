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

    type Id = int

    type TodoList = Map<Id, Todo>

    module TodoList =
        let filterItems (filter: Filter) (items: TodoList) =
            match filter with
            | All -> items
            | Active -> items |> Map.filter (fun _ v -> not v.Completed)
            | Completed -> items |> Map.filter (fun _ v -> v.Completed)

        let countItems (filter: Filter) (items: TodoList) =
            items
            |> filterItems filter
            |> Map.count

    type Model = {
        TodoItems: TodoList
        NewTodoDescription: string
        IdCounter: Id
        ActiveFilter: Filter
    }

    type Msg =
        // Operations on todo list
        | AddTodo
        | DeleteTodo of Id
        | ToggleCompleted of Id
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

    let viewItem (item: Todo)
                 (onToggle: Id -> unit)
                 (onDelete: Id -> unit) =
        Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
            [ Level.left [ ]
                [ Checkradio.checkbox [ Checkradio.Checked item.Completed
                                        Checkradio.OnChange (fun _ -> onToggle item.Id) ]
                    [ str item.Description ] ]

              Level.right [ ]
                [ Delete.delete [ Delete.Modifiers [ Modifier.IsPulledRight ]
                                  Delete.OnClick (fun _ -> onDelete item.Id) ]
                    [ ] ] ]

    let viewItems (items: TodoList)
                  (onToggle: Id -> unit)
                  (onDelete: Id -> unit) =
        [ for KeyValue(_, item) in items ->
            Panel.block [ ] [ viewItem item onToggle onDelete ] ]

    let viewInput (placeholder: string)
                  (value: string)
                  (onInput: string -> unit)
                  (onEnter: unit -> unit) =
        Control.div [ ]
            [ Input.text
                [ Input.Placeholder placeholder
                  Input.Value value
                  Input.Props
                    [ OnInput (fun e -> onInput e.Value)
                      OnKeyDown (fun e -> if e.which = ENTER_KEY then onEnter ()) ] ] ]

    let viewTab (tab: 'a) (activeTab: 'a) (onClick: 'a -> unit) =
        Panel.tab [ Panel.Tab.IsActive (activeTab = tab)
                    Panel.Tab.Props [ OnClick (fun _ -> onClick tab) ] ]
            [ str (sprintf "%A" tab) ]

    let viewTabs (tabs: List<'a>) (activeTab: 'a) (onClick: 'a -> unit) =
        [ for tab in tabs -> viewTab tab activeTab onClick ]

    let view (model: Model) (dispatch: Msg -> unit) =
        let filtered = TodoList.filterItems model.ActiveFilter model.TodoItems

        let onItemToggleHandler = fun id -> dispatch (ToggleCompleted id)
        let onItemDeleteHandler = fun id -> dispatch (DeleteTodo id)
        let viewItems items =
            viewItems items onItemToggleHandler onItemDeleteHandler

        let onInputHandler = fun str -> dispatch (EditNewTodoDescription str)
        let onEnterHandler = fun () -> dispatch (AddTodo)
        let viewInput placeholder descr =
            viewInput placeholder descr onInputHandler onEnterHandler

        let onTabClickHandler = fun filter -> dispatch (ActivateFilter filter)
        let viewTabs tabs activeTab =
            viewTabs tabs activeTab onTabClickHandler

        Panel.panel [ ]
            [ yield Panel.heading [ ]
                [ str "Todos" ]

              yield Panel.block [ ]
                [ viewInput "What needs to be done?" model.NewTodoDescription ]

              yield Panel.tabs [ ]
                [ yield! viewTabs [All; Active; Completed] model.ActiveFilter ]

              yield! viewItems filtered

              yield Panel.block [ ]
                [ Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
                    [ Level.left [ ]
                        [ Text.div [ ]
                            [ str (sprintf "%d items left" (TodoList.countItems Active model.TodoItems)) ] ]

                      Level.right [ ]
                        [ Button.button [ Button.Modifiers [ Modifier.IsPulledRight ]
                                          Button.Size IsSmall
                                          Button.OnClick (fun _ -> dispatch <| ClearCompleted) ]
                            [ str "Clear completed" ] ] ] ] ]