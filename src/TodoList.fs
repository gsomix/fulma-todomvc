namespace TodoList

module Domain =
    type Id = int

    type TodoStatus =
        | TodoActive
        | TodoCompleted

    type Todo = {
        Id: int
        Description: string
        Status: TodoStatus
    }

    module Todo =
        let Empty = {
            Id = 0
            Description = ""
            Status = TodoActive
        }

        let isDone todo =
            todo.Status = TodoCompleted

module Types =
    open Domain

    type Filter = | All | Active | Completed

    type TodoList = Map<Id, Todo>

    module TodoList =
        let filterItems (filter: Filter) (items: TodoList) =
            match filter with
            | All -> items
            | Active -> items |> Map.filter (fun _ v -> not (Todo.isDone v))
            | Completed -> items |> Map.filter (fun _ v -> Todo.isDone v)

        let countItems (filter: Filter) (items: TodoList) =
            items
            |> filterItems filter
            |> Map.count

    type Model = {
        TodoItems: TodoList
        NewTodoDescription: string
        IdCounter: Id
        ActiveFilter: Filter
        EditingItem: Todo option
    }

    type Msg =
        // Operations on todo list
        | AddItem
        | DeleteItem of Id
        | StartEditItem of Id
        | StopEditItem of Id
        | UpdateItem of Id * string
        | ToggleCompleted of Id
        | ClearCompleted

        // UI messages
        | EditNewTodoDescription of string
        | ActivateFilter of Filter

module State =
    open Elmish
    open Domain
    open Types

    let init () : Model * Cmd<Msg> =
        let initialModel =
            { TodoItems = Map.empty
              NewTodoDescription = ""
              IdCounter = 0
              ActiveFilter = All
              EditingItem = None }
        initialModel, Cmd.none

    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | AddItem ->
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

        | DeleteItem id ->
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.remove id }
            model, Cmd.none

        | StartEditItem id ->
            let model =
                { currentModel with
                    EditingItem = Map.tryFind id currentModel.TodoItems }
            model, Cmd.none

        | UpdateItem (_, str) ->
            let item = currentModel.EditingItem |> Option.map (fun item -> { item with Description = str })
            let model = { currentModel with EditingItem = item }
            model, Cmd.none

        | StopEditItem id ->
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.add id currentModel.EditingItem.Value
                    EditingItem = None }
            model, Cmd.none


        | ToggleCompleted id ->
            let item = Map.find id currentModel.TodoItems
            let item = { item with Status = if Todo.isDone item then TodoActive else TodoCompleted }
            let model =
                { currentModel with
                    TodoItems = currentModel.TodoItems |> Map.add id item }
            model, Cmd.none

        | ClearCompleted ->
            let item =
                currentModel.TodoItems |> Map.filter (fun _ v -> not (Todo.isDone v))
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

    open Fulma
    open Fulma.Extensions
    open Domain
    open Types

    let [<Literal>] ENTER_KEY = 13.

    let viewItem (item: Todo)
                 (isEditing: bool)
                 (dispatch: Msg -> unit) =

        let control =
            if isEditing then
                Input.text [ Input.Size Size.IsSmall
                             Input.Value item.Description
                             Input.Props [ OnBlur (fun _ -> StopEditItem item.Id |> dispatch)
                                           OnInput (fun e -> (item.Id, e.Value) |> UpdateItem |> dispatch)
                                           OnKeyDown (fun e -> if e.which = ENTER_KEY then StopEditItem item.Id |> dispatch)
                                           AutoFocus true ] ]
            else
                Text.p [ Props [ OnDoubleClick (fun _ -> StartEditItem item.Id |> dispatch) ] ] [ str item.Description ]

        Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
            [ Level.left [ ]
                [ Level.item [ ]
                        [ Checkradio.checkbox [ Checkradio.Checked (Todo.isDone item)
                                                Checkradio.OnChange (fun _ -> ToggleCompleted item.Id |> dispatch) ] [ ] ]
                  Level.item [ ] [ control ] ]

              Level.right [ ]
                [ Delete.delete [ Delete.Modifiers [ Modifier.IsPulledRight ]
                                  Delete.OnClick (fun _ -> DeleteItem item.Id |> dispatch) ]
                    [ ] ] ]

    let viewItems (model: Model) (dispatch: Msg -> unit) =
        let items = model.TodoItems
        [ for KeyValue(_, item) in items ->
            let isEditing =
                if model.EditingItem.IsNone then false
                else item.Id = model.EditingItem.Value.Id
            let item = if isEditing then
                            model.EditingItem.Value
                       else
                            item

            Panel.block [ ] [ viewItem item isEditing dispatch ] ]

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

        let viewItems model =
            viewItems { model with TodoItems = filtered } dispatch

        let onInputHandler = fun str -> dispatch (EditNewTodoDescription str)
        let onEnterHandler = fun () -> dispatch (AddItem)
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

              yield! viewItems model

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