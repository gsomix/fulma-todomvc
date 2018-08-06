namespace TodoList

module Domain =
    type Id = int

    type TodoStatus =
        | TodoActive
        | TodoCompleted
        with
        member this.Swap =
            match this with
            | TodoActive -> TodoCompleted
            | TodoCompleted -> TodoActive

    type Todo = {
        Id: Id
        Description: string
        Status: TodoStatus
    } with
        member this.IsDone = this.Status = TodoCompleted
        static member Empty = {
            Id = 0
            Description = ""
            Status = TodoActive
        }

module Types =
    open Domain

    type Filter = | All | Active | Completed
    type TodoList = Map<Id, Todo>

    module TodoList =
        let filterItems (filter: Filter) (items: TodoList) =
            match filter with
            | All -> items
            | Active -> items |> Map.filter (fun _ item -> not item.IsDone)
            | Completed -> items |> Map.filter (fun _ item -> item.IsDone)

        let countItems (filter: Filter) (items: TodoList) =
            items
            |> filterItems filter
            |> Map.count

    type Model = {
        TodoItems: TodoList
        DescriptionField: string
        IdCounter: Id
        ActiveFilter: Filter
        EditingItem: Todo option
    }

    type Msg =
        // Operations on todo list
        | AddItem
        | DeleteItem of Id
        | StartEditItem of Id
        | StopEditItem
        | UpdateEditingItem of string
        | ToggleCompleted of Id
        | ClearCompleted

        // UI messages
        | DescriptionFieldInput of string
        | ActivateFilter of Filter

module State =
    open Elmish
    open Domain
    open Types

    let init () : Model * Cmd<Msg> =
        let initialModel =
            { TodoItems = Map.empty
              DescriptionField = ""
              IdCounter = 0
              ActiveFilter = All
              EditingItem = None }
        initialModel, Cmd.none

    let update (msg : Msg) (curModel : Model) : Model * Cmd<Msg> =
        match msg with
        | AddItem ->
            let id = curModel.IdCounter + 1
            let item = { Todo.Empty with Id = id; Description = curModel.DescriptionField }

            { curModel with
                TodoItems = curModel.TodoItems |> Map.add id item
                DescriptionField = ""
                IdCounter = id }, Cmd.none

        | DeleteItem id ->
            { curModel with TodoItems = curModel.TodoItems.Remove id }, Cmd.none

        | StartEditItem id ->
            { curModel with EditingItem = curModel.TodoItems.TryFind id }, Cmd.none

        | UpdateEditingItem str ->
            let item =
                curModel.EditingItem
                |> Option.map (fun item -> { item with Description = str })
            { curModel with EditingItem = item }, Cmd.none

        | StopEditItem ->
            let items =
                curModel.EditingItem
                |> Option.map (fun item -> curModel.TodoItems.Add (item.Id, item))
            { curModel with
                TodoItems = defaultArg items curModel.TodoItems
                EditingItem = None }, Cmd.none

        | ToggleCompleted id ->
            let items =
                curModel.TodoItems.TryFind id
                |> Option.map (fun item -> { item with Status = item.Status.Swap })
                |> Option.map (fun item -> curModel.TodoItems.Add (id, item))
            { curModel with
                TodoItems = defaultArg items curModel.TodoItems}, Cmd.none

        | ClearCompleted ->
            let item = curModel.TodoItems |> Map.filter (fun _ item -> not item.IsDone)
            { curModel with TodoItems = item }, Cmd.none

        | DescriptionFieldInput str ->
            { curModel with DescriptionField = str }, Cmd.none

        | ActivateFilter tab ->
            { curModel with ActiveFilter = tab }, Cmd.none

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
                             Input.Props [ OnBlur (fun _ -> StopEditItem |> dispatch)
                                           OnInput (fun e -> e.Value |> UpdateEditingItem |> dispatch)
                                           OnKeyDown (fun e -> if e.which = ENTER_KEY then StopEditItem |> dispatch)
                                           AutoFocus true ] ]
            else
                Text.p [ Props [ OnDoubleClick (fun _ -> StartEditItem item.Id |> dispatch) ] ] [ str item.Description ]

        Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
            [ Level.left [ ]
                [ Level.item [ ]
                        [ Checkradio.checkbox [ Checkradio.Checked item.IsDone
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

        let onInputHandler = fun str -> dispatch (DescriptionFieldInput str)
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
                [ viewInput "What needs to be done?" model.DescriptionField ]

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