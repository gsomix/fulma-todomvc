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
            if curModel.DescriptionField = ""
            then curModel, Cmd.none
            else
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
                |> Option.map (fun item -> if item.Description = ""
                                           then curModel.TodoItems.Remove item.Id
                                           else curModel.TodoItems.Add (item.Id, item))
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

    let pluralize count =
        if count = 1
        then sprintf "%d item" count
        else sprintf "%d items" count

    let viewItem (item: Todo) (isEditing: bool) (dispatch: Msg -> unit) =
        let control =
            if isEditing then
                Input.text [ Input.Size Size.IsSmall
                             Input.Value item.Description
                             Input.Props [ OnBlur (fun _ -> StopEditItem |> dispatch)
                                           OnChange (fun e -> e.Value |> UpdateEditingItem |> dispatch)
                                           OnKeyDown (fun e -> if e.which = ENTER_KEY then StopEditItem |> dispatch)
                                           AutoFocus true ] ]
            else
                Text.p
                    [ Props [ OnDoubleClick (fun _ -> StartEditItem item.Id |> dispatch) ]
                      Modifiers [ (if item.Status = TodoCompleted
                                   then Modifier.TextColor Color.IsGreyLight
                                   else Modifier.TextColor Color.IsBlack) ] ]
                    [ str item.Description ]

        Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
            [ Level.left [ ]
                [ Level.item [ ]
                    [ Checkradio.checkbox
                        [ Checkradio.Checked item.IsDone
                          Checkradio.OnChange (fun _ -> ToggleCompleted item.Id |> dispatch) ] [ ] ]
                  Level.item [ ]
                    [ control ] ]

              Level.right [ ]
                [ Delete.delete
                    [ Delete.Modifiers [ Modifier.IsPulledRight ]
                      Delete.OnClick (fun _ -> DeleteItem item.Id |> dispatch) ] [ ] ] ]

    let viewItems (model: Model) (dispatch: Msg -> unit) =
        [ for KeyValue(_, item) in model.TodoItems ->
            match model.EditingItem with
            | Some edtItem when edtItem.Id = item.Id ->
                Panel.block [ ] [ viewItem edtItem true dispatch ]
            | _ ->
                Panel.block [ ] [ viewItem item false dispatch ] ]

    let viewInput (placeholder: string) (value: string) (dispatch: Msg -> unit) =
        Input.text
            [ Input.Placeholder placeholder
              Input.Value value
              Input.Props
                [ OnChange (fun field -> DescriptionFieldInput field.Value |> dispatch)
                  OnKeyDown (fun e -> if e.which = ENTER_KEY then AddItem |> dispatch) ] ]

    let viewTab (tab: Filter) (activeTab: Filter) (dispatch: Msg -> unit) =
        Panel.tab
            [ Panel.Tab.IsActive (activeTab = tab)
              Panel.Tab.Props [ OnClick (fun _ -> ActivateFilter tab |> dispatch) ] ]
            [ str (sprintf "%A" tab) ]

    let viewTabs (tabs: List<Filter>) (activeTab: Filter) (dispatch: Msg -> unit) =
        [ for tab in tabs -> viewTab tab activeTab dispatch ]

    let view (model: Model) (dispatch: Msg -> unit) =
        let filtered = TodoList.filterItems model.ActiveFilter model.TodoItems
        Panel.panel [ ]
            [ yield Panel.heading [ ]
                [ str "Todos" ]

              yield Panel.block [ ]
                [ viewInput "What needs to be done?" model.DescriptionField dispatch ]

              yield Panel.tabs [ ]
                [ yield! viewTabs [All; Active; Completed] model.ActiveFilter dispatch ]

              yield! viewItems { model with TodoItems = filtered } dispatch

              yield Panel.block [ ]
                [ Level.level [ Level.Level.Props [ Style [ Flex "auto" ] ] ]
                    [ Level.left [ ]
                        [ Text.div [ ]
                            [ str (sprintf "%s left" (plurarize (model.TodoItems |> TodoList.countItems Active))) ] ]

                      Level.right [ ]
                        [ Button.button [ Button.Modifiers [ Modifier.IsPulledRight ]
                                          Button.Size IsSmall
                                          Button.Disabled (model.TodoItems |> TodoList.countItems Completed = 0)
                                          Button.OnClick (fun _ -> ClearCompleted |> dispatch) ]
                            [ str "Clear completed" ] ] ] ] ]