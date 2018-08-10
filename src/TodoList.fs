namespace TodoList

module rec Domain =
    type Id = int

    type Todo =
        { Id: Id
          Description: string
          Status: Todo.Status }

    type Todo with
        static member Empty =
            { Id = 0
              Description = ""
              Status = Todo.Active }

    module Todo =
        type Status =
            | Active
            | Completed

        type Status with
            member this.Toggle =
                match this with
                | Active -> Completed
                | Completed -> Active

        let isDone todo = todo.Status = Todo.Completed

module Types =
    open Domain

    type Filter = All | Active | Completed
    type TodoList = Map<Id, Todo>

    module Map =
        let filterValues predicate table =
            table |> Map.filter (fun _ v -> predicate v)

    module TodoList =
        let filterEntries (filter: Filter) (entries: TodoList) =
            match filter with
            | All -> entries
            | Active -> entries |> Map.filterValues (Todo.isDone >> not)
            | Completed -> entries |> Map.filterValues Todo.isDone

        let countEntries (filter: Filter) (entries: TodoList) =
            entries |> filterEntries filter |> Map.count

    type Model =
        { Entries: TodoList
          Counter: Id
          Field: string
          Filter: Filter
          EditingEntry: Todo option
        }

    type Msg =
        // Operations on todo list
        | Add
        | Delete of Id
        | DeleteComplete
        | Check of Id

        // Entry editing
        | StartEditEntry of Id
        | StopEditEntry
        | UpdateEditingEntry of string

        // Other UI messages
        | UpdateField of string
        | ChangeFilter of Filter

module State =
    open Elmish
    open Domain
    open Types

    let init () =
        { Entries = Map.empty
          Counter = 0
          Field = ""
          Filter = All
          EditingEntry = None
        }, Cmd.none

    let update (msg : Msg) (model : Model) =
        match msg with
        | Add ->
            if model.Field = ""
            then model, Cmd.none
            else
                let id = model.Counter + 1
                let entry = { Todo.Empty with Id = id; Description = model.Field }
                { model with
                    Entries = model.Entries |> Map.add id entry
                    Field = ""
                    Counter = id
                }, Cmd.none

        | Delete id ->
            { model with Entries = model.Entries.Remove id }, Cmd.none

        | DeleteComplete ->
            let entries = model.Entries |> TodoList.filterEntries Active
            { model with Entries = entries }, Cmd.none

        | Check id ->
            let entries =
                model.Entries
                |> Map.tryFind id
                |> Option.map (fun entry ->
                    let entry = { entry with Status = entry.Status.Toggle }
                    model.Entries.Add (id, entry))
            { model with Entries = defaultArg entries model.Entries }, Cmd.none

        | StartEditEntry id ->
            { model with EditingEntry = model.Entries.TryFind id }, Cmd.none

        | UpdateEditingEntry str ->
            let entry =
                model.EditingEntry
                |> Option.map (fun entry -> { entry with Description = str })
            { model with EditingEntry = entry }, Cmd.none

        | StopEditEntry ->
            let entries =
                model.EditingEntry
                |> Option.map (fun entry -> if entry.Description = ""
                                            then model.Entries.Remove entry.Id
                                            else model.Entries.Add (entry.Id, entry))
            { model with
                Entries = defaultArg entries model.Entries
                EditingEntry = None }, Cmd.none

        | UpdateField str ->
            { model with Field = str }, Cmd.none

        | ChangeFilter tab ->
            { model with Filter = tab }, Cmd.none

module View =
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fulma
    open Fulma.Extensions

    open Domain
    open Types

    let [<Literal>] ENTER_KEY = 13.

    let view (model: Model) (dispatch: Msg -> unit) =
        let viewEntry entry isEditing =
            let color status =
                if status = Todo.Completed
                then Color.IsGreyLight
                else Color.IsBlack

            let input =
                Input.text
                    [ Input.Size Size.IsSmall
                      Input.Value entry.Description
                      Input.Props
                        [ OnBlur    (fun _ -> StopEditEntry |> dispatch)
                          OnChange  (fun e -> e.Value |> UpdateEditingEntry |> dispatch)
                          OnKeyDown (fun e -> if e.which = ENTER_KEY then StopEditEntry |> dispatch)
                          AutoFocus true
                        ]
                    ]

            let description =
                Text.p
                    [ Modifiers [ Modifier.TextColor (color entry.Status) ] ]
                    [ str entry.Description ]

            Columns.columns
                [ Columns.IsMobile
                  Columns.IsVCentered
                  Columns.Props [ Style [ Flex "auto" ] ]
                ]
                [ Column.column
                    [ Column.Width (Screen.All, Column.IsNarrow) ]
                    [ Checkradio.checkbox
                        [ Checkradio.Checked (Todo.isDone entry)
                          Checkradio.OnChange (fun _ -> Check entry.Id |> dispatch)
                        ]
                        [ ]
                    ]
                  Column.column
                    [ Column.Props [ OnDoubleClick (fun _ -> StartEditEntry entry.Id |> dispatch) ] ]
                    [ (if isEditing then input else description) ]
                  Column.column
                    [ Column.Width (Screen.All, Column.IsNarrow) ]
                    [ Delete.delete
                        [ Delete.OnClick (fun _ -> Delete entry.Id |> dispatch) ]
                        [ ]
                    ]
                ]

        let viewEntries entries editingEntry =
            [ for KeyValue(_, entry) in entries ->
                match editingEntry with
                | Some editingEntry when editingEntry.Id = entry.Id ->
                    Panel.block [ ] [ viewEntry editingEntry true ]
                | _ ->
                    Panel.block [ ] [ viewEntry entry false ]
            ]

        let viewInput placeholder value =
            Panel.block [ ]
                [ Input.text
                    [ Input.Placeholder placeholder
                      Input.Value value
                      Input.Props
                        [ OnChange  (fun field -> UpdateField field.Value |> dispatch)
                          OnKeyDown (fun key -> if key.which = ENTER_KEY then Add |> dispatch)
                        ]
                    ]
                ]

        let viewTab tab activeTab =
            Panel.tab
                [ Panel.Tab.IsActive (activeTab = tab)
                  Panel.Tab.Props [ OnClick (fun _ -> ChangeFilter tab |> dispatch) ]
                ]
                [ str (sprintf "%A" tab) ]

        let viewTabs tabs activeTab =
            Panel.tabs [ ] [ for tab in tabs -> viewTab tab activeTab ]

        let viewControls =
            let pluralize count =
                sprintf "%d %s" count (if count = 1 then "item" else "items")

            let activeEntries = model.Entries |> TodoList.countEntries Active
            let completedEntries = model.Entries |> TodoList.countEntries Completed

            Panel.block [ ]
                [ Level.level
                    [ Level.Level.Props [ Style [ Flex "auto" ] ]
                      Level.Level.IsMobile
                    ]
                    [ Level.left [ ]
                        [ Text.p [ ] [ str (sprintf "%s left" (pluralize activeEntries)) ] ]
                      Level.right [ ]
                        [ Button.button
                            [ Button.Modifiers [ Modifier.IsPulledRight ]
                              Button.Size IsSmall
                              Button.Disabled (completedEntries = 0)
                              Button.OnClick (fun _ -> DeleteComplete |> dispatch)
                            ]
                            [ str "Clear completed" ]
                        ]
                    ]
                ]

        let viewHeading title = Panel.heading [ ] [ str title ]

        let filtered = model.Entries |> TodoList.filterEntries model.Filter

        Panel.panel [ ]
            [ yield  viewHeading "Todos"
              yield  viewInput "What needs to be done?" model.Field
              yield  viewTabs [All; Active; Completed] model.Filter
              yield! viewEntries filtered model.EditingEntry
              yield  viewControls
            ]