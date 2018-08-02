module App.View

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR


Program.mkProgram TodoList.State.init TodoList.State.update TodoList.View.view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
