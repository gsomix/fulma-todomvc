namespace Shared

type Counter = int

type Id = int // TODO Make single DU type or use UoM

type Todo = {
    Id: Id 
    Description: string
    Completed: bool 
}

module Todo =
    let Empty = 
        { Id = 0
          Description = ""
          Completed = false }

