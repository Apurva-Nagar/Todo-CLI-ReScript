type fsConfig = {encoding: string, flag: string}

/* https://nodejs.org/api/fs.html#fs_fs_existssync_path */
@bs.module("fs") external existsSync: string => bool = "existsSync"

/* https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options */
@bs.module("fs")
external readFileSync: (string, fsConfig) => string = "readFileSync"

/* https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options */
@bs.module("fs")
external appendFileSync: (string, string, fsConfig) => unit = "appendFileSync"

@bs.module("fs")
external writeFileSync: (string, string, fsConfig) => unit = "writeFileSync"

/* https://nodejs.org/api/os.html#os_os_eol */
@bs.module("os") external eol: string = "EOL"

let encoding = "utf8"

/* Returns date with the format: 2021-02-04 */
/* Given helper function */
let getToday: unit => string = %raw(`
function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
}
  `)

///////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// My Code //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
@bs.module("process")
external argv: array<string> = "argv"

@bs.scope("Number") @bs.val
external parseInt: string => int = "parseInt"

let pending_todos_file = "todo.txt"
let completed_todos_file = "done.txt"

let help_string = "Usage :-
$ ./todo add \"todo item\"  # Add a new todo
$ ./todo ls               # Show remaining todos
$ ./todo del NUMBER       # Delete a todo
$ ./todo done NUMBER      # Complete a todo
$ ./todo help             # Show usage
$ ./todo report           # Statistics"

let readFile = filename => {
  if !existsSync(filename) {
    []
  } else {
    let text = readFileSync(filename, {encoding: encoding, flag: "r"})
    let lines = Js.String2.split(text, "\n")
    lines
  }
}

let appendToFile = (filename, text) => {
  appendFileSync(filename, text, {encoding: encoding, flag: "a+"})
}

let writeFile = (filename, lines) => {
  let text = Js.Array2.joinWith(lines, eol)
  writeFileSync(filename, text, {encoding: encoding, flag: "w"})
}

let updateFile = (filename, updaterFn) => {
  let contents = readFile(filename)
  let updated_content = updaterFn(contents)
  writeFile(filename, updated_content)
}

let cmdHelp = () => {
  Js.Console.log(help_string)
}

let cmdLs = () => {
  let todos = readFile(pending_todos_file)
  if Js.Array2.length(todos) == 0 {
    Js.Console.log("There are no pending todos!")
  } else {
    let length = Js.Array2.length(todos)
    let formatted_todos = Js.Array2.mapi(Belt.Array.reverse(todos), (todo, index) => {
      let todo_index = length - index
      j`[$todo_index] $todo`
    })
    Js.Console.log(Js.Array2.joinWith(formatted_todos, eol))
  }
}

let cmdAddTodo = text => {
  switch Js.Nullable.toOption(text) {
  | Some(x) => {
      updateFile(pending_todos_file, todos => Js.Array2.concat(todos, [x]))
      Js.Console.log(`Added todo: "${x}"`)
    }
  | None => Js.Console.log("Error: Missing todo string. Nothing added!")
  }
}

let cmdDelTodo = number => {
  switch Js.Nullable.toOption(number) {
  | Some(x) => {
      let index = parseInt(x)
      updateFile(pending_todos_file, todos => {
        if index < 1 || index > Js.Array2.length(todos) {
          Js.Console.log(j`Error: todo #$index does not exist. Nothing deleted.`)
        } else {
          let _ = Js.Array2.spliceInPlace(todos, ~pos=index, ~remove=1, ~add=[])
          Js.Console.log(j`Deleted todo #$number`)
        }
        todos
      })
    }
  | None => Js.Console.log("Error: Missing NUMBER for deleting todo.")
  }
}

let cmdMarkDone = number => {
  switch Js.Nullable.toOption(number) {
  | Some(x) => {
      let index = parseInt(x)
      let todos = readFile(pending_todos_file)
      if index < 1 || index > Js.Array2.length(todos) {
        Js.Console.log(j`Error: todo #$number does not exist.`)
      } else {
        let completedTodo = Js.Array2.spliceInPlace(todos, ~pos=index - 1, ~remove=1, ~add=[])
        writeFile(pending_todos_file, todos)
        let completedTodoStr = `x ${getToday()} ${completedTodo[0]}\n`
        appendToFile(completed_todos_file, completedTodoStr)
        Js.Console.log(j`Marked todo #$index as done.`)
      }
    }
  | None => Js.Console.log("Error: Missing NUMBER for marking todo as done.")
  }
}

let cmdReport = () => {
  let pending = Js.Array2.length(readFile(pending_todos_file))
  let completed = Js.Array2.length(readFile(completed_todos_file)) - 1
  Js.Console.log(
    `${getToday()} Pending : ${Belt.Int.toString(pending)} Completed : ${Belt.Int.toString(
        completed,
      )}`,
  )
}

let args = argv

let command = if Js.Array2.length(args) >= 3 {
  Js.Nullable.return(args[2])
} else {
  Js.Nullable.null
}

let arg = if Js.Array2.length(args) >= 4 {
  Js.Nullable.return(args[3])
} else {
  Js.Nullable.null
}

switch Js.Nullable.toOption(command) {
| Some(x) =>
  switch x {
  | "help" => cmdHelp()
  | "ls" => cmdLs()
  | "add" => cmdAddTodo(arg)
  | "del" => cmdDelTodo(arg)
  | "done" => cmdMarkDone(arg)
  | "report" => cmdReport()
  | _ => cmdHelp()
  }
| None => cmdHelp()
}
