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

@bs.scope("process") @bs.val
external argv: array<string> = "argv"

@bs.scope("Number") @bs.val
external parseInt: string => int = "parseInt"

let pendingTodosFile = "todo.txt"
let completedTodosFile = "done.txt"

let helpString = "Usage :-
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
  let updatedContent = updaterFn(contents)
  writeFile(filename, updatedContent)
}

let cmdHelp = () => {
  Js.Console.log(helpString)
}

let cmdLs = () => {
  let todos = readFile(pendingTodosFile)
  if Js.Array2.length(todos) == 0 {
    Js.Console.log("There are no pending todos!")
  } else {
    let length = Js.Array2.length(todos)
    let formattedTodos =
      todos
      ->Belt.Array.reverse
      ->Belt.Array.reduceWithIndex("", (acc, todo, index) => {
        let todoIndex = length - index
        acc ++ j`[$todoIndex] $todo\n`
      })
    Js.Console.log(formattedTodos)
  }
}

let cmdAddTodo = text => {
  let cmdStatus = text->Belt.Option.mapWithDefault(
    "Error: Missing todo string. Nothing added!",
    x => {
      updateFile(pendingTodosFile, todos => Js.Array2.concat(todos, [x]))
      `Added todo: "${x}"`
    },
  )
  Js.Console.log(cmdStatus)
}

let cmdDelTodo = number => {
  let cmdStatus = number->Belt.Option.mapWithDefault(
    "Error: Missing NUMBER for deleting todo.",
    x => {
      let index = parseInt(x)
      let todos = readFile(pendingTodosFile)
      if index < 1 || index > Js.Array2.length(todos) {
        j`Error: todo #$index does not exist. Nothing deleted.`
      } else {
        updateFile(pendingTodosFile, todos => {
          let _ = Js.Array2.spliceInPlace(todos, ~pos=index, ~remove=1, ~add=[])
          todos
        })
        j`Deleted todo #$number`
      }
    },
  )
  Js.Console.log(cmdStatus)
}

let cmdMarkDone = number => {
  let cmdStatus =
    number->Belt.Option.mapWithDefault("Error: Missing NUMBER for marking todo as done.", x => {
      let index = parseInt(x)
      let todos = readFile(pendingTodosFile)
      if index < 1 || index > Js.Array2.length(todos) {
        j`Error: todo #$number does not exist.`
      } else {
        let completedTodo = Js.Array2.spliceInPlace(todos, ~pos=index - 1, ~remove=1, ~add=[])
        writeFile(pendingTodosFile, todos)
        let completedTodoStr = `x ${getToday()} ${completedTodo[0]}\n`
        appendToFile(completedTodosFile, completedTodoStr)
        j`Marked todo #$index as done.`
      }
    })
  Js.Console.log(cmdStatus)
}

let cmdReport = () => {
  let pending = Js.Array2.length(readFile(pendingTodosFile))
  let completed = Js.Array2.length(readFile(completedTodosFile)) - 1
  Js.Console.log(
    `${getToday()} Pending : ${Belt.Int.toString(pending)} Completed : ${Belt.Int.toString(
        completed,
      )}`,
  )
}

let command = argv->Belt.Array.get(2)
let arg = argv->Belt.Array.get(3)

switch command {
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
