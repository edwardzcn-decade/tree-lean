import TreeLean.Config
open System



def printUsage : IO Unit := do
  IO.println (fancy "Usage: treelean [-a] [--ascii] [--help] [--dir <path>] [--version]\nOptions:\n\t--ascii\tUse ASCII characters to display the directory structure")

def printVersion : IO Unit := do
  IO.println (fancy "treelean 0.1.0")

def configFromArgs : List String → IO (Option Config)
  | [] => pure none
  | args =>
    if (args.contains "--help" || args.contains "-h") then do
      -- printUsage >>= fun () => -- a little weird, but it works the same as `do print Usage`
      printUsage
      pure none
    else if (args.contains "--version" || args.contains "-v") then do
      printVersion
      pure none
    else
      let ascii := args.contains "--ascii"
      let fancy := args.contains "--fancy"
      let argslen := args.length
      let id_path := args.indexOf "--dir" + 1
      let rootpath := if argslen > 0  && id_path < argslen then args[id_path]! else "/"
      pure (some { ascii := ascii, fancy := fancy, rootpath := rootpath })



inductive Entry where
  | file : String → Entry
  | dir : String → Entry

def toEntry (path : FilePath) : IO (Option Entry) := do
  match path.components.getLast? with
  | none => pure none
  | some "." | some ".." => pure none
  | some name =>
    if (← path.isDir) then
      pure (some (Entry.dir name))
    else
      pure (some (Entry.file name))


def doList [Applicative f] : List α → (α → f Unit) → f Unit
  | [], _ => pure ()
  | x :: xs, next => next x *> doList xs next
-- Lean's logic has no way to know that directory trees are finite. Indeed, some systems allow the construction of circular directory structures. Thus, dirTree is declared partial:
partial def dirTree (scfg: ShowConfig) (path : FilePath) (desc_layer : Nat): IO Unit := do
  match (← toEntry path) with
  | none => pure ()
  | some (.file filename) =>
    showFileName scfg filename
  | some (.dir dirname) =>
    showDirName scfg dirname
    -- unfold with desc_layer
    -- or
    match desc_layer with
    | 0 => pure ()
    | n + 1 => do
      let contents ← path.readDir
      let newConfig := scfg.inDirectory -- step in change prefix
      doList contents.toList fun d => -- recursive call visit all children dir
        dirTree newConfig d.path n






def main (args : List String) : IO UInt32 := do
  -- Not using IO.Process.spawn to wait
  let config ← configFromArgs args
  match config with
  | none =>
    IO.eprintln s!"# Didn't understand arguments(s) {args}"
    IO.eprintln s!"# Read Usage: treelean --help"
    pure 1
  | some config =>
    let showConfig := ShowConfig.fromConfig config
    println! s!"root path: {config.rootpath}"
    dirTree showConfig config.rootpath 1 *>
    pure 0
