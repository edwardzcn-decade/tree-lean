
structure Config where
  ascii : Bool
  fancy : Bool
  rootpath  : String
deriving Repr

instance : ToString (Option Config) where
  toString
    | some config => s!"Config(ascii: {config.ascii}, fancy: {config.fancy})"
    | none => "No Config"





-- TODO: Implement fancy to show colors
abbrev Color := String
abbrev Font := String
structure FancyConfig where
  color : Option Color
  lut : Option (List Color)
  font : Option Font
def fancy : String → String :=
  fun s => s

structure ShowConfig extends FancyConfig where
  useASCII : Bool := false
  currentPrefix : String := ""
  layer : Option Nat := none

-- TODO injections between currentPrefix and layer

-- TODO construct showconfig from input config
def ShowConfig.fromConfig (config : Config) : ShowConfig :=
 ⟨⟨none, none, none⟩, config.ascii, "", some 0⟩

def ShowConfig.preFile (scfg : ShowConfig) :=
  if scfg.useASCII then "|--" else "├──"

def ShowConfig.preDir (scfg : ShowConfig) :=
  if scfg.useASCII then "|  " else "│  "

def ShowConfig.fileName (scfg : ShowConfig) (file : String) : String :=
  s!"{scfg.currentPrefix}{scfg.preFile} {file}"

def ShowConfig.dirName (scfg : ShowConfig) (dir : String) : String :=
  s!"{scfg.currentPrefix}{scfg.preFile} {dir}/"

def ShowConfig.inDirectory (scfg : ShowConfig) : ShowConfig :=
  { scfg with
    currentPrefix := scfg.preDir ++ " " ++ scfg.currentPrefix,
    layer := scfg.layer.map (· + 1) }

def showFileName (scfg : ShowConfig) (file : String) : IO Unit := do
  IO.println (scfg.fileName file)

def showDirName (scfg : ShowConfig) (dir : String) : IO Unit := do
  IO.println (scfg.dirName dir)
