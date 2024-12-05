class Config (κ: Type) where
  toConfigStyleString : κ → String
  toConfigStyleString? : Option κ → Option String
  buildCurrentIO : κ → IO κ := fun k => pure k

-- BasicConfig
structure BasicConfig where
  ascii : Bool
  fancy : Bool
  rootPath  : String
  maxLayer : Option Nat
deriving Repr

instance : ToString (Option BasicConfig) where
  toString
    | some c => s!"Configuration (ascii: {c.ascii}, fancy: {c.fancy})"
    | none => "No Configuration!"

def BasicConfig.toConfigString (c : BasicConfig) : String :=
  s!"[CONFIG]Basic Configuration (ascii: {c.ascii}, fancy: {c.fancy}, rootpath: {c.rootPath})"

def BasicConfig.toConfigString? (oc : Option BasicConfig) : Option String :=
  oc.map toConfigString

instance : Config BasicConfig where
  toConfigStyleString := BasicConfig.toConfigString
  toConfigStyleString? := BasicConfig.toConfigString?




-- FancyConfig
-- TODO: Implement fancy to show colors
abbrev Color := String
abbrev Font := String
structure FancyConfig extends BasicConfig where
  color : Color
  lut : List Color
  font : Font
deriving Repr
def FancyConfig.fancyF : String → String :=
  fun s => s
def FancyConfig.toConfigString (c : FancyConfig) : String :=
  s!"[CONFIG]Fancy Configuration (ascii: {c.ascii}, fancy: {c.fancy}, rootpath: {c.rootPath}\n\t\t\tcolor: {c.color}, lut: {c.lut}, font: {c.font})"
def FancyConfig.toConfigString? (oc: Option FancyConfig) : Option String :=
  oc.map toConfigString
instance : Config FancyConfig where
  toConfigStyleString := FancyConfig.toConfigString
  toConfigStyleString? := FancyConfig.toConfigString?


-- ShowConfig
structure ShowConfig extends FancyConfig where
  useASCII : Bool := false
  curPrefix : String := ""
  curLayer : Option Nat
deriving Repr
def ShowConfig.toConfigString (c : ShowConfig) : String :=
  s!"[CONFIG]Show Configuration (ascii: {c.ascii}, fancy: {c.fancy}, rootpath: {c.rootPath}\n\t\t\tcolor: {c.color}, lut: {c.lut}, font: {c.font}\n\t\t\tuseASCII: {c.useASCII}, curPrefix: {c.curPrefix}, curLayer: {c.curLayer})"
def ShowConfig.toConfigString? (oc : Option ShowConfig) : Option String :=
  oc.map toConfigString
instance : Config ShowConfig where
  toConfigStyleString := ShowConfig.toConfigString
  toConfigStyleString? := ShowConfig.toConfigString?

-- TODO injections between currentPrefix and layer

-- TODO construct showconfig from input config file
-- Build ShowConfig from the given BasicConfig
def ShowConfig.fromBasicConfig (c : BasicConfig) : ShowConfig :=
  match c.maxLayer with
  | none => ⟨⟨⟨c.ascii, c.fancy , c.rootPath, c.maxLayer⟩,"", [] , ""⟩, c.ascii, "", none⟩
  | _ => ⟨⟨⟨c.ascii, c.fancy, c.rootPath, c.maxLayer⟩,"", [] , ""⟩, c.ascii, "", some 0⟩

def ShowConfig.preFile (scfg : ShowConfig) :=
  if scfg.useASCII then "|--" else "├──"

def ShowConfig.preDir (scfg : ShowConfig) :=
  if scfg.useASCII then "|  " else "│  "

def ShowConfig.fileName (scfg : ShowConfig) (file : String) : String :=
  s!"{scfg.curPrefix}{scfg.preFile} {file}"

def ShowConfig.dirName (scfg : ShowConfig) (dir : String) : String :=
  s!"{scfg.curPrefix}{scfg.preFile} {dir}/"

def ShowConfig.inDirectory (scfg : ShowConfig) : ShowConfig :=
  { scfg with
    curPrefix := scfg.preDir ++ " " ++ scfg.curPrefix,
    curLayer := scfg.curLayer.map (· + 1) } -- Functor f to be applied in Option


-- use `κ` (\kappa) to represent a specific ConfigType (e.g. C)
def ConfigIO [Config κ] (α : Type) : Type := κ → IO α


def showFileName (scfg : ShowConfig) (file : String) : IO Unit := do
  IO.println (scfg.fileName file)

def showDirName (scfg : ShowConfig) (dir : String) : IO Unit := do
  IO.println (scfg.dirName dir)
