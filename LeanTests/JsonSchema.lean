import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

namespace LeanTests.JsonSchema
open Lean

/-- The report contract uses this deliberately bounded subset of JSON Schema 2020-12.
Compilation rejects new keywords so a contract change cannot silently weaken its checks. -/
inductive Rule where
  | always (allow : Bool)
  | types (names : Array String)
  | equal (value : Json)
  | choices (values : Array Json)
  | properties (fields : Array (String × Rule)) (extra : Rule)
  | required (names : Array String)
  | dependencies (fields : Array (String × Array String))
  | items (rule : Rule)
  | minimum (value : JsonNumber)
  | oneOf (rules : Array Rule)
  | conditional (condition yes no : Rule)
  | all (rules : Array Rule)
  deriving Inhabited

/-- JSON Schema compares numbers by value: 1, 1.0, and 1e0 are equal. -/
private partial def sameValue : Json → Json → Bool
  | .num a, .num b => compare a b == .eq
  | .arr a, .arr b => a.size == b.size && (a.zip b).all fun (a, b) => sameValue a b
  | .obj a, .obj b => a.size == b.size && a.all fun key value =>
      (b.get? key).any (sameValue value)
  | a, b => a == b

private def unique [BEq α] (values : Array α) : Bool := Id.run do
  let mut seen := #[]
  for value in values do
    if seen.contains value then return false
    seen := seen.push value
  return true

private def strings (value : Json) : Except String (Array String) := do
  let values ← (← value.getArr?).mapM Json.getStr?
  unless unique values do throw "expected unique strings"
  return values

private def definition (root : Json) (ref : String) : Except String Json := do
  let some name := ref.dropPrefix? "#/$defs/" | throw s!"unsupported schema reference {ref}"
  -- The contract uses named root definitions, with no external or recursive schemas.
  if name.isEmpty || name.contains '/' || name.contains '~' then
    throw s!"unsupported schema reference {ref}"
  (root.getObjValD "$defs").getObjVal? name.copy

private def typeNames : Array String := #["null", "boolean", "object", "array", "number", "integer", "string"]

private def knownKeywords : Array String := #["$schema", "title", "description", "$defs", "$ref", "type",
  "const", "enum", "properties", "additionalProperties", "required", "dependentRequired", "items",
  "minimum", "oneOf", "if", "then", "else"]

private def located (path : String) (result : Except String α) : Except String α :=
  result.mapError fun message => s!"{path}: {message}"

private partial def compileAt (root schema : Json) (path : String) (refs : List String) : Except String Rule := located path do
  if let .bool allow := schema then return .always allow
  let obj ← schema.getObj?
  for (key, _) in obj do
    unless knownKeywords.contains key do throw s!"unsupported schema keyword {key}"
  let mut rules := #[]
  for (key, value) in obj do
    match key with
    | "$schema" =>
      unless (← value.getStr?) == "https://json-schema.org/draft/2020-12/schema" do
        throw "unsupported JSON Schema dialect"
    | "title" | "description" => discard <| value.getStr?
    | "$defs" =>
      for (name, sub) in ← value.getObj? do
        discard <| compileAt root sub (path ++ "/$defs/" ++ name) refs
    | "$ref" =>
      let ref ← value.getStr?
      if refs.contains ref then throw s!"recursive schema reference {ref} is unsupported"
      rules := rules.push (← compileAt root (← definition root ref) ref (ref :: refs))
    | "type" =>
      let names ← match value with
        | .str name => pure #[name]
        | _ => strings value
      unless !names.isEmpty && names.all typeNames.contains do throw "invalid schema type"
      rules := rules.push (.types names)
    | "const" => rules := rules.push (.equal value)
    | "enum" =>
      let values ← value.getArr?
      if values.isEmpty then throw "enum must not be empty"
      for i in [:values.size] do
        if (values.extract 0 i).any (sameValue values[i]!) then throw "enum values must be unique"
      rules := rules.push (.choices values)
    | "required" => rules := rules.push (.required (← strings value))
    | "dependentRequired" =>
      let fields ← (← value.getObj?).toArray.mapM fun (name, value) => do
        return (name, ← strings value)
      rules := rules.push (.dependencies fields)
    | "items" => rules := rules.push (.items (← compileAt root value (path ++ "/items") refs))
    | "minimum" => rules := rules.push (.minimum (← value.getNum?))
    | "oneOf" =>
      let values ← value.getArr?
      if values.isEmpty then throw "oneOf must not be empty"
      let choices ← values.zipIdx.mapM fun (sub, i) => compileAt root sub s!"{path}/oneOf/{i}" refs
      rules := rules.push (.oneOf choices)
    | "properties" | "additionalProperties" | "if" | "then" | "else" => pure ()
    | _ => throw s!"unsupported schema keyword {key}"
  let mut properties := #[]
  if let some value := obj.get? "properties" then
    properties ← (← value.getObj?).toArray.mapM fun (name, sub) => do
      return (name, ← compileAt root sub (path ++ "/properties/" ++ name) refs)
  let extra ← match obj.get? "additionalProperties" with
    | none => pure (.always true)
    | some sub => compileAt root sub (path ++ "/additionalProperties") refs
  if !properties.isEmpty || (obj.get? "additionalProperties").isSome then
    rules := rules.push (.properties properties extra)
  -- Validate then/else even without if, although JSON Schema ignores their assertions there.
  let yes ← match obj.get? "then" with
    | none => pure (.always true)
    | some sub => compileAt root sub (path ++ "/then") refs
  let no ← match obj.get? "else" with
    | none => pure (.always true)
    | some sub => compileAt root sub (path ++ "/else") refs
  if let some condition := obj.get? "if" then
    rules := rules.push (.conditional (← compileAt root condition (path ++ "/if") refs) yes no)
  return .all rules

def compile (schema : Json) : Except String Rule := compileAt schema schema "#" []

private def hasType (value : Json) (name : String) : Bool :=
  match name, value with
  | "null", .null | "boolean", .bool _ | "object", .obj _ | "array", .arr _
  | "number", .num _ | "string", .str _ => true
  | "integer", .num n =>
    let (sign, digits, exponent) := n.normalize
    sign == 0 || exponent >= (toString digits).length
  | _, _ => false

partial def validate (rule : Rule) (value : Json) (path := "$") : Except String Unit := do
  match rule with
  | .always true => pure ()
  | .always false => throw s!"{path}: value is forbidden"
  | .types names => unless names.any (hasType value) do throw s!"{path}: expected {names}"
  | .equal expected => unless sameValue value expected do throw s!"{path}: expected {expected.compress}"
  | .choices choices => unless choices.any (sameValue value) do throw s!"{path}: value is outside enum"
  | .properties fields extra =>
    if let .obj values := value then
      for (name, value) in values do
        let rule := ((fields.find? (·.1 == name)).map Prod.snd).getD extra
        validate rule value s!"{path}.{name}"
  | .required names =>
    if let .obj values := value then
      for name in names do
        unless (values.get? name).isSome do throw s!"{path}: missing required property {name}"
  | .dependencies fields =>
    if let .obj values := value then
      for (name, names) in fields do
        if (values.get? name).isSome then validate (.required names) value path
  | .items item =>
    if let .arr values := value then
      for (value, i) in values.zipIdx do validate item value s!"{path}[{i}]"
  | .minimum minimum =>
    if let .num n := value then
      if n < minimum then throw s!"{path}: expected a number at least {minimum}"
  | .oneOf choices =>
    let count := choices.countP fun choice => (validate choice value path).isOk
    unless count == 1 do throw s!"{path}: expected exactly one matching schema, got {count}"
  | .conditional condition yes no =>
    validate (if (validate condition value path).isOk then yes else no) value path
  | .all rules => for rule in rules do validate rule value path

def reportSchema := include_str "../schema/report-v1.json"

def reportRule : Except String Rule := Json.parse reportSchema >>= compile

/-- Negative cases protect the validator itself, including features used by the real contract. -/
def runTests : IO Unit := do
  let parse := fun text => IO.ofExcept (Json.parse text)
  let test := fun schema value expected => do
    let rule ← IO.ofExcept (compile (← parse schema))
    let result := validate rule (← parse value)
    unless result.isOk == expected do
      throw <| IO.userError s!"schema test failed: {schema} against {value}: {repr result}"
  for (schema, value, valid) in [
      (r#"{"type":"integer","minimum":0}"#, "1.0", true),
      (r#"{"type":"integer","minimum":0}"#, "1.5", false),
      (r#"{"type":"integer","minimum":0}"#, "-1", false),
      (r#"{"type":"integer"}"#, "true", false),
      (r#"{"type":"integer"}"#, "1e-100000", false),
      (r#"{"const":{"v":[1]}}"#, r#"{"v":[1.0]}"#, true),
      (r#"{"const":true}"#, "1", false),
      (r#"{"enum":[null,1]}"#, "1.0", true),
      (r#"{"enum":[null,1]}"#, "2", false),
      (r#"{"type":["string","null"]}"#, "null", true),
      (r#"{"type":["string","null"]}"#, "2", false),
      (r#"{"required":["x"]}"#, r#"{"x":null}"#, true),
      (r#"{"required":["x"]}"#, "{}", false),
      (r#"{"properties":{"x":{"type":"string"}},"additionalProperties":false}"#, r#"{"x":"ok","y":0}"#, false),
      (r#"{"additionalProperties":{"type":"string"}}"#, r#"{"x":1}"#, false),
      (r#"{"items":{"type":"string"}}"#, r#"["ok",1]"#, false),
      (r#"{"dependentRequired":{"x":["y"]}}"#, r#"{"x":null}"#, false),
      (r#"{"oneOf":[{"type":"integer"},{"type":"number"}]}"#, "1", false),
      (r#"{"oneOf":[{"type":"string"},{"type":"number"}]}"#, "1", true),
      (r#"{"oneOf":[{"type":"string"},{"type":"number"}]}"#, "null", false),
      (r#"{"if":{"const":1},"then":false,"else":true}"#, "1", false),
      (r#"{"if":{"const":1},"then":false,"else":true}"#, "2", true),
      (r#"{"then":false}"#, "1", true),
      (r##"{"$defs":{"s":{"type":"string"}},"$ref":"#/$defs/s"}"##, "1", false)] do
    test schema value valid
  for schema in [r#"{"unknownAssertion":true}"#, r#"{"type":"str"}"#,
      r#"{"type":[]}"#, r#"{"type":["string","string"]}"#, r#"{"required":[1]}"#,
      r#"{"required":["x","x"]}"#, r#"{"enum":[]}"#, r#"{"enum":[1,1.0]}"#,
      r#"{"oneOf":[]}"#, r#"{"properties":[]}"#, r#"{"minimum":"0"}"#,
      r#"{"$defs":{"unused":{"pattern":"x"}}}"#, r##"{"$ref":"#/$defs/missing"}"##,
      r##"{"$defs":{"x":{"$ref":"#/$defs/x"}},"$ref":"#/$defs/x"}"##] do
    if (compile (← parse schema)).isOk then
      throw <| IO.userError s!"invalid or unsupported schema was accepted: {schema}"
  let rule ← IO.ofExcept reportRule
  let error := Json.mkObj [("schema_version", 1), ("error", "bad input")]
  IO.ofExcept (validate rule error)
  for invalid in [error.setObjVal! "schema_version" 2, error.setObjVal! "error" true] do
    if (validate rule invalid).isOk then throw <| IO.userError "invalid error report was accepted"
  let base ← parse r#"{"schema_version":1,"project":"app","target":"t","root_toolchain":null,"packages":[],"findings":[]}"#
  IO.ofExcept (validate rule base)
  let passed ← parse r#"{"status":"passed","message":"ok","output":"","lake_version":"v","checked_revisions":{}}"#
  let trial := (base.setObjVal! "verification" passed).setObjVal! "trial_revisions" (Json.mkObj [])
  IO.ofExcept (validate rule trial)
  let tested := trial.setObjVal! "test_requested" true
  let completed := tested.setObjVal! "verification" (passed.setObjVal! "test_exit_code" 0)
  IO.ofExcept (validate rule completed)
  for invalid in [base.setObjVal! "verification" passed, tested,
      trial.setObjVal! "verification" (Json.mkObj [("status", "passed"), ("message", "ok")]),
      completed.setObjVal! "verification" (passed.setObjVal! "test_exit_code" 1),
      base.setObjVal! "test_args" (.arr #[]), base.setObjVal! "error" "both branches",
      base.setObjVal! "packages" (.arr #[Json.mkObj []]),
      base.setObjVal! "findings" (.arr #[Json.mkObj []])] do
    if (validate rule invalid).isOk then
      throw <| IO.userError s!"invalid report was accepted: {invalid.compress}"

end LeanTests.JsonSchema
