import Lake
open Lake DSL

package «p4» where
  -- add package configuration options here

lean_lib «P4» where
  -- add library configuration options here

require std from git "https://github.com/leanprover/std4.git" @ s!"v{Lean.versionString}"

@[default_target]
lean_exe «p4» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
