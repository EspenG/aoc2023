import Lake
open Lake DSL

package «aoc» where
  -- add package configuration options here

lean_lib «AOC» where
  -- add library configuration options here

require std from git "https://github.com/leanprover/std4.git" @ s!"v{Lean.versionString}"

@[default_target]
lean_exe «aoc» where
  root := `Main
