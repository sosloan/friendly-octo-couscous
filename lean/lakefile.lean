import Lake
open Lake DSL

package «hft» where
  -- add package configuration options here

lean_lib «HFT» where
  -- add library configuration options here

@[default_target]
lean_exe «hft» where
  root := `Main
