import Lake
open Lake DSL

package «hft» where
  -- add package configuration options here

lean_lib «HFT» where
  -- add library configuration options here

lean_lib «FingerTrap» where
  -- Chinese Finger Trap metric and Bourbaki subsumption proofs

@[default_target]
lean_exe «hft» where
  root := `Main
