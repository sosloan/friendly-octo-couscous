-- Optional singleton runtime built from the reusable HFT_Ravenscar types.
pragma Ada_2022;

with HFT_Ravenscar;

package HFT_Ravenscar_Runtime is
   Queue : HFT_Ravenscar.Order_Queue;
   Statistics : HFT_Ravenscar.RT_Compliance_Monitor;

   task Compliance_Monitor is
      pragma Priority (7);
   end Compliance_Monitor;
end HFT_Ravenscar_Runtime;
