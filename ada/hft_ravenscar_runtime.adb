pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with HFT_Engine;
with HFT_Spark;

package body HFT_Ravenscar_Runtime is
   task body Compliance_Monitor is
      Period    : constant Time_Span := Milliseconds (50);
      Next_Wake : Time := Clock + Period;
      An_Order  : HFT_Engine.Order;
      Found     : Boolean;
      Result    : HFT_Spark.Spark_Result;
      Started   : Time;
   begin
      loop
         delay until Next_Wake;
         Next_Wake := Next_Wake + Period;
         Queue.Dequeue (An_Order, Found);
         if Found then
            Started := Clock;
            Result := HFT_Spark.Spark_Full_Check (An_Order);
            Statistics.Record_Check
              (Result.Passed,
               HFT_Ravenscar.Time_Span_To_NS (Clock - Started));
         end if;
      end loop;
   end Compliance_Monitor;
end HFT_Ravenscar_Runtime;
