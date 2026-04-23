-- Ada Ravenscar-Profile HFT Real-Time Engine -- Implementation
pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with HFT_Spark;

package body HFT_Ravenscar is

   -- ----------------------------------------------------------------
   -- Protected Order Queue body
   -- ----------------------------------------------------------------
   protected body Order_Queue is

      procedure Enqueue (O : HFT_Engine.Order; Success : out Boolean) is
      begin
         if Count = Max_Queue then
            Success := False;
            return;
         end if;
         Buffer (Tail) := O;
         Tail          := (if Tail = Max_Queue then 1 else Tail + 1);
         Count         := Count + 1;
         Success        := True;
      end Enqueue;

      procedure Dequeue (O : out HFT_Engine.Order; Success : out Boolean) is
      begin
         if Count = 0 then
            O       := Sentinel_Order;
            Success := False;
            return;
         end if;
         O    := Buffer (Head);
         Head := (if Head = Max_Queue then 1 else Head + 1);
         Count := Count - 1;
         Success := True;
      end Dequeue;

      function Depth return Queue_Depth is
      begin
         return Count;
      end Depth;

      function Empty return Boolean is
      begin
         return Count = 0;
      end Empty;

   end Order_Queue;

   -- ----------------------------------------------------------------
   -- Protected Compliance Statistics body
   -- ----------------------------------------------------------------
   protected body Compliance_Stats is

      procedure Record_Pass is
      begin
         Pass_Ct := Pass_Ct + 1;
      end Record_Pass;

      procedure Record_Fail is
      begin
         Fail_Ct := Fail_Ct + 1;
      end Record_Fail;

      procedure Reset is
      begin
         Pass_Ct := 0;
         Fail_Ct := 0;
      end Reset;

      function Passed return Natural is
      begin
         return Pass_Ct;
      end Passed;

      function Failed return Natural is
      begin
         return Fail_Ct;
      end Failed;

      function Total return Natural is
      begin
         return Pass_Ct + Fail_Ct;
      end Total;

      function Pass_Rate_Pct return Natural is
         T : constant Natural := Pass_Ct + Fail_Ct;
      begin
         if T = 0 then
            return 0;
         end if;
         return (Pass_Ct * 100) / T;
      end Pass_Rate_Pct;

   end Compliance_Stats;

   -- ----------------------------------------------------------------
   -- Periodic Compliance Monitor Task body
   -- Runs every 50 ms; processes one order per cycle.
   -- The infinite loop satisfies Ravenscar No_Task_Termination.
   -- ----------------------------------------------------------------
   task body Compliance_Monitor is
      Period    : constant Time_Span := Milliseconds (50);
      Next_Wake : Time              := Start_Time + Period;
      An_Order  : HFT_Engine.Order;
      Got       : Boolean;
      Result    : HFT_Spark.Spark_Result;
   begin
      loop
         delay until Next_Wake;
         Next_Wake := Next_Wake + Period;

         -- Dequeue and check one order per cycle
         Order_Queue.Dequeue (An_Order, Got);
         if Got then
            Result := HFT_Spark.Spark_Full_Check (An_Order);
            if Result.Passed then
               Compliance_Stats.Record_Pass;
            else
               Compliance_Stats.Record_Fail;
            end if;
         end if;
      end loop;
   end Compliance_Monitor;

end HFT_Ravenscar;
