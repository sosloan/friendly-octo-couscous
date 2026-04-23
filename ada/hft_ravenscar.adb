-- Ravenscar Real-Time Package Body for HFT Engine
pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;

package body HFT_Ravenscar is

   -- Module-level epoch: set once on first use via Clock at elaboration
   Module_Epoch : constant Ada.Real_Time.Time := Clock;

   -- -----------------------------------------------------------------------
   -- Utility: Ada.Real_Time.Time_Span -> nanoseconds (relative to module epoch)
   -- Uses a safe conversion that avoids overflow from Time_First subtraction.
   -- -----------------------------------------------------------------------
   function Time_Span_To_Ns (Span : Ada.Real_Time.Time_Span)
      return Long_Long_Integer
   is
      D : constant Duration := To_Duration (Span);
   begin
      -- Guard against negative spans (shouldn't happen, but be defensive)
      if D <= 0.0 then
         return 0;
      end if;
      -- Cap at Long_Long_Integer'Last / 2 to avoid overflow in conversion
      if D >= Duration (Long_Long_Integer'Last / 2_000_000_000) then
         return Long_Long_Integer'Last / 2;
      end if;
      return Long_Long_Integer (Float (D) * 1.0e9);
   end Time_Span_To_Ns;

   -- -----------------------------------------------------------------------
   -- Order_Queue protected body
   -- -----------------------------------------------------------------------
   protected body Order_Queue is

      procedure Enqueue (O       : in  HFT_Engine.Order;
                         Success : out Boolean) is
         Now : constant Ada.Real_Time.Time := Clock;
      begin
         if Count = Max_Queue_Capacity then
            Stats.Overflow_Count := Stats.Overflow_Count + 1;
            Success := False;
            return;
         end if;

         if Count = 0 then
            Head := 1;
            Tail := 1;
         else
            Tail := (Tail mod Max_Queue_Capacity) + 1;
         end if;

         Items (Tail) := O;
         Count := Count + 1;
         Stats.Enqueue_Count := Stats.Enqueue_Count + 1;

         if Count > Stats.Peak_Size then
            Stats.Peak_Size := Count;
         end if;

         Stats.Last_Enqueue_ns := Time_Span_To_Ns (Now - Module_Epoch);
         Success := True;
      end Enqueue;

      procedure Dequeue (O       : out HFT_Engine.Order;
                         Success : out Boolean) is
         Now : constant Ada.Real_Time.Time := Clock;
      begin
         if Count = 0 then
            Stats.Underflow_Count := Stats.Underflow_Count + 1;
            Success := False;
            return;
         end if;

         O := Items (Head);
         Count := Count - 1;
         Stats.Dequeue_Count := Stats.Dequeue_Count + 1;
         Stats.Last_Dequeue_ns := Time_Span_To_Ns (Now - Module_Epoch);

         if Count = 0 then
            Head := 0;
            Tail := 0;
         else
            Head := (Head mod Max_Queue_Capacity) + 1;
         end if;

         Success := True;
      end Dequeue;

      procedure Peek (O       : out HFT_Engine.Order;
                      Success : out Boolean) is
      begin
         if Count = 0 then
            Success := False;
            return;
         end if;
         O := Items (Head);
         Success := True;
      end Peek;

      procedure Clear is
      begin
         Head  := 0;
         Tail  := 0;
         Count := 0;
      end Clear;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      function Is_Full return Boolean is
      begin
         return Count = Max_Queue_Capacity;
      end Is_Full;

      function Size return Natural is
      begin
         return Count;
      end Size;

      function Capacity return Natural is
      begin
         return Max_Queue_Capacity;
      end Capacity;

      function Get_Statistics return RT_Statistics is
      begin
         return Stats;
      end Get_Statistics;

   end Order_Queue;

   -- -----------------------------------------------------------------------
   -- RT_Compliance_Monitor protected body
   -- -----------------------------------------------------------------------
   protected body RT_Compliance_Monitor is

      procedure Record_Check (Passed     : Boolean;
                              Latency_ns : Long_Long_Integer) is
      begin
         Checks := Checks + 1;
         if Passed then
            RT_Compliance_Monitor.Passed := RT_Compliance_Monitor.Passed + 1;
         else
            Failed := Failed + 1;
         end if;

         Sum_Lat := Sum_Lat + Latency_ns;

         if Latency_ns < Min_Lat then
            Min_Lat := Latency_ns;
         end if;
         if Latency_ns > Max_Lat then
            Max_Lat := Latency_ns;
         end if;
      end Record_Check;

      procedure Reset is
      begin
         Checks  := 0;
         Passed  := 0;
         Failed  := 0;
         Min_Lat := Long_Long_Integer'Last;
         Max_Lat := 0;
         Sum_Lat := 0;
      end Reset;

      function Total_Checks return Natural is
      begin
         return Checks;
      end Total_Checks;

      function Total_Passed return Natural is
      begin
         return Passed;
      end Total_Passed;

      function Total_Failed return Natural is
      begin
         return Failed;
      end Total_Failed;

      function Success_Rate return Float is
      begin
         if Checks = 0 then
            return 0.0;
         end if;
         return Float (Passed) / Float (Checks) * 100.0;
      end Success_Rate;

      function Min_Latency return Long_Long_Integer is
      begin
         if Checks = 0 then
            return 0;
         end if;
         return Min_Lat;
      end Min_Latency;

      function Max_Latency return Long_Long_Integer is
      begin
         return Max_Lat;
      end Max_Latency;

      function Avg_Latency return Long_Long_Integer is
      begin
         if Checks = 0 then
            return 0;
         end if;
         return Sum_Lat / Long_Long_Integer (Checks);
      end Avg_Latency;

   end RT_Compliance_Monitor;

   -- -----------------------------------------------------------------------
   -- RT_Clock protected body
   -- -----------------------------------------------------------------------
   protected body RT_Clock is

      procedure Set_Epoch is
      begin
         Epoch := Clock;
      end Set_Epoch;

      function Elapsed_ns return Long_Long_Integer is
         Now  : constant Ada.Real_Time.Time := Clock;
         Span : constant Ada.Real_Time.Time_Span := Now - Epoch;
      begin
         return Time_Span_To_Ns (Span);
      end Elapsed_ns;

      function Now_Epoch return Ada.Real_Time.Time is
      begin
         return Epoch;
      end Now_Epoch;

   end RT_Clock;
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
