pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;

package body HFT_Ravenscar is
   Module_Epoch : constant Ada.Real_Time.Time := Clock;

   function Time_Span_To_NS
     (Span : Ada.Real_Time.Time_Span) return Long_Long_Integer
   is
      Value : constant Duration := To_Duration (Span);
   begin
      if Value <= 0.0 then
         return 0;
      elsif Value >= Duration (Long_Long_Integer'Last / 1_000_000_000) then
         return Long_Long_Integer'Last;
      else
         return Long_Long_Integer (Value * 1_000_000_000);
      end if;
   end Time_Span_To_NS;

   protected body Order_Queue is
      procedure Enqueue (O : HFT_Engine.Order; Success : out Boolean) is
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
         Stats.Peak_Size := Natural'Max (Stats.Peak_Size, Count);
         Stats.Last_Enqueue_NS := Time_Span_To_NS (Now - Module_Epoch);
         Success := True;
      end Enqueue;

      procedure Dequeue (O : out HFT_Engine.Order; Success : out Boolean) is
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
         Stats.Last_Dequeue_NS := Time_Span_To_NS (Now - Module_Epoch);
         if Count = 0 then
            Head := 0;
            Tail := 0;
         else
            Head := (Head mod Max_Queue_Capacity) + 1;
         end if;
         Success := True;
      end Dequeue;

      procedure Peek (O : out HFT_Engine.Order; Success : out Boolean) is
      begin
         if Count = 0 then
            Success := False;
         else
            O := Items (Head);
            Success := True;
         end if;
      end Peek;

      procedure Clear is
      begin
         Head := 0;
         Tail := 0;
         Count := 0;
      end Clear;

      function Is_Empty return Boolean is (Count = 0);
      function Is_Full return Boolean is (Count = Max_Queue_Capacity);
      function Size return Natural is (Count);
      function Capacity return Natural is (Max_Queue_Capacity);
      function Get_Statistics return RT_Statistics is (Stats);
   end Order_Queue;

   protected body RT_Compliance_Monitor is
      procedure Record_Check
        (Passed : Boolean; Latency_NS : Long_Long_Integer) is
      begin
         Checks := Checks + 1;
         if Passed then
            RT_Compliance_Monitor.Passed :=
              RT_Compliance_Monitor.Passed + 1;
         else
            Failed := Failed + 1;
         end if;
         Sum_Lat := Sum_Lat + Latency_NS;
         Min_Lat := Long_Long_Integer'Min (Min_Lat, Latency_NS);
         Max_Lat := Long_Long_Integer'Max (Max_Lat, Latency_NS);
      end Record_Check;

      procedure Reset is
      begin
         Checks := 0;
         Passed := 0;
         Failed := 0;
         Min_Lat := Long_Long_Integer'Last;
         Max_Lat := 0;
         Sum_Lat := 0;
      end Reset;

      function Total_Checks return Natural is (Checks);
      function Total_Passed return Natural is (Passed);
      function Total_Failed return Natural is (Failed);
      function Success_Rate return Float is
        (if Checks = 0 then 0.0
         else Float (Passed) / Float (Checks) * 100.0);
      function Min_Latency return Long_Long_Integer is
        (if Checks = 0 then 0 else Min_Lat);
      function Max_Latency return Long_Long_Integer is (Max_Lat);
      function Avg_Latency return Long_Long_Integer is
        (if Checks = 0 then 0 else Sum_Lat / Long_Long_Integer (Checks));
   end RT_Compliance_Monitor;

   protected body RT_Clock is
      procedure Set_Epoch is
      begin
         Epoch := Clock;
      end Set_Epoch;

      function Elapsed_NS return Long_Long_Integer is
        (Time_Span_To_NS (Clock - Epoch));

      function Now_Epoch return Ada.Real_Time.Time is (Epoch);
   end RT_Clock;
end HFT_Ravenscar;
