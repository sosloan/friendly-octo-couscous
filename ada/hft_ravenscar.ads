-- Reusable Ravenscar-compatible protected types.  Long-lived tasks are kept
-- in HFT_Ravenscar_Runtime so clients can instantiate these types independently.
pragma Ada_2022;

with HFT_Engine;
with Ada.Real_Time;
with System;

package HFT_Ravenscar is
   HFT_Ceiling_Priority : constant System.Priority :=
      System.Priority'Last - 2;
   Monitor_Priority : constant System.Priority :=
      System.Priority'Last - 4;
   Max_Queue_Capacity : constant Positive := 256;

   subtype Queue_Index is Positive range 1 .. Max_Queue_Capacity;
   type Order_Array is array (Queue_Index) of HFT_Engine.Order;

   type RT_Statistics is record
      Enqueue_Count   : Natural := 0;
      Dequeue_Count   : Natural := 0;
      Overflow_Count  : Natural := 0;
      Underflow_Count : Natural := 0;
      Peak_Size       : Natural := 0;
      Last_Enqueue_NS : Long_Long_Integer := 0;
      Last_Dequeue_NS : Long_Long_Integer := 0;
   end record;

   protected type Order_Queue is
      pragma Priority (HFT_Ceiling_Priority);
      procedure Enqueue (O : HFT_Engine.Order; Success : out Boolean);
      procedure Dequeue (O : out HFT_Engine.Order; Success : out Boolean);
      procedure Peek (O : out HFT_Engine.Order; Success : out Boolean);
      procedure Clear;
      function Is_Empty return Boolean;
      function Is_Full return Boolean;
      function Size return Natural;
      function Capacity return Natural;
      function Get_Statistics return RT_Statistics;
   private
      Items : Order_Array;
      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
      Stats : RT_Statistics;
   end Order_Queue;

   protected type RT_Compliance_Monitor is
      pragma Priority (Monitor_Priority);
      procedure Record_Check
        (Passed : Boolean; Latency_NS : Long_Long_Integer);
      procedure Reset;
      function Total_Checks return Natural;
      function Total_Passed return Natural;
      function Total_Failed return Natural;
      function Success_Rate return Float;
      function Min_Latency return Long_Long_Integer;
      function Max_Latency return Long_Long_Integer;
      function Avg_Latency return Long_Long_Integer;
   private
      Checks  : Natural := 0;
      Passed  : Natural := 0;
      Failed  : Natural := 0;
      Min_Lat : Long_Long_Integer := Long_Long_Integer'Last;
      Max_Lat : Long_Long_Integer := 0;
      Sum_Lat : Long_Long_Integer := 0;
   end RT_Compliance_Monitor;

   protected type RT_Clock is
      pragma Priority (Monitor_Priority);
      procedure Set_Epoch;
      function Elapsed_NS return Long_Long_Integer;
      function Now_Epoch return Ada.Real_Time.Time;
   private
      Epoch : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end RT_Clock;

   function Time_Span_To_NS
     (Span : Ada.Real_Time.Time_Span) return Long_Long_Integer;
end HFT_Ravenscar;
