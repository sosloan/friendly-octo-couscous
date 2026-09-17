-- Ada Ravenscar-Profile HFT Periodic Runtime
-- Owns the shared queue, statistics, and long-lived compliance task.
pragma Ada_2022;

with Ada.Real_Time;
with HFT_Engine;

package HFT_Ravenscar_Runtime is
   pragma Elaborate_Body;

   Max_Queue : constant := 32;

   subtype Queue_Depth is Natural range 0 .. Max_Queue;
   subtype Buf_Index is Positive range 1 .. Max_Queue;

   Sentinel_Order : constant HFT_Engine.Order :=
     (Order_ID   => 1,
      Symbol     => "SENTINEL  ",
      Price_Val  => 0.01,
      Qty        => 1,
      Order_Side => HFT_Engine.Buy,
      Time_Stamp => 1);

   type Order_Buffer is array (Buf_Index) of HFT_Engine.Order;

   protected Order_Queue is
      pragma Priority (10);

      procedure Enqueue (O : HFT_Engine.Order; Success : out Boolean);
      procedure Dequeue (O : out HFT_Engine.Order; Success : out Boolean);
      function Depth return Queue_Depth;
      function Empty return Boolean;

   private
      Buffer : Order_Buffer := [others => Sentinel_Order];
      Head   : Buf_Index := 1;
      Tail   : Buf_Index := 1;
      Count  : Queue_Depth := 0;
   end Order_Queue;

   protected Compliance_Stats is
      pragma Priority (9);

      procedure Record_Pass;
      procedure Record_Fail;
      procedure Reset;

      function Passed return Natural;
      function Failed return Natural;
      function Total return Natural;
      function Pass_Rate_Pct return Natural;

   private
      Pass_Ct : Natural := 0;
      Fail_Ct : Natural := 0;
   end Compliance_Stats;

   Monitor_Period : constant Ada.Real_Time.Time_Span :=
      Ada.Real_Time.Milliseconds (50);
   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   task Compliance_Monitor is
      pragma Priority (7);
   end Compliance_Monitor;

end HFT_Ravenscar_Runtime;
