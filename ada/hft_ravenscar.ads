-- Ada Ravenscar-Profile HFT Real-Time Engine
-- Protected objects and a periodic compliance-monitor task.
-- Complies with the Ravenscar tasking profile (Ada 2022, D.13):
--   * Fixed-size static storage only (no heap allocation)
--   * All protected objects and tasks declared at library level
--   * Periodic task uses Ada.Real_Time.delay until (no relative delay)
--   * No select statements, no requeue, no task entries
pragma Ada_2022;

with HFT_Engine;
with Ada.Real_Time;

package HFT_Ravenscar is
   pragma Elaborate_Body;

   -- ------------------------------------------------------------------
   -- Queue configuration (static allocation)
   -- ------------------------------------------------------------------
   Max_Queue : constant := 32;

   subtype Queue_Depth is Natural range 0 .. Max_Queue;
   subtype Buf_Index   is Positive range 1 .. Max_Queue;

   -- Default/sentinel order used to initialise empty buffer slots
   Sentinel_Order : constant HFT_Engine.Order :=
     (Order_ID   => 1,
      Symbol     => "SENTINEL  ",
      Price_Val  => 0.01,
      Qty        => 1,
      Order_Side => HFT_Engine.Buy,
      Time_Stamp => 1);

   type Order_Buffer is array (Buf_Index) of HFT_Engine.Order;

   -- ------------------------------------------------------------------
   -- Protected Order Queue
   -- Thread-safe circular FIFO; Ravenscar-compliant (no entries).
   -- ------------------------------------------------------------------
   protected Order_Queue is
      pragma Priority (10);

      -- Add an order; Success = False when the queue is full.
      procedure Enqueue (O : HFT_Engine.Order; Success : out Boolean);

      -- Remove the head order; Success = False when the queue is empty.
      procedure Dequeue (O : out HFT_Engine.Order; Success : out Boolean);

      -- Current number of orders waiting in the queue.
      function Depth return Queue_Depth;

      -- True when no orders are pending.
      function Empty return Boolean;

   private
      Buffer : Order_Buffer := [others => Sentinel_Order];
      Head   : Buf_Index  := 1;   -- position of next item to dequeue
      Tail   : Buf_Index  := 1;   -- position where next item will land
      Count  : Queue_Depth := 0;
   end Order_Queue;

   -- ------------------------------------------------------------------
   -- Protected Compliance Statistics
   -- Updated by Compliance_Monitor; read by the main task.
   -- ------------------------------------------------------------------
   protected Compliance_Stats is
      pragma Priority (9);

      procedure Record_Pass;
      procedure Record_Fail;
      procedure Reset;

      function Passed  return Natural;
      function Failed  return Natural;
      function Total   return Natural;

      -- Integer percentage 0 .. 100 (0 when Total = 0)
      function Pass_Rate_Pct return Natural;

   private
      Pass_Ct : Natural := 0;
      Fail_Ct : Natural := 0;
   end Compliance_Stats;

   -- ------------------------------------------------------------------
   -- Monitor start time (set at elaboration, used by Compliance_Monitor)
   -- ------------------------------------------------------------------
   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   -- ------------------------------------------------------------------
   -- Periodic Compliance Monitor Task
   -- Runs every 50 ms; dequeues one order per cycle and runs the full
   -- SPARK compliance check.  Never terminates (Ravenscar: No_Task_Termination).
   -- ------------------------------------------------------------------
   task Compliance_Monitor is
      pragma Priority (7);
   end Compliance_Monitor;

end HFT_Ravenscar;
