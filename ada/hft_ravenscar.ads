-- Ravenscar Real-Time Profile Package for HFT Engine
-- Provides protected-object interfaces compliant with the Ada Ravenscar profile:
--   * Only protected procedures and functions (no conditional entries)
--   * Ceiling-priority locking
--   * No dynamic memory allocation
--   * Bounded, statically-allocated data structures
--   * Ada.Real_Time for monotonic timing
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
with System;

package HFT_Ravenscar is

   -- -----------------------------------------------------------------------
   -- Real-time priority constants (Ravenscar ceiling-priority model)
   -- -----------------------------------------------------------------------
   HFT_Ceiling_Priority : constant System.Priority :=
      System.Priority'Last - 2;

   Monitor_Priority : constant System.Priority :=
      System.Priority'Last - 4;

   -- -----------------------------------------------------------------------
   -- Bounded order queue capacity
   -- -----------------------------------------------------------------------
   Max_Queue_Capacity : constant Positive := 256;

   -- Named array type required for protected-object components
   subtype Queue_Index is Positive range 1 .. Max_Queue_Capacity;
   type Order_Array is array (Queue_Index) of HFT_Engine.Order;

   -- -----------------------------------------------------------------------
   -- Real-time statistics record
   -- -----------------------------------------------------------------------
   type RT_Statistics is record
      Enqueue_Count    : Natural  := 0;
      Dequeue_Count    : Natural  := 0;
      Overflow_Count   : Natural  := 0;
      Underflow_Count  : Natural  := 0;
      Peak_Size        : Natural  := 0;
      Last_Enqueue_ns  : Long_Long_Integer := 0;
      Last_Dequeue_ns  : Long_Long_Integer := 0;
   end record;

   -- -----------------------------------------------------------------------
   -- Protected: bounded FIFO order queue
   -- Ravenscar-compatible: only protected procedures and functions.
   -- Ceiling priority set to HFT_Ceiling_Priority.
   -- -----------------------------------------------------------------------
   protected type Order_Queue is
      pragma Priority (HFT_Ceiling_Priority);

      -- Attempt to add an order; Success => False if queue is full
      procedure Enqueue (O : in     HFT_Engine.Order;
                         Success : out Boolean);

      -- Attempt to remove front order; Success => False if queue is empty
      procedure Dequeue (O : out HFT_Engine.Order;
                         Success : out Boolean);

      -- Peek at the front order without removing
      procedure Peek (O : out HFT_Engine.Order;
                      Success : out Boolean);

      -- Clear all pending orders
      procedure Clear;

      -- Query current occupancy
      function Is_Empty return Boolean;
      function Is_Full  return Boolean;
      function Size     return Natural;
      function Capacity return Natural;

      -- Retrieve snapshot of runtime statistics
      function Get_Statistics return RT_Statistics;

   private
      Items       : Order_Array;
      Head        : Natural := 0;
      Tail        : Natural := 0;
      Count       : Natural := 0;
      Stats       : RT_Statistics;
   end Order_Queue;

   -- -----------------------------------------------------------------------
   -- Protected: real-time compliance monitor
   -- Tracks pass/fail counts and latency using Ada.Real_Time.
   -- Ravenscar-compatible: only protected procedures and functions.
   -- -----------------------------------------------------------------------
   protected type RT_Compliance_Monitor is
      pragma Priority (Monitor_Priority);

      -- Record the outcome of a single compliance check
      procedure Record_Check (Passed      : Boolean;
                              Latency_ns  : Long_Long_Integer);

      -- Reset all counters
      procedure Reset;

      -- Query aggregated results
      function Total_Checks  return Natural;
      function Total_Passed  return Natural;
      function Total_Failed  return Natural;
      function Success_Rate  return Float;
      function Min_Latency   return Long_Long_Integer;
      function Max_Latency   return Long_Long_Integer;
      function Avg_Latency   return Long_Long_Integer;

   private
      Checks  : Natural := 0;
      Passed  : Natural := 0;
      Failed  : Natural := 0;
      Min_Lat : Long_Long_Integer := Long_Long_Integer'Last;
      Max_Lat : Long_Long_Integer := 0;
      Sum_Lat : Long_Long_Integer := 0;
   end RT_Compliance_Monitor;

   -- -----------------------------------------------------------------------
   -- Protected: shared real-time clock reference
   -- Provides a monotonic baseline timestamp for Ravenscar tasks.
   -- -----------------------------------------------------------------------
   protected type RT_Clock is
      pragma Priority (Monitor_Priority);

      procedure Set_Epoch;
      function  Elapsed_ns return Long_Long_Integer;
      function  Now_Epoch  return Ada.Real_Time.Time;

   private
      Epoch : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end RT_Clock;

   -- -----------------------------------------------------------------------
   -- Utility: convert Ada.Real_Time.Time_Span to nanoseconds
   -- -----------------------------------------------------------------------
   function Time_Span_To_Ns (Span : Ada.Real_Time.Time_Span)
      return Long_Long_Integer;

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
