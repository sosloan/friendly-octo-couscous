-- Ravenscar Extreme Test Suite
-- Tests the HFT_Ravenscar protected objects and periodic compliance
-- monitor task under the Ada Ravenscar real-time tasking profile.
--
-- Ravenscar-profile restrictions in effect:
--   * No_Relative_Delay    : delay until only
--   * No_Select_Statements : no select blocks
--   * No_Task_Hierarchy    : tasks at library level only
--   * No_Task_Termination  : Compliance_Monitor runs forever
--   * No_Implicit_Heap_Allocations : fixed-size structures only
--
-- The main procedure enqueues orders, waits for the monitor task to
-- process them (Ada.Real_Time.delay until), then reads protected stats.
pragma Ada_2022;
pragma Profile (Ravenscar);

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with HFT_Engine;     use HFT_Engine;
with HFT_Ravenscar;
with GNAT.OS_Lib;

procedure HFT_Ravenscar_Test is

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Cond : Boolean; Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Cond then
         Pass_Count := Pass_Count + 1;
         Put ("  "); Put ("OK  "); Put_Line (Name);
      else
         Put ("  "); Put ("FAIL"); Put_Line (Name);
      end if;
   end Assert;

   -- Fixed Unix-style timestamp (avoids Ada.Calendar in Ravenscar)
   -- Value corresponds to a representative epoch point.
   Test_TS : constant Timestamp := 1_716_000_000;

   -- Build a test order inline
   function Make
     (ID   : Positive;
      Sym  : String;
      P    : Price;
      Q    : Quantity;
      Side : HFT_Engine.Side := Buy) return Order is
   begin
      return (Order_ID   => ID,
              Symbol     => Sym,
              Price_Val  => P,
              Qty        => Q,
              Order_Side => Side,
              Time_Stamp => Test_TS);
   end Make;

   OK : Boolean;

begin
   Put_Line ("=============================================");
   Put_Line ("=== Ada Ravenscar Extreme Test Suite     ===");
   Put_Line ("=============================================");
   New_Line;

   -- Reset state from any earlier run
   HFT_Ravenscar.Compliance_Stats.Reset;

   -- ==============================================================
   -- Test 1: Protected queue — basic enqueue / dequeue
   -- ==============================================================
   Put_Line ("Test 1: Protected Order Queue Operations");
   Put_Line ("-----------------------------------------");

   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (1, "AAPL      ", 150.50, 100), OK);
   Assert (OK, "Enqueue valid order 1 (AAPL)");

   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (2, "GOOGL     ", 2800.0, 10, Sell), OK);
   Assert (OK, "Enqueue valid order 2 (GOOGL Sell)");

   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (3, "NVDA      ", 450.0, 50), OK);
   Assert (OK, "Enqueue valid order 3 (NVDA)");

   Assert (HFT_Ravenscar.Order_Queue.Depth = 3,
           "Queue depth = 3 after 3 enqueues");
   Assert (not HFT_Ravenscar.Order_Queue.Empty,
           "Queue is not empty");

   -- ==============================================================
   -- Test 2: Ravenscar real-time compliance monitoring
   -- Enqueue 10 valid orders, wait for the 50 ms periodic task.
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 2: Real-Time Compliance Monitoring");
   Put_Line ("----------------------------------------");

   for I in 4 .. 12 loop
      HFT_Ravenscar.Order_Queue.Enqueue (
         Make (I, "MSFT      ", Price (100.0 + Price (I)), Quantity (I * 10)),
         OK);
      Assert (OK, "Enqueue order " & I'Image);
   end loop;

   Put_Line ("  Waiting 1.5 s for Compliance_Monitor task ...");
   -- 12 orders + initial 3 = 15; at 50 ms/order => 750 ms.  Wait 1.5 s.
   delay until Clock + Seconds (1) + Milliseconds (500);

   Assert (HFT_Ravenscar.Order_Queue.Empty,
           "All orders processed by monitor task");
   Assert (HFT_Ravenscar.Compliance_Stats.Total > 0,
           "Compliance statistics updated");

   -- ==============================================================
   -- Test 3: Statistics integrity
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 3: Compliance Statistics Integrity");
   Put_Line ("----------------------------------------");
   declare
      P : constant Natural := HFT_Ravenscar.Compliance_Stats.Passed;
      F : constant Natural := HFT_Ravenscar.Compliance_Stats.Failed;
      T : constant Natural := HFT_Ravenscar.Compliance_Stats.Total;
      R : constant Natural := HFT_Ravenscar.Compliance_Stats.Pass_Rate_Pct;
   begin
      Put ("  Passed:   "); Put_Line (Natural'Image (P));
      Put ("  Failed:   "); Put_Line (Natural'Image (F));
      Put ("  Total:    "); Put_Line (Natural'Image (T));
      Put ("  Pass %:   "); Put_Line (Natural'Image (R));

      Assert (P + F = T,  "Passed + Failed = Total");
      Assert (P > 0,      "At least one order passed compliance");
      Assert (T = 12,     "All 12 enqueued orders were checked");
      Assert (R = 100,    "All 12 valid orders have 100% pass rate");
   end;

   -- ==============================================================
   -- Test 4: Invalid order rejected by monitor
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 4: Non-Compliant Order Detection");
   Put_Line ("--------------------------------------");

   HFT_Ravenscar.Compliance_Stats.Reset;

   -- Invalid: lowercase symbol fails SPARK symbol check
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (20, "tsla      ", 200.0, 50), OK);
   Assert (OK, "Non-compliant order enqueued (queue accepts all)");

   -- Invalid: symbol contains digits
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (21, "XYZ123    ", 100.0, 100), OK);
   Assert (OK, "Symbol-with-digits order enqueued");

   Put_Line ("  Waiting 300 ms for monitor to process 2 invalid orders ...");
   delay until Clock + Milliseconds (300);

   Assert (HFT_Ravenscar.Compliance_Stats.Failed = 2,
           "Monitor detected 2 non-compliant orders");
   Assert (HFT_Ravenscar.Compliance_Stats.Passed = 0,
           "No valid orders in this batch");

   -- ==============================================================
   -- Test 5: Queue overflow protection
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 5: Queue Overflow Protection (Max =" &
             Integer'Image (HFT_Ravenscar.Max_Queue) & ")");
   Put_Line ("--------------------------------------");

   -- Drain any residual orders first
   delay until Clock + Milliseconds (200);

   HFT_Ravenscar.Compliance_Stats.Reset;

   -- Fill to max
   for I in 1 .. HFT_Ravenscar.Max_Queue loop
      HFT_Ravenscar.Order_Queue.Enqueue (
         Make (I, "FILL      ", 10.0, Quantity (I)), OK);
   end loop;
   Assert (HFT_Ravenscar.Order_Queue.Depth = HFT_Ravenscar.Max_Queue,
           "Queue filled to Max_Queue");

   -- One more must be rejected
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (99, "OVER      ", 10.0, 1), OK);
   Assert (not OK, "Overflow enqueue returns Success = False");

   -- ==============================================================
   -- Test 6: Real-time throughput measurement
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 6: Real-Time Throughput");
   Put_Line ("-----------------------------");

   -- Queue is full (32 orders).  Wait enough time for all to drain.
   -- 32 orders * 50 ms/order = 1.6 s.  Wait 2 s.
   Put_Line ("  Waiting 2 s to drain full queue ...");
   delay until Clock + Seconds (2);

   Assert (HFT_Ravenscar.Order_Queue.Empty,
           "Full queue drained within 2 s");
   Assert (HFT_Ravenscar.Compliance_Stats.Total = 32,
           "All 32 queued orders were processed");

   -- ==============================================================
   -- Test 7: Extreme price / quantity boundary orders
   -- ==============================================================
   Put_Line ("");
   Put_Line ("Test 7: Extreme Value Orders");
   Put_Line ("-----------------------------");

   HFT_Ravenscar.Compliance_Stats.Reset;

   -- Minimum compliant price and quantity
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (30, "MINN      ", 0.01, 1), OK);
   Assert (OK, "Min price (0.01) + min qty (1) enqueued");

   -- Order exactly at value limit: 100_000 * 1000 = 100M
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (31, "LIMT      ", 100_000.0, 1_000), OK);
   Assert (OK, "Order exactly at 100M value limit enqueued");

   -- Order just over value limit: 100_001 * 1000 = 100.001M (should fail)
   HFT_Ravenscar.Order_Queue.Enqueue (
      Make (32, "OVER      ", 100_001.0, 1_000), OK);
   Assert (OK, "Order over 100M limit enqueued");

   Put_Line ("  Waiting 300 ms for monitor ...");
   delay until Clock + Milliseconds (300);

   Assert (HFT_Ravenscar.Compliance_Stats.Passed = 2,
           "Min-value + at-limit orders passed");
   Assert (HFT_Ravenscar.Compliance_Stats.Failed = 1,
           "Over-limit order failed");

   -- ==============================================================
   -- Summary
   -- ==============================================================
   New_Line;
   Put_Line ("=============================================");
   Put_Line ("=== Ravenscar Test Summary              ===");
   Put_Line ("=============================================");
   Put ("Total:   "); Put_Line (Natural'Image (Test_Count));
   Put ("Passed:  "); Put_Line (Natural'Image (Pass_Count));
   Put ("Failed:  "); Put_Line (Natural'Image (Test_Count - Pass_Count));
   New_Line;
   if Pass_Count = Test_Count then
      Put_Line ("✓✓✓ ALL RAVENSCAR TESTS PASSED! ✓✓✓");
   else
      Put_Line ("✗✗✗ SOME RAVENSCAR TESTS FAILED ✗✗✗");
   end if;
   Put_Line ("=============================================");

   -- Compliance_Monitor task runs forever (Ravenscar No_Task_Termination).
   -- Force process exit so the test suite can return a result.
   GNAT.OS_Lib.OS_Exit (if Pass_Count = Test_Count then 0 else 1);

end HFT_Ravenscar_Test;
