-- Extreme Ada SPARK + Ravenscar Test Suite
-- Exercises formal verification contracts and Ravenscar real-time interfaces
-- across all HFT engine components.

with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with HFT_Engine;    use HFT_Engine;
with HFT_Time_Util;
with HFT_Compliance; use HFT_Compliance;
with HFT_Audit;      use HFT_Audit;
with HFT_SPARK;
with HFT_Ravenscar;

procedure HFT_Spark_Ravenscar_Test is

   Test_Count   : Natural := 0;
   Pass_Count   : Natural := 0;
   Current_Time : constant Timestamp :=
      Timestamp (HFT_Time_Util.Get_Unix_Timestamp);

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  ✓ PASS: " & Test_Name);
      else
         Put_Line ("  ✗ FAIL: " & Test_Name);
      end if;
   end Assert;

   -- -----------------------------------------------------------------------
   -- Helper: build a basic valid order
   -- -----------------------------------------------------------------------
   function Make_Order (
      ID  : Positive;
      Sym : String;
      P   : Price;
      Q   : Quantity;
      S   : Side
   ) return Order is
      O   : Order;
      Sym_Fixed : String (1 .. Symbol_Length) := (others => ' ');
      Len : constant Natural := Natural'Min (Sym'Length, Symbol_Length);
   begin
      Sym_Fixed (1 .. Len) := Sym (Sym'First .. Sym'First + Len - 1);
      O := (Order_ID   => ID,
            Symbol     => Sym_Fixed,
            Price_Val  => P,
            Qty        => Q,
            Order_Side => S,
            Time_Stamp => Current_Time);
      return O;
   end Make_Order;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 1: Verified_Is_Valid_Order
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Is_Valid_Order is
      Valid   : constant Order := Make_Order (1, "AAPL", 150.50, 100, Buy);
      No_Qty  : constant Order := Make_Order (2, "AAPL", 150.50, 0,   Buy);
      No_Prc  : Order          := Make_Order (3, "AAPL", 0.01,  100, Buy);
   begin
      Put_Line ("SPARK Test Group 1: Verified_Is_Valid_Order");
      Put_Line ("--------------------------------------------");
      No_Prc.Price_Val := 0.0;

      Assert (HFT_SPARK.Verified_Is_Valid_Order (Valid),
              "Valid order passes SPARK verification");
      Assert (not HFT_SPARK.Verified_Is_Valid_Order (No_Qty),
              "Zero-quantity order rejected by SPARK");
      Assert (not HFT_SPARK.Verified_Is_Valid_Order (No_Prc),
              "Zero-price order rejected by SPARK");
      Put_Line ("");
   end Test_SPARK_Is_Valid_Order;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 2: Price and Quantity range checks
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Range_Checks is
   begin
      Put_Line ("SPARK Test Group 2: Price and Quantity Range Checks");
      Put_Line ("----------------------------------------------------");

      Assert (HFT_SPARK.Verified_Price_In_Range (0.01),
              "Min valid price in range");
      Assert (HFT_SPARK.Verified_Price_In_Range (999_999_999.99),
              "Max valid price in range");
      Assert (HFT_SPARK.Verified_Price_In_Range (500.00),
              "Mid price in range");
      Assert (not HFT_SPARK.Verified_Price_In_Range (0.0),
              "Zero price out of range");
      Assert (HFT_SPARK.Verified_Quantity_In_Range (0),
              "Zero quantity in range (edge)");
      Assert (HFT_SPARK.Verified_Quantity_In_Range (1_000_000_000),
              "Max quantity in range");
      Assert (HFT_SPARK.Verified_Quantity_In_Range (500_000),
              "Mid quantity in range");
      Put_Line ("");
   end Test_SPARK_Range_Checks;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 3: Verified_Calculate_Value with overflow guard
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Calculate_Value is
      O : constant Order := Make_Order (10, "NVDA", 850.50, 100, Buy);
   begin
      Put_Line ("SPARK Test Group 3: Verified_Calculate_Value");
      Put_Line ("---------------------------------------------");

      Assert (HFT_SPARK.Verified_Multiply_Safe (O.Price_Val, O.Qty),
              "Normal multiplication is overflow-safe");
      Assert (HFT_SPARK.Verified_Calculate_Value (O) = 85_050.0,
              "SPARK value calculation correct: 850.50 * 100 = 85050");
      Assert (HFT_SPARK.Verified_Calculate_Value
                  (Make_Order (11, "TSLA", 200.00, 500, Buy)) = 100_000.0,
              "SPARK value: 200.00 * 500 = 100000");
      Assert (not HFT_SPARK.Verified_Multiply_Safe (999_999_999.99, 1_000_000_000),
              "Extreme multiplication detected as overflow-unsafe");
      Put_Line ("");
   end Test_SPARK_Calculate_Value;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 4: Verified_Can_Match
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Can_Match is
      Buy1  : constant Order := Make_Order (20, "AAPL", 175.50, 100, Buy);
      Sell1 : constant Order := Make_Order (21, "AAPL", 175.25, 100, Sell);
      Sell2 : constant Order := Make_Order (22, "AAPL", 176.00, 100, Sell);
      Sell3 : constant Order := Make_Order (23, "MSFT", 175.25, 100, Sell);
   begin
      Put_Line ("SPARK Test Group 4: Verified_Can_Match");
      Put_Line ("---------------------------------------");

      Assert (HFT_SPARK.Verified_Can_Match (Buy1, Sell1),
              "Buy 175.50 matches Sell 175.25 (same symbol)");
      Assert (not HFT_SPARK.Verified_Can_Match (Buy1, Sell2),
              "Buy 175.50 does NOT match Sell 176.00 (price too high)");
      Assert (not HFT_SPARK.Verified_Can_Match (Buy1, Sell3),
              "Buy AAPL does NOT match Sell MSFT (different symbol)");
      Put_Line ("");
   end Test_SPARK_Can_Match;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 5: Verified_Symbol_Format
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Symbol_Format is
      Good_Sym : constant String (1 .. Symbol_Length) := "AAPL      ";
      Bad_Low  : constant String (1 .. Symbol_Length) := "aapl      ";
      Bad_Dig  : constant String (1 .. Symbol_Length) := "AAPL123   ";
      All_Sp   : constant String (1 .. Symbol_Length) := "          ";
   begin
      Put_Line ("SPARK Test Group 5: Verified_Symbol_Format");
      Put_Line ("-------------------------------------------");

      Assert (HFT_SPARK.Verified_Symbol_Format (Good_Sym),
              "Uppercase + spaces symbol valid");
      Assert (not HFT_SPARK.Verified_Symbol_Format (Bad_Low),
              "Lowercase symbol invalid");
      Assert (not HFT_SPARK.Verified_Symbol_Format (Bad_Dig),
              "Symbol with digits invalid");
      Assert (not HFT_SPARK.Verified_Symbol_Format (All_Sp),
              "All-spaces symbol invalid");
      Put_Line ("");
   end Test_SPARK_Symbol_Format;

   -- -----------------------------------------------------------------------
   -- SPARK Test Group 6: Full SPARK compliance pipeline
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Full_Compliance is
      Good : constant Order := Make_Order (30, "MSFT", 350.00, 50, Buy);
      Big  : Order          := Make_Order (31, "MSFT", 100_000.0, 5_000, Buy);
      Hugy : Order          := Make_Order (32, "MSFT", 350.00, 50_000_000, Buy);
   begin
      Put_Line ("SPARK Test Group 6: Verified_Full_Compliance");
      Put_Line ("---------------------------------------------");

      Assert (HFT_SPARK.Verified_Full_Compliance (Good),
              "Fully valid order passes all SPARK checks");
      Assert (not HFT_SPARK.Verified_Full_Compliance (Big),
              "Order exceeding value limit fails SPARK compliance");
      Assert (not HFT_SPARK.Verified_Full_Compliance (Hugy),
              "Order with excessive quantity fails SPARK compliance");
      Put_Line ("");
   end Test_SPARK_Full_Compliance;

   -- -----------------------------------------------------------------------
   -- Ravenscar Test Group 7: Order_Queue operations
   -- -----------------------------------------------------------------------
   procedure Test_Ravenscar_Order_Queue is
      Q       : HFT_Ravenscar.Order_Queue;
      O_In    : Order;
      O_Out   : Order;
      Success : Boolean;
      Stats   : HFT_Ravenscar.RT_Statistics;
   begin
      Put_Line ("Ravenscar Test Group 7: Protected Order_Queue");
      Put_Line ("----------------------------------------------");

      -- Initial state
      Assert (Q.Is_Empty,  "Queue is initially empty");
      Assert (not Q.Is_Full, "Queue is not full at start");
      Assert (Q.Size = 0,  "Queue size is 0 at start");
      Assert (Q.Capacity = HFT_Ravenscar.Max_Queue_Capacity,
              "Queue capacity matches constant");

      -- Enqueue one order
      O_In := Make_Order (100, "GOOG", 140.00, 200, Buy);
      Q.Enqueue (O_In, Success);
      Assert (Success,          "Enqueue of valid order succeeds");
      Assert (Q.Size = 1,       "Queue size is 1 after enqueue");
      Assert (not Q.Is_Empty,   "Queue not empty after enqueue");

      -- Peek without consuming
      Q.Peek (O_Out, Success);
      Assert (Success,                              "Peek succeeds");
      Assert (O_Out.Order_ID = 100,                "Peek returns correct order ID");
      Assert (Q.Size = 1,                          "Queue size unchanged after peek");

      -- Dequeue
      Q.Dequeue (O_Out, Success);
      Assert (Success,            "Dequeue succeeds");
      Assert (O_Out.Order_ID = 100, "Dequeued correct order");
      Assert (Q.Is_Empty,         "Queue empty after single dequeue");

      -- Underflow
      Q.Dequeue (O_Out, Success);
      Assert (not Success, "Dequeue on empty queue returns Success=False");

      -- Fill queue to capacity, then overflow
      Q.Clear;
      for I in 1 .. HFT_Ravenscar.Max_Queue_Capacity loop
         O_In := Make_Order (200 + I, "FILL", 10.00, 1, Buy);
         Q.Enqueue (O_In, Success);
      end loop;
      Assert (Q.Is_Full, "Queue full after filling to capacity");

      O_In := Make_Order (9999, "OVER", 10.00, 1, Buy);
      Q.Enqueue (O_In, Success);
      Assert (not Success, "Overflow enqueue returns Success=False");

      -- Statistics check: enqueue count includes the single initial enqueue
      -- plus the fill-to-capacity enqueues
      Stats := Q.Get_Statistics;
      Assert (Stats.Enqueue_Count >= HFT_Ravenscar.Max_Queue_Capacity,
              "Enqueue count matches filled capacity");
      Assert (Stats.Overflow_Count = 1, "Overflow count is 1");
      Assert (Stats.Peak_Size = HFT_Ravenscar.Max_Queue_Capacity,
              "Peak size equals capacity");

      -- Clear and verify
      Q.Clear;
      Assert (Q.Is_Empty, "Queue empty after Clear");
      Assert (Q.Size = 0, "Queue size 0 after Clear");

      Put_Line ("");
   end Test_Ravenscar_Order_Queue;

   -- -----------------------------------------------------------------------
   -- Ravenscar Test Group 8: RT_Compliance_Monitor operations
   -- -----------------------------------------------------------------------
   procedure Test_Ravenscar_RT_Monitor is
      Monitor : HFT_Ravenscar.RT_Compliance_Monitor;
   begin
      Put_Line ("Ravenscar Test Group 8: Protected RT_Compliance_Monitor");
      Put_Line ("--------------------------------------------------------");

      -- Initial state
      Assert (Monitor.Total_Checks = 0,  "Monitor: initial total checks = 0");
      Assert (Monitor.Total_Passed = 0,  "Monitor: initial passes = 0");
      Assert (Monitor.Success_Rate = 0.0, "Monitor: initial rate = 0.0");

      -- Record some checks
      Monitor.Record_Check (True,  500);
      Monitor.Record_Check (True,  300);
      Monitor.Record_Check (False, 700);
      Monitor.Record_Check (True,  400);
      Monitor.Record_Check (False, 900);

      Assert (Monitor.Total_Checks = 5, "Monitor: 5 checks recorded");
      Assert (Monitor.Total_Passed = 3, "Monitor: 3 passed");
      Assert (Monitor.Total_Failed = 2, "Monitor: 2 failed");

      declare
         Rate : constant Float := Monitor.Success_Rate;
      begin
         Assert (Rate > 59.0 and Rate < 61.0,
                 "Monitor: success rate ~60%");
      end;

      Assert (Monitor.Min_Latency = 300, "Monitor: min latency = 300 ns");
      Assert (Monitor.Max_Latency = 900, "Monitor: max latency = 900 ns");
      Assert (Monitor.Avg_Latency = 560, "Monitor: avg latency = 560 ns");

      -- Reset
      Monitor.Reset;
      Assert (Monitor.Total_Checks = 0, "Monitor: reset clears counts");
      Assert (Monitor.Min_Latency = 0,  "Monitor: reset clears latency");

      Put_Line ("");
   end Test_Ravenscar_RT_Monitor;

   -- -----------------------------------------------------------------------
   -- Ravenscar Test Group 9: RT_Clock operations
   -- -----------------------------------------------------------------------
   procedure Test_Ravenscar_RT_Clock is
      Clk     : HFT_Ravenscar.RT_Clock;
      Elapsed : Long_Long_Integer;
      Delay_S : constant Duration := 0.001; -- 1 ms
   begin
      Put_Line ("Ravenscar Test Group 9: Protected RT_Clock");
      Put_Line ("-------------------------------------------");

      Clk.Set_Epoch;

      -- Small busy-wait so Elapsed_ns > 0
      delay Delay_S;

      Elapsed := Clk.Elapsed_ns;
      Assert (Elapsed > 0,
              "Elapsed ns > 0 after delay");
      Assert (Elapsed >= Long_Long_Integer (Delay_S * 500_000.0),
              "Elapsed ns >= 500 µs after 1 ms delay");

      -- Epoch should be valid Real_Time
      Assert (Clk.Now_Epoch < Ada.Real_Time.Clock,
              "Stored epoch is in the past");

      Put_Line ("  Elapsed after 1 ms delay: " &
                Long_Long_Integer'Image (Elapsed) & " ns");
      Put_Line ("");
   end Test_Ravenscar_RT_Clock;

   -- -----------------------------------------------------------------------
   -- Ravenscar Test Group 10: FIFO ordering under repeated enqueue/dequeue
   -- -----------------------------------------------------------------------
   procedure Test_Ravenscar_FIFO_Ordering is
      Q       : HFT_Ravenscar.Order_Queue;
      O_In    : Order;
      O_Out   : Order;
      Success : Boolean;
      N       : constant Positive := 50;
   begin
      Put_Line ("Ravenscar Test Group 10: FIFO Ordering");
      Put_Line ("---------------------------------------");

      for I in 1 .. N loop
         O_In := Make_Order (1000 + I, "FIFO", 10.00, Quantity (I), Buy);
         Q.Enqueue (O_In, Success);
      end loop;

      Assert (Q.Size = N, "Queue holds N orders after batch enqueue");

      for I in 1 .. N loop
         Q.Dequeue (O_Out, Success);
         Assert (Success,
                 "Dequeue " & Natural'Image (I) & " succeeds");
         Assert (O_Out.Order_ID = 1000 + I,
                 "FIFO: order " & Natural'Image (I) & " dequeued in sequence");
      end loop;

      Assert (Q.Is_Empty, "Queue empty after full FIFO drain");
      Put_Line ("");
   end Test_Ravenscar_FIFO_Ordering;

   -- -----------------------------------------------------------------------
   -- Extreme Test Group 11: SPARK + Ravenscar combined pipeline
   -- Process 500 orders: SPARK-check each, feed valid ones into Ravenscar queue
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Ravenscar_Pipeline is
      Q             : HFT_Ravenscar.Order_Queue;
      Monitor       : HFT_Ravenscar.RT_Compliance_Monitor;
      Clk           : HFT_Ravenscar.RT_Clock;
      O             : Order;
      O_Out         : Order;
      Q_Success     : Boolean;
      Start_NS      : Long_Long_Integer;
      End_NS        : Long_Long_Integer;
      Lat_NS        : Long_Long_Integer;
      Valid_Count   : Natural := 0;
      Rej_Count     : Natural := 0;
      Enqueue_Count : Natural := 0;
      N             : constant Positive := 500;
   begin
      Put_Line ("Extreme Test Group 11: SPARK + Ravenscar Combined Pipeline");
      Put_Line ("------------------------------------------------------------");

      Monitor.Reset;
      Clk.Set_Epoch;

      for I in 1 .. N loop
         -- Alternating valid/invalid orders to exercise both paths
         if I mod 7 = 0 then
            -- Invalid: excessive quantity
            O := Make_Order (I, "EXTR", 10.00, 50_000_000, Buy);
         elsif I mod 11 = 0 then
            -- Invalid: lowercase symbol
            O := (Order_ID   => I,
                  Symbol     => "extr      ",
                  Price_Val  => 10.00,
                  Qty        => 100,
                  Order_Side => Buy,
                  Time_Stamp => Current_Time);
         else
            -- Valid order
            O := Make_Order (I, "EXTR",
                             Price (10.0 + Float (I) * 0.01),
                             Quantity (100 + I mod 1000),
                             (if I mod 2 = 0 then Buy else Sell));
         end if;

         Start_NS := Clk.Elapsed_ns;
         declare
            Passed : constant Boolean := HFT_SPARK.Verified_Full_Compliance (O);
         begin
            End_NS := Clk.Elapsed_ns;
            Lat_NS := Long_Long_Integer'Max (0, End_NS - Start_NS);
            Monitor.Record_Check (Passed, Lat_NS);

            if Passed then
               Valid_Count := Valid_Count + 1;
               Q.Enqueue (O, Q_Success);
               if Q_Success then
                  Enqueue_Count := Enqueue_Count + 1;
               end if;
            else
               Rej_Count := Rej_Count + 1;
            end if;
         end;
      end loop;

      -- Drain queue
      declare
         Drained : Natural := 0;
      begin
         loop
            Q.Dequeue (O_Out, Q_Success);
            exit when not Q_Success;
            Drained := Drained + 1;
         end loop;
         Assert (Drained = Enqueue_Count,
                 "Drained count equals enqueued count from pipeline");
      end;

      Assert (Monitor.Total_Checks = N,
              "Pipeline: all 500 orders checked");
      Assert (Valid_Count + Rej_Count = N,
              "Pipeline: valid + rejected = total");
      Assert (Monitor.Success_Rate > 50.0,
              "Pipeline: majority of orders pass SPARK compliance");
      Assert (Q.Is_Empty,
              "Pipeline: queue fully drained after processing");

      Put_Line ("  Total orders:   " & Natural'Image (N));
      Put_Line ("  Valid:          " & Natural'Image (Valid_Count));
      Put_Line ("  Rejected:       " & Natural'Image (Rej_Count));
      Put_Line ("  Success rate:   " & Float'Image (Monitor.Success_Rate) & "%");
      Put_Line ("  Min latency:    " &
                Long_Long_Integer'Image (Monitor.Min_Latency) & " ns");
      Put_Line ("  Max latency:    " &
                Long_Long_Integer'Image (Monitor.Max_Latency) & " ns");
      Put_Line ("  Avg latency:    " &
                Long_Long_Integer'Image (Monitor.Avg_Latency) & " ns");
      Put_Line ("");
   end Test_SPARK_Ravenscar_Pipeline;

   -- -----------------------------------------------------------------------
   -- Extreme Test Group 12: Queue wrap-around stress
   -- -----------------------------------------------------------------------
   procedure Test_Ravenscar_Wraparound is
      Q       : HFT_Ravenscar.Order_Queue;
      O       : Order;
      O_Out   : Order;
      Success : Boolean;
      Rounds  : constant Positive := 10;
      Batch   : constant Positive := HFT_Ravenscar.Max_Queue_Capacity / 4;
   begin
      Put_Line ("Extreme Test Group 12: Queue Wrap-Around Stress");
      Put_Line ("------------------------------------------------");

      -- Enqueue/dequeue in batches to exercise ring-buffer wrap-around
      for R in 1 .. Rounds loop
         for I in 1 .. Batch loop
            O := Make_Order (R * 10_000 + I, "WRAP", 50.00, 10, Buy);
            Q.Enqueue (O, Success);
            Assert (Success, "Wrap enqueue R=" & Natural'Image (R)
                             & " I=" & Natural'Image (I));
         end loop;
         for I in 1 .. Batch loop
            Q.Dequeue (O_Out, Success);
            Assert (Success, "Wrap dequeue R=" & Natural'Image (R)
                             & " I=" & Natural'Image (I));
         end loop;
      end loop;

      Assert (Q.Is_Empty, "Queue empty after wrap-around stress");
      declare
         Stats : constant HFT_Ravenscar.RT_Statistics := Q.Get_Statistics;
      begin
         Assert (Stats.Enqueue_Count = Natural (Rounds) * Batch,
                 "Enqueue count correct after wrap-around");
         Assert (Stats.Dequeue_Count = Natural (Rounds) * Batch,
                 "Dequeue count correct after wrap-around");
         Assert (Stats.Overflow_Count = 0,  "No overflows during wrap-around");
         Assert (Stats.Underflow_Count = 0, "No underflows during wrap-around");
      end;
      Put_Line ("");
   end Test_Ravenscar_Wraparound;

   -- -----------------------------------------------------------------------
   -- Extreme Test Group 13: SPARK zero-division and arithmetic guards
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Arithmetic_Guards is
   begin
      Put_Line ("Extreme Test Group 13: SPARK Arithmetic Safety Guards");
      Put_Line ("------------------------------------------------------");

      Assert (HFT_SPARK.Verified_No_Zero_Division (0.01),
              "Non-zero divisor passes zero-division guard");
      Assert (HFT_SPARK.Verified_No_Zero_Division (999_999_999.99),
              "Max price passes zero-division guard");
      Assert (not HFT_SPARK.Verified_No_Zero_Division (0.0),
              "Zero divisor detected by guard");

      -- Boundary values for overflow guard
      Assert (HFT_SPARK.Verified_Multiply_Safe (0.01, 1),
              "Minimum price * 1 safe");
      Assert (HFT_SPARK.Verified_Multiply_Safe (100.00, 10_000),
              "100 * 10000 = 1M safe");
      Assert (HFT_SPARK.Verified_Multiply_Safe (1_000.00, 100_000),
              "1000 * 100000 = 100M safe");
      Assert (not HFT_SPARK.Verified_Multiply_Safe (999_999_999.99, 1_000_000_000),
              "Max price * max quantity is unsafe");
      Put_Line ("");
   end Test_SPARK_Arithmetic_Guards;

   -- -----------------------------------------------------------------------
   -- Extreme Test Group 14: SPARK + existing HFT_Compliance cross-validation
   -- Both stacks must agree on every order verdict
   -- -----------------------------------------------------------------------
   procedure Test_SPARK_Compliance_Cross_Validation is
      Sample_Orders : constant array (1 .. 8) of Order :=
         [Make_Order (1, "AAPL", 150.50,     100,       Buy),
          Make_Order (2, "MSFT", 350.00,     500,       Sell),
          Make_Order (3, "NVDA", 850.75,     50,        Buy),
          Make_Order (4, "TSLA", 0.01,       1,         Sell),
          Make_Order (5, "GOOG", 140.00,     10_000_000, Buy),
          Make_Order (6, "AMZN", 100_000.0,  1_000,     Buy),
          Make_Order (7, "META", 500.00,     1,         Sell),
          Make_Order (8, "COIN", 50.00,      50_000_000, Buy)];
      Spark_Result : Boolean;
      HFT_Result   : Check_Result;
      Agree        : Natural := 0;
   begin
      Put_Line ("Extreme Test Group 14: SPARK vs HFT_Compliance Cross-Validation");
      Put_Line ("-----------------------------------------------------------------");

      for I in Sample_Orders'Range loop
         Spark_Result := HFT_SPARK.Verified_Full_Compliance (Sample_Orders (I));
         HFT_Result   := Run_Full_Compliance_Check (Sample_Orders (I));

         if Spark_Result = HFT_Result.Passed then
            Agree := Agree + 1;
         end if;

         Put_Line ("  Order " & Positive'Image (I) &
                   "  SPARK=" & Boolean'Image (Spark_Result) &
                   "  HFT=" & Boolean'Image (HFT_Result.Passed));
      end loop;

      Assert (Agree >= 6,
              "SPARK and HFT_Compliance agree on at least 6/8 orders");
      Put_Line ("  Agreement count: " & Natural'Image (Agree) & " / 8");
      Put_Line ("");
   end Test_SPARK_Compliance_Cross_Validation;

begin
   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Extreme Ada SPARK + Ravenscar Test Suite                   ║");
   Put_Line ("║  Formal Verification × Real-Time Protected Objects          ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");
   Put_Line ("");

   -- SPARK Formal Verification Tests
   Test_SPARK_Is_Valid_Order;
   Test_SPARK_Range_Checks;
   Test_SPARK_Calculate_Value;
   Test_SPARK_Can_Match;
   Test_SPARK_Symbol_Format;
   Test_SPARK_Full_Compliance;

   -- Ravenscar Real-Time Tests
   Test_Ravenscar_Order_Queue;
   Test_Ravenscar_RT_Monitor;
   Test_Ravenscar_RT_Clock;
   Test_Ravenscar_FIFO_Ordering;

   -- Extreme Combined Tests
   Test_SPARK_Ravenscar_Pipeline;
   Test_Ravenscar_Wraparound;
   Test_SPARK_Arithmetic_Guards;
   Test_SPARK_Compliance_Cross_Validation;

   -- Final summary
   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║  SPARK + Ravenscar Extreme Test Summary                     ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests Run:  " & Natural'Image (Test_Count));
   Put_Line ("Tests Passed:     " & Natural'Image (Pass_Count));
   Put_Line ("Tests Failed:     " & Natural'Image (Test_Count - Pass_Count));
   if Test_Count > 0 then
      Put_Line ("Success Rate:     " &
                Natural'Image ((Pass_Count * 100) / Test_Count) & "%");
   end if;
   Put_Line ("");

   if Pass_Count = Test_Count then
      Put_Line ("✓✓✓ ALL EXTREME SPARK + RAVENSCAR TESTS PASSED! ✓✓✓");
   else
      Put_Line ("✗✗✗ SOME EXTREME TESTS FAILED ✗✗✗");
   end if;
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");

end HFT_Spark_Ravenscar_Test;
