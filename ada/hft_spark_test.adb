-- SPARK Extreme-Value Test Suite
-- Exercises every formally-verified HFT_Spark contract with boundary,
-- overflow, zero, maximum, and adversarial inputs.
with Ada.Text_IO; use Ada.Text_IO;

with HFT_Engine; use HFT_Engine;
with HFT_Spark;
with HFT_Time_Util;

procedure HFT_Spark_Test is

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

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

   Now : constant Timestamp :=
           Timestamp (HFT_Time_Util.Get_Unix_Timestamp);

   -- Helpers for building test orders quickly
   function Make_Order
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
              Time_Stamp => Now);
   end Make_Order;

begin
   Put_Line ("========================================");
   Put_Line ("=== Ada SPARK Extreme Test Suite    ===");
   Put_Line ("========================================");
   New_Line;

   -- ================================================================
   -- Group 1: Spark_Price_In_Range — boundary & extreme values
   -- ================================================================
   Put_Line ("Group 1: Spark_Price_In_Range");
   Put_Line ("------------------------------");
   Assert (HFT_Spark.Spark_Price_In_Range (0.01),
           "Minimum valid price (0.01)");
   Assert (HFT_Spark.Spark_Price_In_Range (999_999_999.99),
           "Maximum valid price (999_999_999.99)");
   Assert (HFT_Spark.Spark_Price_In_Range (150.50),
           "Typical price");
   Assert (not HFT_Spark.Spark_Price_In_Range (0.0),
           "Zero price rejected");
   Assert (not HFT_Spark.Spark_Price_In_Range (0.00),
           "0.00 price rejected (same as zero)");
   New_Line;

   -- ================================================================
   -- Group 2: Spark_Quantity_In_Range — boundary values
   -- ================================================================
   Put_Line ("Group 2: Spark_Quantity_In_Range");
   Put_Line ("---------------------------------");
   Assert (HFT_Spark.Spark_Quantity_In_Range (1),
           "Minimum valid quantity (1)");
   Assert (HFT_Spark.Spark_Quantity_In_Range (1_000_000_000),
           "Maximum valid quantity (1_000_000_000)");
   Assert (HFT_Spark.Spark_Quantity_In_Range (100),
           "Typical quantity");
   Assert (not HFT_Spark.Spark_Quantity_In_Range (0),
           "Zero quantity rejected");
   New_Line;

   -- ================================================================
   -- Group 3: Spark_Symbol_Valid — format & content extremes
   -- ================================================================
   Put_Line ("Group 3: Spark_Symbol_Valid");
   Put_Line ("----------------------------");
   Assert (HFT_Spark.Spark_Symbol_Valid ("AAPL      "),
           "Typical 4-char symbol");
   Assert (HFT_Spark.Spark_Symbol_Valid ("GOOGL     "),
           "5-char symbol");
   Assert (HFT_Spark.Spark_Symbol_Valid ("A         "),
           "Single-letter symbol (minimum content)");
   Assert (HFT_Spark.Spark_Symbol_Valid ("ABCDEFGHIJ"),
           "Maximum 10-char all-letter symbol");
   Assert (not HFT_Spark.Spark_Symbol_Valid ("aapl      "),
           "Lowercase symbol rejected");
   Assert (not HFT_Spark.Spark_Symbol_Valid ("AAPL123   "),
           "Symbol with digits rejected");
   Assert (not HFT_Spark.Spark_Symbol_Valid ("          "),
           "All-spaces symbol rejected");
   Assert (not HFT_Spark.Spark_Symbol_Valid ("AAPL!     "),
           "Symbol with special character rejected");
   New_Line;

   -- ================================================================
   -- Group 4: Spark_Mul_Safe — overflow boundary
   -- ================================================================
   Put_Line ("Group 4: Spark_Mul_Safe (overflow detection)");
   Put_Line ("---------------------------------------------");
   Assert (HFT_Spark.Spark_Mul_Safe (150.50, 100),
           "Typical mul: 150.50 x 100 safe");
   Assert (HFT_Spark.Spark_Mul_Safe (1_000.0, 1_000_000),
           "1000.00 x 1_000_000 safe");
   Assert (HFT_Spark.Spark_Mul_Safe (0.01, 1),
           "Minimum price x minimum qty safe");
   Assert (HFT_Spark.Spark_Mul_Safe (0.01, 1_000_000_000),
           "Min price x max qty safe");
   Assert (not HFT_Spark.Spark_Mul_Safe (999_999_999.99, 1_000_000_000),
           "Max price x max qty overflows");
   New_Line;

   -- ================================================================
   -- Group 5: Spark_Add_Safe — compliance-range boundary
   -- ================================================================
   Put_Line ("Group 5: Spark_Add_Safe (compliance range)");
   Put_Line ("--------------------------------------------");
   Assert (HFT_Spark.Spark_Add_Safe (100.0, 200.0),
           "100 + 200 safe");
   Assert (HFT_Spark.Spark_Add_Safe (499_999_999.99, 499_999_999.99),
           "Two halves of max sum safe");
   Assert (HFT_Spark.Spark_Add_Safe (0.01, 0.01),
           "Two minimum prices safe");
   Assert (not HFT_Spark.Spark_Add_Safe (999_999_999.0, 999_999_999.0),
           "Two large prices exceed compliance range");
   Assert (not HFT_Spark.Spark_Add_Safe (999_999_999.99, 0.01),
           "Max price + smallest increment unsafe");
   New_Line;

   -- ================================================================
   -- Group 6: Spark_Value_Under_Limit
   -- ================================================================
   Put_Line ("Group 6: Spark_Value_Under_Limit");
   Put_Line ("---------------------------------");
   Assert (HFT_Spark.Spark_Value_Under_Limit (150.50, 100, 100_000_000.0),
           "Typical order under 100M limit");
   Assert (HFT_Spark.Spark_Value_Under_Limit (100_000_000.0, 1, 100_000_000.0),
           "Exactly at 100M limit");
   Assert (not HFT_Spark.Spark_Value_Under_Limit (50_000.0, 5_000, 100_000_000.0),
           "250M order exceeds limit");
   Assert (HFT_Spark.Spark_Value_Under_Limit (0.01, 1, 1.0),
           "Tiny order under small limit");
   New_Line;

   -- ================================================================
   -- Group 7: Spark_Order_Invariant
   -- ================================================================
   Put_Line ("Group 7: Spark_Order_Invariant");
   Put_Line ("-------------------------------");
   Assert (HFT_Spark.Spark_Order_Invariant (
             Make_Order (1, "AAPL      ", 150.50, 100)),
           "Valid order satisfies invariant");
   declare
      Bad : Order := Make_Order (1, "AAPL      ", 0.0, 100);
   begin
      Assert (not HFT_Spark.Spark_Order_Invariant (Bad),
              "Zero price violates invariant");
   end;
   declare
      Bad : Order := Make_Order (1, "AAPL      ", 150.50, 0);
   begin
      Assert (not HFT_Spark.Spark_Order_Invariant (Bad),
              "Zero quantity violates invariant");
   end;
   New_Line;

   -- ================================================================
   -- Group 8: Spark_Orders_Match — matching logic
   -- ================================================================
   Put_Line ("Group 8: Spark_Orders_Match");
   Put_Line ("----------------------------");
   declare
      B : constant Order := Make_Order (1, "AAPL      ", 150.50, 100, Buy);
      S : constant Order := Make_Order (2, "AAPL      ", 150.25, 100, Sell);
   begin
      Assert (HFT_Spark.Spark_Orders_Match (B, S),
              "Buy 150.50 matches Sell 150.25 (same symbol)");
   end;
   declare
      B : constant Order := Make_Order (1, "AAPL      ", 150.00, 100, Buy);
      S : constant Order := Make_Order (2, "AAPL      ", 150.25, 100, Sell);
   begin
      Assert (not HFT_Spark.Spark_Orders_Match (B, S),
              "Buy 150.00 does NOT match Sell 150.25");
   end;
   declare
      B : constant Order := Make_Order (1, "AAPL      ", 150.50, 100, Buy);
      S : constant Order := Make_Order (2, "GOOGL     ", 150.25, 100, Sell);
   begin
      Assert (not HFT_Spark.Spark_Orders_Match (B, S),
              "Different symbols do not match");
   end;
   declare
      B : constant Order := Make_Order (1, "AAPL      ", 150.50, 100, Buy);
      S : constant Order := Make_Order (2, "AAPL      ", 150.50, 100, Sell);
   begin
      Assert (HFT_Spark.Spark_Orders_Match (B, S),
              "Equal prices match (buy == sell)");
   end;
   New_Line;

   -- ================================================================
   -- Group 9: Spark_Full_Check — full SPARK compliance pipeline
   -- ================================================================
   Put_Line ("Group 9: Spark_Full_Check (full pipeline)");
   Put_Line ("------------------------------------------");
   declare
      R : HFT_Spark.Spark_Result;
   begin
      -- Fully valid order
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (1, "NVDA      ", 450.75, 200));
      Assert (R.Passed,        "Valid order: Passed");
      Assert (R.Type_Safety,   "Valid order: Type_Safety");
      Assert (R.Contract_Valid,"Valid order: Contract_Valid");
      Assert (R.Range_Safe,    "Valid order: Range_Safe");
      Assert (R.Symbol_Ok,     "Valid order: Symbol_Ok");
      Assert (R.Value_Ok,      "Valid order: Value_Ok");
      Assert (R.NIL_Safe,      "Valid order: NIL_Safe");

      -- Lowercase symbol
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (2, "tsla      ", 200.0, 50));
      Assert (not R.Passed,    "Lowercase symbol: rejected");
      Assert (not R.Symbol_Ok, "Lowercase symbol: Symbol_Ok = False");

      -- Symbol with digits
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (3, "AAA1111111", 100.0, 100));
      Assert (not R.Passed,    "Symbol with digits: rejected");

      -- Extreme value that would exceed 100M limit
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (4, "MEGA      ", 50_000.0, 5_000));
      Assert (not R.Passed,    "250M order: rejected (value limit)");
      Assert (not R.Value_Ok,  "250M order: Value_Ok = False");

      -- Zero price (fails multiple categories)
      declare
         Bad_Order : Order := Make_Order (5, "TEST      ", 0.01, 100);
      begin
         Bad_Order.Price_Val := 0.0;
         R := HFT_Spark.Spark_Full_Check (Bad_Order);
         Assert (not R.Passed,         "Zero price: rejected");
         Assert (not R.Type_Safety,    "Zero price: Type_Safety = False");
         Assert (not R.Contract_Valid, "Zero price: Contract_Valid = False");
      end;

      -- Maximum-value valid order (should still pass compliance)
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (6, "ZZZZ      ", 0.01, 1));
      Assert (R.Passed, "Minimum-value order passes");

      -- Large but compliant: 999 * 100 = 99_900 < 100M
      R := HFT_Spark.Spark_Full_Check (
             Make_Order (7, "BIGG      ", 999.0, 100));
      Assert (R.Passed, "999 * 100 = 99_900: within limit, passes");
   end;
   New_Line;

   -- ================================================================
   -- Group 10: Side and timestamp edge cases
   -- ================================================================
   Put_Line ("Group 10: Auxiliary SPARK predicates");
   Put_Line ("--------------------------------------");
   Assert (HFT_Spark.Spark_Side_Valid (Buy),  "Buy side valid");
   Assert (HFT_Spark.Spark_Side_Valid (Sell), "Sell side valid");
   Assert (HFT_Spark.Spark_Price_Nonzero (0.01),  "0.01 is non-zero");
   Assert (not HFT_Spark.Spark_Price_Nonzero (0.0), "0.0 is zero");
   Assert (HFT_Spark.Spark_Timestamp_Initialized (1),       "TS 1 initialized");
   Assert (HFT_Spark.Spark_Timestamp_Initialized (Now),     "Current TS initialized");
   Assert (not HFT_Spark.Spark_Timestamp_Initialized (0),   "TS 0 = uninitialized");
   New_Line;

   -- ================================================================
   -- Summary
   -- ================================================================
   Put_Line ("========================================");
   Put_Line ("=== SPARK Test Summary              ===");
   Put_Line ("========================================");
   Put_Line ("Total Tests Run:  " & Natural'Image (Test_Count));
   Put_Line ("Tests Passed:     " & Natural'Image (Pass_Count));
   Put_Line ("Tests Failed:     " & Natural'Image (Test_Count - Pass_Count));
   Put_Line ("Success Rate:     " &
             Natural'Image ((Pass_Count * 100) / Test_Count) & "%");
   New_Line;
   if Pass_Count = Test_Count then
      Put_Line ("✓✓✓ ALL SPARK TESTS PASSED! ✓✓✓");
   else
      Put_Line ("✗✗✗ SOME SPARK TESTS FAILED ✗✗✗");
   end if;
   Put_Line ("========================================");

end HFT_Spark_Test;
