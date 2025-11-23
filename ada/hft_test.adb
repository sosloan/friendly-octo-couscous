-- Ada Test Suite for HFT Engine
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with HFT_Engine; use HFT_Engine;

procedure HFT_Test is
   
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   
   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("✓ PASS: " & Test_Name);
      else
         Put_Line ("✗ FAIL: " & Test_Name);
      end if;
   end Assert;
   
   Current_Time : Time := Clock;
   Valid_Buy : Order;
   Valid_Sell : Order;
   Invalid_Order : Order;
   
begin
   Put_Line ("=== Ada HFT Engine Test Suite ===");
   Put_Line ("");
   
   -- Initialize test orders
   Valid_Buy := (
      Order_ID   => 1,
      Symbol     => "AAPL      ",
      Price_Val  => 150.50,
      Qty        => 100,
      Order_Side => Buy,
      Timestamp  => Current_Time
   );
   
   Valid_Sell := (
      Order_ID   => 2,
      Symbol     => "AAPL      ",
      Price_Val  => 150.25,
      Qty        => 100,
      Order_Side => Sell,
      Timestamp  => Current_Time
   );
   
   Invalid_Order := (
      Order_ID   => 3,
      Symbol     => "TEST      ",
      Price_Val  => 0.0,  -- Invalid: zero price
      Qty        => 0,    -- Invalid: zero quantity
      Order_Side => Buy,
      Timestamp  => Current_Time
   );
   
   Put_Line ("Test 1: Valid Order Validation");
   Assert (Is_Valid_Order (Valid_Buy), "Valid buy order should pass validation");
   Assert (Is_Valid_Order (Valid_Sell), "Valid sell order should pass validation");
   
   Put_Line ("");
   Put_Line ("Test 2: Invalid Order Validation");
   Assert (not Is_Valid_Order (Invalid_Order), "Invalid order should fail validation");
   
   Put_Line ("");
   Put_Line ("Test 3: Order Value Calculation");
   declare
      Expected_Value : Price := 15050.0;  -- 150.50 * 100
      Calculated_Value : Price := Calculate_Value (Valid_Buy);
   begin
      Assert (Calculated_Value = Expected_Value, 
              "Order value calculation should be correct");
   end;
   
   Put_Line ("");
   Put_Line ("Test 4: Order Matching");
   Assert (Can_Match (Valid_Buy, Valid_Sell), 
           "Buy at 150.50 should match sell at 150.25");
   
   Put_Line ("");
   Put_Line ("Test 5: Non-matching Orders");
   declare
      Low_Buy : Order := Valid_Buy;
   begin
      Low_Buy.Price_Val := 150.00;  -- Lower than sell price
      Assert (Can_Match (Low_Buy, Valid_Sell),
              "Buy at 150.00 should still match sell at 150.25");
   end;
   
   Put_Line ("");
   Put_Line ("Test 6: Different Symbols");
   declare
      Different_Symbol : Order := Valid_Sell;
   begin
      Different_Symbol.Symbol := "GOOGL     ";
      Assert (not Can_Match (Valid_Buy, Different_Symbol),
              "Orders with different symbols should not match");
   end;
   
   Put_Line ("");
   Put_Line ("=== Test Summary ===");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Test_Count - Pass_Count));
   
   if Pass_Count = Test_Count then
      Put_Line ("");
      Put_Line ("✓ All tests passed!");
   else
      Put_Line ("");
      Put_Line ("✗ Some tests failed");
   end if;
   
end HFT_Test;
