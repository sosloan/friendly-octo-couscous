-- Ada Compliance Test Suite
-- Comprehensive tests for Ada compliance checking functionality

with Ada.Text_IO; use Ada.Text_IO;
 
with HFT_Engine; use HFT_Engine;
with HFT_Compliance; use HFT_Compliance;
with HFT_Time_Util;

procedure HFT_Compliance_Test is
   
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
   
   Current_Time : constant Timestamp := Timestamp (HFT_Time_Util.Get_Unix_Timestamp);
   Valid_Order : Order;
   
begin
   Put_Line ("======================================");
   Put_Line ("=== Ada Compliance Test Suite ===");
   Put_Line ("======================================");
   Put_Line ("");
   
   -- Initialize valid test order
   Valid_Order := (
      Order_ID   => 1,
      Symbol     => "AAPL      ",
      Price_Val  => 150.50,
      Qty        => 100,
      Order_Side => Buy,
      Time_Stamp => Current_Time
   );
   
   -- Test 1: Type Safety Checks
   Put_Line ("Test Group 1: Type Safety Checks");
   Put_Line ("----------------------------------");
   Assert (Check_Price_Range (150.50), "Valid price in range");
   Assert (Check_Price_Range (0.01), "Minimum valid price");
   Assert (Check_Price_Range (999_999_999.99), "Maximum valid price");
   Assert (not Check_Price_Range (0.0), "Invalid zero price");
   Assert (Check_Quantity_Range (100), "Valid quantity");
   Assert (Check_Quantity_Range (0), "Zero quantity (edge case)");
   Assert (Check_Quantity_Range (1_000_000_000), "Maximum valid quantity");
   Assert (Check_Order_ID_Valid (1), "Valid order ID");
   Assert (Check_Order_ID_Valid (999_999), "Large valid order ID");
   Put_Line ("");
   
   -- Test 2: Contract Compliance Checks
   Put_Line ("Test Group 2: Contract Compliance Checks");
   Put_Line ("------------------------------------------");
   Assert (Verify_Order_Preconditions (Valid_Order), "Valid order preconditions");
   declare
      Invalid_Order : Order := Valid_Order;
   begin
      Invalid_Order.Qty := 0;
      Assert (not Verify_Order_Preconditions (Invalid_Order), 
              "Invalid order preconditions (zero qty)");
   end;
   declare
      Value : Price := Calculate_Value (Valid_Order);
   begin
      Assert (Verify_Order_Postconditions (Valid_Order, Value),
              "Valid order postconditions");
   end;
   Put_Line ("");
   
   -- Test 3: Range and Overflow Checks
   Put_Line ("Test Group 3: Range and Overflow Checks");
   Put_Line ("-----------------------------------------");
   Assert (Check_Multiplication_Overflow (150.50, 100), 
           "Safe multiplication (normal case)");
   Assert (Check_Multiplication_Overflow (1000.0, 1000), 
           "Safe multiplication (large values)");
   Assert (not Check_Multiplication_Overflow (999_999_999.99, 1_000_000_000),
           "Unsafe multiplication overflow detected");
   Assert (Check_Price_Addition_Safe (100.0, 200.0),
           "Safe price addition");
   Assert (not Check_Price_Addition_Safe (999_999_999.0, 999_999_999.0),
           "Unsafe price addition detected");
   Assert (Check_No_Zero_Division (150.50), "Non-zero divisor");
   Assert (not Check_No_Zero_Division (0.0), "Zero divisor detected");
   Put_Line ("");
   
   -- Test 4: Coding Standards Checks
   Put_Line ("Test Group 4: Coding Standards Checks");
   Put_Line ("---------------------------------------");
   Assert (Check_Symbol_Format ("AAPL      "), "Valid symbol format");
   Assert (Check_Symbol_Format ("GOOGL     "), "Valid symbol with spaces");
   Assert (Check_Symbol_Format ("MSFT      "), "Valid short symbol");
   Assert (not Check_Symbol_Format ("aapl      "), "Invalid lowercase symbol");
   Assert (not Check_Symbol_Format ("AAPL123   "), "Invalid symbol with numbers");
   Assert (not Check_Symbol_Format ("          "), "Invalid all-spaces symbol");
   Assert (Check_Order_Side_Valid (Buy), "Valid Buy side");
   Assert (Check_Order_Side_Valid (Sell), "Valid Sell side");
   Put_Line ("");
   
   -- Test 5: Security Compliance Checks
   Put_Line ("Test Group 5: Security Compliance Checks");
   Put_Line ("------------------------------------------");
   Assert (Check_Order_Value_Limit (Valid_Order), 
           "Order value within limit");
   declare
      High_Value_Order : Order := Valid_Order;
   begin
      High_Value_Order.Price_Val := 50000.0;
      High_Value_Order.Qty := 5000;
      Assert (not Check_Order_Value_Limit (High_Value_Order),
              "Order value exceeds limit detected");
   end;
   Assert (Check_Timestamp_Valid (Valid_Order), "Valid timestamp");
   declare
      Future_Order : Order := Valid_Order;
   begin
      Future_Order.Time_Stamp := Current_Time + 3600; -- 1 hour in the future
      Assert (not Check_Timestamp_Valid (Future_Order),
              "Future timestamp detected");
   end;
   Put_Line ("");
   
   -- Test 6: Performance Compliance Checks
   Put_Line ("Test Group 6: Performance Compliance Checks");
   Put_Line ("---------------------------------------------");
   Assert (Check_Symbol_Length_Optimal ("AAPL      "), 
           "Optimal symbol length");
   Assert (Check_Order_Size_Reasonable (100), 
           "Reasonable order size");
   Assert (Check_Order_Size_Reasonable (1), 
           "Minimum reasonable size");
   Assert (Check_Order_Size_Reasonable (10_000_000),
           "Maximum reasonable size");
   Assert (not Check_Order_Size_Reasonable (0),
           "Too small order size detected");
   declare
      Large_Order : Order := Valid_Order;
   begin
      Large_Order.Qty := 50_000_000;
      Assert (not Check_Order_Size_Reasonable (Large_Order.Qty),
              "Too large order size detected");
   end;
   Put_Line ("");
   
   -- Test 7: Category-Specific Checks
   Put_Line ("Test Group 7: Category-Specific Compliance");
   Put_Line ("--------------------------------------------");
   declare
      Result : Check_Result;
   begin
      Result := Run_Category_Check (Valid_Order, Type_Safety);
      Assert (Result.Passed, "Type Safety category check");
      
      Result := Run_Category_Check (Valid_Order, Contract_Validity);
      Assert (Result.Passed, "Contract Validity category check");
      
      Result := Run_Category_Check (Valid_Order, Range_Safety);
      Assert (Result.Passed, "Range Safety category check");
      
      Result := Run_Category_Check (Valid_Order, Coding_Standards);
      Assert (Result.Passed, "Coding Standards category check");
      
      Result := Run_Category_Check (Valid_Order, Security);
      Assert (Result.Passed, "Security category check");
      
      Result := Run_Category_Check (Valid_Order, Performance);
      Assert (Result.Passed, "Performance category check");
   end;
   Put_Line ("");
   
   -- Test 8: Full Compliance Check
   Put_Line ("Test Group 8: Full Compliance Check");
   Put_Line ("-------------------------------------");
   declare
      Result : Check_Result;
   begin
      Result := Run_Full_Compliance_Check (Valid_Order);
      Assert (Result.Passed, "Full compliance check passes for valid order");
   end;
   
   declare
      Invalid_Order : Order := Valid_Order;
      Result : Check_Result;
   begin
      Invalid_Order.Symbol := "INVALID123"; -- Contains digits, violates format
      Result := Run_Full_Compliance_Check (Invalid_Order);
      Assert (not Result.Passed, 
              "Full compliance check fails for invalid order");
   end;
   Put_Line ("");
   
   -- Test 9: Compliance Statistics
   Put_Line ("Test Group 9: Compliance Statistics");
   Put_Line ("-------------------------------------");
   declare
      Stats : Compliance_Stats;
   begin
      Stats := Get_Compliance_Statistics (Valid_Order);
      Assert (Stats.Total_Checks = 7, "Correct total check count");
      Assert (Stats.Passed_Checks = 7, "All checks passed for valid order");
      Assert (Stats.Failed_Checks = 0, "No failed checks for valid order");
      Put_Line ("  Statistics: " & Natural'Image (Stats.Passed_Checks) & 
                " / " & Natural'Image (Stats.Total_Checks) & " checks passed");
   end;
   Put_Line ("");
   
   -- Test 10: Compliance Report Generation
   Put_Line ("Test Group 10: Compliance Report");
   Put_Line ("----------------------------------");
   Put_Line ("  Generating compliance report for valid order...");
   Put_Line ("");
   Print_Compliance_Report (Valid_Order);
   Put_Line ("");
   
   -- Test Summary
   Put_Line ("======================================");
   Put_Line ("=== Test Summary ===");
   Put_Line ("======================================");
   Put_Line ("Total Tests Run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests Passed:       " & Natural'Image (Pass_Count));
   Put_Line ("Tests Failed:       " & Natural'Image (Test_Count - Pass_Count));
   Put_Line ("Success Rate:       " & 
             Natural'Image ((Pass_Count * 100) / Test_Count) & "%");
   Put_Line ("");
   
   if Pass_Count = Test_Count then
      Put_Line ("✓✓✓ ALL COMPLIANCE TESTS PASSED! ✓✓✓");
   else
      Put_Line ("✗✗✗ SOME COMPLIANCE TESTS FAILED ✗✗✗");
   end if;
   Put_Line ("======================================");
   
end HFT_Compliance_Test;
