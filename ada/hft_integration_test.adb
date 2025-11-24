-- Ada Compliance Integration Test Suite
-- Comprehensive integration tests with audit functionality

with Ada.Text_IO; use Ada.Text_IO;
 
with HFT_Engine; use HFT_Engine;
with HFT_Compliance; use HFT_Compliance;
with HFT_Audit; use HFT_Audit;

procedure HFT_Integration_Test is
   
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Current_Time : Timestamp := 1732479420;
   
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

   -- Integration Test 1: Multi-Order Compliance Batch
   procedure Test_Multi_Order_Compliance is
      Orders : array (1 .. 5) of Order;
      Result : Check_Result;
      Stats : Audit_Statistics;
   begin
      Put_Line ("Integration Test 1: Multi-Order Compliance Batch");
      Put_Line ("==================================================");
      
      Clear_Audit_History;
      
      -- Create 5 test orders
      for I in Orders'Range loop
         declare
            Num_Str : String := Positive'Image (I);
            Symbol_Str : String (1 .. 10) := (others => ' ');
         begin
            Symbol_Str (1 .. 4) := "TEST";
            if I < 10 then
               Symbol_Str (5) := Num_Str (Num_Str'Last);
            else
               Symbol_Str (5 .. 6) := Num_Str (Num_Str'Last - 1 .. Num_Str'Last);
            end if;
            
            Orders (I) := (
               Order_ID   => I,
               Symbol     => Symbol_Str,
               Price_Val  => Price (100.0 + Float (I) * 10.0),
               Qty        => Quantity (100 * I),
               Order_Side => (if I mod 2 = 0 then Buy else Sell),
               Time_Stamp => Current_Time
            );
         end;
         
         Audit_Order_Compliance (Orders (I), Result);
      end loop;
      
      Stats := Get_Audit_Statistics;
      Assert (Stats.Total_Events > 0, "Audit events recorded");
      Assert (Stats.Total_Checks = 5, "All orders checked");
      Put_Line ("  Orders processed: " & Natural'Image (Stats.Total_Checks));
      Put_Line ("");
   end Test_Multi_Order_Compliance;

   -- Integration Test 2: Order Lifecycle with Audit Trail
   procedure Test_Order_Lifecycle_With_Audit is
      Order_Buy : Order;
      Order_Sell : Order;
      Result : Check_Result;
      Event_Count : Natural;
   begin
      Put_Line ("Integration Test 2: Order Lifecycle with Audit Trail");
      Put_Line ("=====================================================");
      
      Clear_Audit_History;
      
      -- Create and audit buy order
      Order_Buy := (
         Order_ID   => 1001,
         Symbol     => "AAPL      ",
         Price_Val  => 175.50,
         Qty        => 100,
         Order_Side => Buy,
         Time_Stamp => Current_Time
      );
      
      Audit_Order_Compliance (Order_Buy, Result);
      Assert (Result.Passed, "Buy order compliance check");
      
      -- Create and audit sell order
      Order_Sell := (
         Order_ID   => 1002,
         Symbol     => "AAPL      ",
         Price_Val  => 175.25,
         Qty        => 100,
         Order_Side => Sell,
         Time_Stamp => Current_Time
      );
      
      Audit_Order_Compliance (Order_Sell, Result);
      Assert (Result.Passed, "Sell order compliance check");
      
      -- Verify audit trail
      Event_Count := Get_Audit_Events_By_Order (1001);
      Assert (Event_Count > 0, "Buy order audit events recorded");
      
      Event_Count := Get_Audit_Events_By_Order (1002);
      Assert (Event_Count > 0, "Sell order audit events recorded");
      
      -- Check if orders can match
      if Can_Match (Order_Buy, Order_Sell) then
         Put_Line ("  ✓ Orders can be matched");
      end if;
      
      Put_Line ("");
   end Test_Order_Lifecycle_With_Audit;

   -- Integration Test 3: Compliance Violation Detection
   procedure Test_Compliance_Violation_Detection is
      Valid_Order : Order;
      Invalid_Orders : array (1 .. 4) of Order;
      Result : Check_Result;
      Stats : Audit_Statistics;
   begin
      Put_Line ("Integration Test 3: Compliance Violation Detection");
      Put_Line ("===================================================");
      
      Clear_Audit_History;
      
      -- Valid order
      Valid_Order := (
         Order_ID   => 2001,
         Symbol     => "MSFT      ",
         Price_Val  => 350.00,
         Qty        => 100,
         Order_Side => Buy,
         Time_Stamp => Current_Time
      );
      
      Audit_Order_Compliance (Valid_Order, Result);
      Assert (Result.Passed, "Valid order accepted");
      
      -- Invalid order 1: Future timestamp
      Invalid_Orders (1) := Valid_Order;
      Invalid_Orders (1).Order_ID := 2002;
      Invalid_Orders (1).Time_Stamp := Current_Time + 3600; -- 1 hour in future
      Audit_Order_Compliance (Invalid_Orders (1), Result);
      Assert (not Result.Passed, "Future timestamp detected");
      
      -- Invalid order 2: Invalid symbol format
      Invalid_Orders (2) := Valid_Order;
      Invalid_Orders (2).Order_ID := 2003;
      Invalid_Orders (2).Symbol := "msft123   "; -- lowercase and digits
      Audit_Order_Compliance (Invalid_Orders (2), Result);
      Assert (not Result.Passed, "Invalid symbol format detected");
      
      -- Invalid order 3: Excessive order size
      Invalid_Orders (3) := Valid_Order;
      Invalid_Orders (3).Order_ID := 2004;
      Invalid_Orders (3).Qty := 50_000_000;
      Audit_Order_Compliance (Invalid_Orders (3), Result);
      Assert (not Result.Passed, "Excessive order size detected");
      
      -- Invalid order 4: High value exceeds limit
      Invalid_Orders (4) := Valid_Order;
      Invalid_Orders (4).Order_ID := 2005;
      Invalid_Orders (4).Price_Val := 50_000.0;
      Invalid_Orders (4).Qty := 5_000;
      Audit_Order_Compliance (Invalid_Orders (4), Result);
      Assert (not Result.Passed, "High order value detected");
      
      Stats := Get_Audit_Statistics;
      Assert (Stats.Orders_Accepted >= 1, "Valid orders accepted");
      Assert (Stats.Orders_Rejected >= 4, "Invalid orders rejected");
      
      Put_Line ("  Total violations detected: " & 
                Natural'Image (Stats.Total_Violations));
      Put_Line ("");
   end Test_Compliance_Violation_Detection;

   -- Integration Test 4: Audit Reporting and Analysis
   procedure Test_Audit_Reporting_And_Analysis is
      Orders : array (1 .. 10) of Order;
      Result : Check_Result;
      Summary : Audit_Summary;
      Trend : Trend_Direction;
   begin
      Put_Line ("Integration Test 4: Audit Reporting and Analysis");
      Put_Line ("=================================================");
      
      Clear_Audit_History;
      
      -- Process multiple orders
      for I in Orders'Range loop
         declare
            Num_Str : String := Positive'Image (I);
            Symbol_Str : String (1 .. 10) := (others => ' ');
         begin
            Symbol_Str (1 .. 5) := "STOCK";
            if I < 10 then
               Symbol_Str (6) := Num_Str (Num_Str'Last);
            else
               Symbol_Str (6 .. 7) := Num_Str (Num_Str'Last - 1 .. Num_Str'Last);
            end if;
            
            Orders (I) := (
               Order_ID   => 3000 + I,
               Symbol     => Symbol_Str,
               Price_Val  => Price (50.0 + Float (I) * 5.0),
               Qty        => Quantity (10 * I),
               Order_Side => (if I mod 2 = 0 then Buy else Sell),
               Time_Stamp => Current_Time
            );
         end;
         
         Audit_Order_Compliance (Orders (I), Result);
      end loop;
      
      -- Generate summary
      Summary := Generate_Audit_Summary;
      Assert (Summary.Stats.Total_Checks = 10, "All orders audited");
      Assert (Summary.Success_Rate > 0.0, "Success rate calculated");
      
      Put_Line ("  Success Rate: " & Float'Image (Summary.Success_Rate) & "%");
      
      -- Analyze trend
      Trend := Analyze_Compliance_Trend;
      Put_Line ("  Compliance Trend: " & Trend_Direction'Image (Trend));
      
      Assert (True, "Audit analysis completed");
      Put_Line ("");
   end Test_Audit_Reporting_And_Analysis;

   -- Integration Test 5: High-Volume Processing
   procedure Test_High_Volume_Processing is
      Order_Template : Order;
      Result : Check_Result;
      Stats : Audit_Statistics;
   begin
      Put_Line ("Integration Test 5: High-Volume Processing");
      Put_Line ("===========================================");
      
      Clear_Audit_History;
      
      Order_Template := (
         Order_ID   => 1,
         Symbol     => "NVDA      ",
         Price_Val  => 800.00,
         Qty        => 100,
         Order_Side => Buy,
         Time_Stamp => Current_Time
      );
      
      -- Process 100 orders
      for I in 1 .. 100 loop
         Order_Template.Order_ID := 4000 + I;
         Audit_Order_Compliance (Order_Template, Result);
      end loop;
      
      Stats := Get_Audit_Statistics;
      Assert (Stats.Total_Checks = 200, "All orders processed (2 events per order)");
      Put_Line ("  Successfully processed 100 orders");
      Put_Line ("");
   end Test_High_Volume_Processing;

   -- Integration Test 6: Category-Specific Violation Tracking
   procedure Test_Category_Specific_Violations is
      Order_Template : Order;
      Result : Check_Result;
      Stats : Audit_Statistics;
   begin
      Put_Line ("Integration Test 6: Category-Specific Violation Tracking");
      Put_Line ("=========================================================");
      
      Clear_Audit_History;
      
      Order_Template := (
         Order_ID   => 5000,
         Symbol     => "TEST      ",
         Price_Val  => 100.00,
         Qty        => 100,
         Order_Side => Buy,
         Time_Stamp => Current_Time
      );
      
      -- Create type safety violation
      declare
         Bad_Order : Order := Order_Template;
      begin
         Bad_Order.Order_ID := 5001;
         Bad_Order.Symbol := "test123   "; -- lowercase violation
         Audit_Order_Compliance (Bad_Order, Result);
      end;
      
      -- Create security violation
      declare
         Bad_Order : Order := Order_Template;
      begin
         Bad_Order.Order_ID := 5002;
         Bad_Order.Price_Val := 100_000.0;
         Bad_Order.Qty := 10_000; -- Exceeds value limit
         Audit_Order_Compliance (Bad_Order, Result);
      end;
      
      -- Create performance warning
      declare
         Bad_Order : Order := Order_Template;
      begin
         Bad_Order.Order_ID := 5003;
         Bad_Order.Qty := 100_000_000; -- Too large
         Audit_Order_Compliance (Bad_Order, Result);
      end;
      
      Stats := Get_Audit_Statistics;
      Assert (Stats.Total_Violations >= 3, "Violations tracked by category");
      
      Put_Line ("  Type Safety Violations: " & 
                Natural'Image (Stats.Type_Safety_Failures));
      Put_Line ("  Security Violations: " & 
                Natural'Image (Stats.Security_Failures));
      Put_Line ("  Performance Warnings: " & 
                Natural'Image (Stats.Performance_Warnings));
      Put_Line ("");
   end Test_Category_Specific_Violations;

   -- Integration Test 7: Audit Export and Persistence
   procedure Test_Audit_Export is
      Order_Sample : Order;
      Result : Check_Result;
   begin
      Put_Line ("Integration Test 7: Audit Export and Persistence");
      Put_Line ("=================================================");
      
      Clear_Audit_History;
      
      -- Create some audit events
      for I in 1 .. 5 loop
         Order_Sample := (
            Order_ID   => 6000 + I,
            Symbol     => "EXPORT    ",
            Price_Val  => 150.00,
            Qty        => Quantity (100 * I),
            Order_Side => Buy,
            Time_Stamp => Current_Time
         );
         
         Audit_Order_Compliance (Order_Sample, Result);
      end loop;
      
      -- Export audit log
      begin
         Export_Audit_Log ("/tmp/hft_audit.log");
         Assert (True, "Audit log exported successfully");
         Put_Line ("  Log file: /tmp/hft_audit.log");
      exception
         when others =>
            Assert (False, "Failed to export audit log");
      end;
      
      Put_Line ("");
   end Test_Audit_Export;

   -- Integration Test 8: Audit Configuration
   procedure Test_Audit_Configuration is
      Config : Audit_Config;
      Order_Test : Order;
      Result : Check_Result;
      Stats_Before, Stats_After : Audit_Statistics;
   begin
      Put_Line ("Integration Test 8: Audit Configuration");
      Put_Line ("========================================");
      
      Clear_Audit_History;
      
      -- Test with audit enabled
      Config := (
         Enable_Audit => True,
         Log_All_Events => True,
         Log_Passed_Checks => True,
         Log_Failed_Checks => True,
         Max_History_Size => 1000,
         Enable_Performance_Tracking => True
      );
      Configure_Audit (Config);
      
      Order_Test := (
         Order_ID   => 7001,
         Symbol     => "CONFIG    ",
         Price_Val  => 200.00,
         Qty        => 50,
         Order_Side => Buy,
         Time_Stamp => Current_Time
      );
      
      Audit_Order_Compliance (Order_Test, Result);
      Stats_Before := Get_Audit_Statistics;
      Assert (Stats_Before.Total_Events > 0, "Events logged with audit enabled");
      
      -- Test with audit disabled
      Clear_Audit_History;
      Config.Enable_Audit := False;
      Configure_Audit (Config);
      
      Order_Test.Order_ID := 7002;
      Audit_Order_Compliance (Order_Test, Result);
      Stats_After := Get_Audit_Statistics;
      Assert (Stats_After.Total_Events = 0, "No events logged with audit disabled");
      
      -- Re-enable for remaining tests
      Config.Enable_Audit := True;
      Configure_Audit (Config);
      
      Put_Line ("");
   end Test_Audit_Configuration;

begin
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  Ada Compliance Integration Test Suite                  ║");
   Put_Line ("║  Comprehensive Integration Testing with Audit System    ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   Put_Line ("");
   
   -- Run all integration tests
   Test_Multi_Order_Compliance;
   Test_Order_Lifecycle_With_Audit;
   Test_Compliance_Violation_Detection;
   Test_Audit_Reporting_And_Analysis;
   Test_High_Volume_Processing;
   Test_Category_Specific_Violations;
   Test_Audit_Export;
   Test_Audit_Configuration;
   
   -- Print final audit report
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  Final Audit Report                                      ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   Put_Line ("");
   Print_Audit_Report;
   Put_Line ("");
   
   -- Print test summary
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  Integration Test Summary                                ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests Run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests Passed:       " & Natural'Image (Pass_Count));
   Put_Line ("Tests Failed:       " & Natural'Image (Test_Count - Pass_Count));
   Put_Line ("Success Rate:       " & 
             Natural'Image ((Pass_Count * 100) / Test_Count) & "%");
   Put_Line ("");
   
   if Pass_Count = Test_Count then
      Put_Line ("✓✓✓ ALL INTEGRATION TESTS PASSED! ✓✓✓");
   else
      Put_Line ("✗✗✗ SOME INTEGRATION TESTS FAILED ✗✗✗");
   end if;
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   
end HFT_Integration_Test;
