-- Comprehensive Ada Compliance Example
-- Demonstrates all compliance checking features in action

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with HFT_Engine; use HFT_Engine;
with HFT_Compliance; use HFT_Compliance;

procedure Compliance_Example is
   Current_Time : Time := Clock;
   
   procedure Demonstrate_Type_Safety is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 1. Type Safety Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 100,
         Symbol     => "TSLA      ",
         Price_Val  => 250.75,
         Qty        => 500,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Checking price range...");
      if Check_Price_Range (Order.Price_Val) then
         Put_Line ("  ✓ Price is within valid range");
      end if;
      
      Put_Line ("Checking quantity range...");
      if Check_Quantity_Range (Order.Qty) then
         Put_Line ("  ✓ Quantity is within valid range");
      end if;
      
      Put_Line ("Checking order ID validity...");
      if Check_Order_ID_Valid (Order.Order_ID) then
         Put_Line ("  ✓ Order ID is valid");
      end if;
      
      Put_Line ("");
   end Demonstrate_Type_Safety;
   
   procedure Demonstrate_Contract_Validity is
      Order : HFT_Engine.Order;
      Value : Price;
   begin
      Put_Line ("=== 2. Contract Validity Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 200,
         Symbol     => "GOOGL     ",
         Price_Val  => 2800.50,
         Qty        => 25,
         Order_Side => Sell,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Verifying preconditions...");
      if Verify_Order_Preconditions (Order) then
         Put_Line ("  ✓ All preconditions satisfied");
         
         Value := HFT_Engine.Calculate_Value (Order);
         Put_Line ("Verifying postconditions...");
         if Verify_Order_Postconditions (Order, Value) then
            Put_Line ("  ✓ All postconditions satisfied");
            Put_Line ("  Order value: $" & Price'Image (Value));
         end if;
      end if;
      
      Put_Line ("");
   end Demonstrate_Contract_Validity;
   
   procedure Demonstrate_Range_Safety is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 3. Range Safety Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 300,
         Symbol     => "AMZN      ",
         Price_Val  => 3500.25,
         Qty        => 100,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Checking multiplication overflow...");
      if Check_Multiplication_Overflow (Order.Price_Val, Order.Qty) then
         Put_Line ("  ✓ Multiplication is safe (no overflow)");
      else
         Put_Line ("  ✗ WARNING: Multiplication would overflow!");
      end if;
      
      Put_Line ("Checking price addition safety...");
      if Check_Price_Addition_Safe (Order.Price_Val, 1000.0) then
         Put_Line ("  ✓ Addition is safe");
      end if;
      
      Put_Line ("Checking for zero division...");
      if Check_No_Zero_Division (Order.Price_Val) then
         Put_Line ("  ✓ No division by zero risk");
      end if;
      
      Put_Line ("");
   end Demonstrate_Range_Safety;
   
   procedure Demonstrate_Coding_Standards is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 4. Coding Standards Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 400,
         Symbol     => "MSFT      ",
         Price_Val  => 350.00,
         Qty        => 200,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Checking symbol format...");
      if Check_Symbol_Format (Order.Symbol) then
         Put_Line ("  ✓ Symbol format is compliant (uppercase)");
      end if;
      
      Put_Line ("Checking order side validity...");
      if Check_Order_Side_Valid (Order.Order_Side) then
         Put_Line ("  ✓ Order side is valid");
      end if;
      
      Put_Line ("");
   end Demonstrate_Coding_Standards;
   
   procedure Demonstrate_Security is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 5. Security Compliance Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 500,
         Symbol     => "NVDA      ",
         Price_Val  => 800.00,
         Qty        => 1000,
         Order_Side => Sell,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Checking order value limit...");
      if Check_Order_Value_Limit (Order) then
         Put_Line ("  ✓ Order value within security limit (max $100M)");
      else
         Put_Line ("  ✗ WARNING: Order value exceeds security limit!");
      end if;
      
      Put_Line ("Checking timestamp validity...");
      if Check_Timestamp_Valid (Order) then
         Put_Line ("  ✓ Timestamp is valid (not future, not too old)");
      end if;
      
      Put_Line ("");
   end Demonstrate_Security;
   
   procedure Demonstrate_Performance is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 6. Performance Compliance Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 600,
         Symbol     => "META      ",
         Price_Val  => 450.50,
         Qty        => 150,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Checking symbol length optimization...");
      if Check_Symbol_Length_Optimal (Order.Symbol) then
         Put_Line ("  ✓ Symbol length is optimal for HFT");
      end if;
      
      Put_Line ("Checking order size reasonableness...");
      if Check_Order_Size_Reasonable (Order.Qty) then
         Put_Line ("  ✓ Order size is reasonable (1-10M range)");
      end if;
      
      Put_Line ("");
   end Demonstrate_Performance;
   
   procedure Demonstrate_Full_Check is
      Order : HFT_Engine.Order;
      Result : Check_Result;
      Stats : Compliance_Stats;
   begin
      Put_Line ("=== 7. Full Compliance Check ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 700,
         Symbol     => "AAPL      ",
         Price_Val  => 175.50,
         Qty        => 100,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Running comprehensive compliance check...");
      Result := Run_Full_Compliance_Check (Order);
      
      if Result.Passed then
         Put_Line ("  ✓✓✓ ORDER FULLY COMPLIANT ✓✓✓");
      else
         Put_Line ("  ✗✗✗ ORDER FAILED COMPLIANCE ✗✗✗");
      end if;
      
      Put_Line ("");
      Put_Line ("Getting compliance statistics...");
      Stats := Get_Compliance_Statistics (Order);
      Put_Line ("  Total checks:  " & Natural'Image (Stats.Total_Checks));
      Put_Line ("  Passed:        " & Natural'Image (Stats.Passed_Checks));
      Put_Line ("  Failed:        " & Natural'Image (Stats.Failed_Checks));
      Put_Line ("  Success rate:  " & 
                Natural'Image ((Stats.Passed_Checks * 100) / Stats.Total_Checks) & "%");
      
      Put_Line ("");
   end Demonstrate_Full_Check;
   
   procedure Demonstrate_Category_Checks is
      Order : HFT_Engine.Order;
      Result : Check_Result;
   begin
      Put_Line ("=== 8. Category-Specific Checks ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 800,
         Symbol     => "NFLX      ",
         Price_Val  => 600.00,
         Qty        => 50,
         Order_Side => Sell,
         Timestamp  => Current_Time
      );
      
      Put_Line ("Running category-specific checks...");
      Put_Line ("");
      
      for Category in Compliance_Category loop
         Result := Run_Category_Check (Order, Category);
         if Result.Passed then
            Put_Line ("  ✓ " & Compliance_Category'Image (Category) & ": PASS");
         else
            Put_Line ("  ✗ " & Compliance_Category'Image (Category) & ": FAIL");
         end if;
      end loop;
      
      Put_Line ("");
   end Demonstrate_Category_Checks;
   
   procedure Demonstrate_Compliance_Report is
      Order : HFT_Engine.Order;
   begin
      Put_Line ("=== 9. Detailed Compliance Report ===");
      Put_Line ("");
      
      Order := (
         Order_ID   => 900,
         Symbol     => "INTC      ",
         Price_Val  => 45.25,
         Qty        => 1000,
         Order_Side => Buy,
         Timestamp  => Current_Time
      );
      
      Print_Compliance_Report (Order);
      Put_Line ("");
   end Demonstrate_Compliance_Report;
   
begin
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  Ada Comprehensive Compliance Checking System Demo      ║");
   Put_Line ("║  High-Frequency Trading Engine Compliance Framework     ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   Put_Line ("");
   
   Demonstrate_Type_Safety;
   Demonstrate_Contract_Validity;
   Demonstrate_Range_Safety;
   Demonstrate_Coding_Standards;
   Demonstrate_Security;
   Demonstrate_Performance;
   Demonstrate_Full_Check;
   Demonstrate_Category_Checks;
   Demonstrate_Compliance_Report;
   
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  All Compliance Features Demonstrated Successfully!     ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   
end Compliance_Example;
