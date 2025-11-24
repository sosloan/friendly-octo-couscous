-- Main Ada HFT Application
-- Demonstrates the type-safe HFT engine with comprehensive compliance checks

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with HFT_Engine; use HFT_Engine;
with HFT_Compliance; use HFT_Compliance;

procedure HFT_Main is
   Buy_Ord  : Order;
   Sell_Ord : Order;
   Current_Time : Time := Clock;
   Compliance_Result : Check_Result;
   Stats : Compliance_Stats;
begin
   Put_Line ("=== Ada HFT Engine Demo with Compliance Checks ===");
   Put_Line ("");
   
   -- Create a buy order
   Buy_Ord := (
      Order_ID   => 1,
      Symbol     => "AAPL      ",
      Price_Val  => 150.50,
      Qty        => 100,
      Order_Side => Buy,
      Timestamp  => Current_Time
   );
   
   -- Create a sell order
   Sell_Ord := (
      Order_ID   => 2,
      Symbol     => "AAPL      ",
      Price_Val  => 150.25,
      Qty        => 100,
      Order_Side => Sell,
      Timestamp  => Current_Time
   );
   
   Put_Line ("Buy Order: " & Buy_Ord.Symbol & 
             " @ $" & Price'Image (Buy_Ord.Price_Val) &
             " Qty:" & Quantity'Image (Buy_Ord.Qty));
   Put_Line ("Sell Order: " & Sell_Ord.Symbol & 
             " @ $" & Price'Image (Sell_Ord.Price_Val) &
             " Qty:" & Quantity'Image (Sell_Ord.Qty));
   Put_Line ("");
   
   -- Run compliance checks on buy order
   Put_Line ("--- Running Compliance Checks on Buy Order ---");
   Compliance_Result := Run_Full_Compliance_Check (Buy_Ord);
   if Compliance_Result.Passed then
      Put_Line ("✓ Buy order passed all compliance checks");
   else
      Put_Line ("✗ Buy order failed compliance checks");
   end if;
   Put_Line ("");
   
   -- Run compliance checks on sell order
   Put_Line ("--- Running Compliance Checks on Sell Order ---");
   Compliance_Result := Run_Full_Compliance_Check (Sell_Ord);
   if Compliance_Result.Passed then
      Put_Line ("✓ Sell order passed all compliance checks");
   else
      Put_Line ("✗ Sell order failed compliance checks");
   end if;
   Put_Line ("");
   
   -- Detailed compliance report
   Put_Line ("--- Detailed Compliance Report for Buy Order ---");
   Print_Compliance_Report (Buy_Ord);
   Put_Line ("");
   
   -- Get compliance statistics
   Stats := Get_Compliance_Statistics (Buy_Ord);
   Put_Line ("--- Compliance Statistics ---");
   Put_Line ("Total Checks:      " & Natural'Image (Stats.Total_Checks));
   Put_Line ("Passed Checks:     " & Natural'Image (Stats.Passed_Checks));
   Put_Line ("Failed Checks:     " & Natural'Image (Stats.Failed_Checks));
   Put_Line ("Type Safety:       " & Natural'Image (Stats.Type_Safety_Pass) & " passed");
   Put_Line ("Contract Validity: " & Natural'Image (Stats.Contract_Pass) & " passed");
   Put_Line ("Range Safety:      " & Natural'Image (Stats.Range_Safety_Pass) & " passed");
   Put_Line ("Coding Standards:  " & Natural'Image (Stats.Coding_Std_Pass) & " passed");
   Put_Line ("Security:          " & Natural'Image (Stats.Security_Pass) & " passed");
   Put_Line ("Performance:       " & Natural'Image (Stats.Performance_Pass) & " passed");
   Put_Line ("");
   
   -- Traditional order validation and matching
   Put_Line ("--- Traditional Order Processing ---");
   if Is_Valid_Order (Buy_Ord) and Is_Valid_Order (Sell_Ord) then
      Put_Line ("✓ Both orders are valid");
      
      if Can_Match (Buy_Ord, Sell_Ord) then
         Put_Line ("✓ Orders can be matched!");
         Put_Line ("  Trade Value: $" & Price'Image (Calculate_Value (Sell_Ord)));
      else
         Put_Line ("✗ Orders cannot be matched");
      end if;
   else
      Put_Line ("✗ Invalid orders detected");
   end if;
   
   Put_Line ("");
   Put_Line ("=== Ada Engine with Compliance Checks Running ===");
end HFT_Main;
