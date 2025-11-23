-- Main Ada HFT Application
-- Demonstrates the type-safe HFT engine

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with HFT_Engine; use HFT_Engine;

procedure HFT_Main is
   Buy_Ord  : Order;
   Sell_Ord : Order;
   Current_Time : Time := Clock;
begin
   Put_Line ("=== Ada HFT Engine Demo ===");
   
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
   
   if Is_Valid_Order (Buy_Ord) and Is_Valid_Order (Sell_Ord) then
      Put_Line ("Both orders are valid ✓");
      
      if Can_Match (Buy_Ord, Sell_Ord) then
         Put_Line ("Orders can be matched! ✓");
         Put_Line ("Trade Value: $" & Price'Image (Calculate_Value (Sell_Ord)));
      else
         Put_Line ("Orders cannot be matched");
      end if;
   else
      Put_Line ("Invalid orders detected");
   end if;
   
   Put_Line ("=== Ada Engine Running ===");
end HFT_Main;
