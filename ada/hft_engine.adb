-- Ada HFT Engine Implementation
pragma Ada_2022;

package body HFT_Engine is

   function Is_Valid_Order (O : Order) return Boolean is
   begin
      return O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0;
   end Is_Valid_Order;

   function Calculate_Value (O : Order) return Price is
   begin
      return O.Price_Val * Price (O.Qty);
   end Calculate_Value;

   function Can_Match (Buy_Order : Order; Sell_Order : Order) return Boolean is
   begin
      -- Buy price must be >= sell price for a match
      return Buy_Order.Price_Val >= Sell_Order.Price_Val 
         and Buy_Order.Symbol = Sell_Order.Symbol;
   end Can_Match;

end HFT_Engine;
