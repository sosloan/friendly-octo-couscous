-- SPARK Formal Verification Package Body for HFT Engine
pragma Ada_2022;

package body HFT_SPARK
   with SPARK_Mode => On
is

   -- Ghost predicate body (only called in proof contexts)
   function Is_Well_Formed (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Qty > 0
             and O.Price_Val >= Min_Valid_Price
             and O.Price_Val <= Max_Valid_Price
             and O.Order_ID > 0
             and (for all C of O.Symbol => C in 'A' .. 'Z' | ' ');
   end Is_Well_Formed;

   function Verified_Is_Valid_Order (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0;
   end Verified_Is_Valid_Order;

   function Verified_Price_In_Range (P : HFT_Engine.Price) return Boolean is
   begin
      return P >= Min_Valid_Price and P <= Max_Valid_Price;
   end Verified_Price_In_Range;

   function Verified_Quantity_In_Range (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q >= 0 and Q <= 1_000_000_000;
   end Verified_Quantity_In_Range;

   function Verified_Calculate_Value (O : HFT_Engine.Order) return HFT_Engine.Price is
   begin
      return O.Price_Val * HFT_Engine.Price (O.Qty);
   end Verified_Calculate_Value;

   function Verified_Multiply_Safe (
      P : HFT_Engine.Price;
      Q : HFT_Engine.Quantity
   ) return Boolean is
   begin
      return Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last);
   end Verified_Multiply_Safe;

   function Verified_Can_Match (
      Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order
   ) return Boolean is
   begin
      return Buy_Order.Price_Val >= Sell_Order.Price_Val
             and Buy_Order.Symbol = Sell_Order.Symbol;
   end Verified_Can_Match;

   function Verified_Symbol_Format (Symbol : String) return Boolean is
   begin
      for C of Symbol loop
         if not (C in 'A' .. 'Z' | ' ') then
            return False;
         end if;
      end loop;
      -- At least one non-space character
      for C of Symbol loop
         if C /= ' ' then
            return True;
         end if;
      end loop;
      return False;
   end Verified_Symbol_Format;

   function Verified_Order_Value_Within_Limit (O : HFT_Engine.Order) return Boolean is
      Val : constant HFT_Engine.Price := Verified_Calculate_Value (O);
   begin
      return Val <= Max_Order_Value;
   end Verified_Order_Value_Within_Limit;

   function Verified_Order_Size_Reasonable (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q >= 1 and Q <= Max_Reasonable_Qty;
   end Verified_Order_Size_Reasonable;

   function Verified_No_Zero_Division (Divisor : HFT_Engine.Price) return Boolean is
   begin
      return Divisor /= 0.0;
   end Verified_No_Zero_Division;

   function Verified_Full_Compliance (O : HFT_Engine.Order) return Boolean is
      Safe_Multiply : constant Boolean :=
         Verified_Multiply_Safe (O.Price_Val, O.Qty);
   begin
      if not Verified_Is_Valid_Order (O)           then return False; end if;
      if not Verified_Price_In_Range (O.Price_Val) then return False; end if;
      if not Verified_Quantity_In_Range (O.Qty)    then return False; end if;
      if not Verified_Symbol_Format (O.Symbol)     then return False; end if;
      if not Safe_Multiply                         then return False; end if;
      if not Verified_Order_Value_Within_Limit (O) then return False; end if;
      if not Verified_Order_Size_Reasonable (O.Qty) then return False; end if;
      return True;
   end Verified_Full_Compliance;

end HFT_SPARK;
