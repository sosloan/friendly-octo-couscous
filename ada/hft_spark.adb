-- Ada SPARK Formally-Verified HFT Compliance Implementation
pragma Ada_2022;
pragma SPARK_Mode (On);

package body HFT_Spark is
   function Verified_Is_Valid_Order
     (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0;
   end Verified_Is_Valid_Order;

   function Verified_Price_In_Range
     (P : HFT_Engine.Price) return Boolean is
   begin
      return P >= Min_Valid_Price and P <= Max_Valid_Price;
   end Verified_Price_In_Range;

   function Verified_Quantity_In_Range
     (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q <= HFT_Engine.Quantity'Last;
   end Verified_Quantity_In_Range;

   function Verified_Calculate_Value
     (O : HFT_Engine.Order) return HFT_Engine.Price is
   begin
      return O.Price_Val * HFT_Engine.Price (O.Qty);
   end Verified_Calculate_Value;

   function Verified_Multiply_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last);
   end Verified_Multiply_Safe;

   function Verified_Can_Match
     (Buy_Order, Sell_Order : HFT_Engine.Order) return Boolean is
   begin
      return Buy_Order.Price_Val >= Sell_Order.Price_Val
        and Buy_Order.Symbol = Sell_Order.Symbol;
   end Verified_Can_Match;

   function Verified_Symbol_Format (Symbol : String) return Boolean is
   begin
      return Spark_Symbol_Uppercase (Symbol)
        and Spark_Symbol_Has_Content (Symbol);
   end Verified_Symbol_Format;

   function Verified_Order_Value_Within_Limit
     (O : HFT_Engine.Order) return Boolean is
   begin
      return Verified_Calculate_Value (O) <= Max_Order_Value;
   end Verified_Order_Value_Within_Limit;

   function Verified_Order_Size_Reasonable
     (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q >= 1 and Q <= Max_Reasonable_Qty;
   end Verified_Order_Size_Reasonable;

   function Verified_No_Zero_Division
     (Divisor : HFT_Engine.Price) return Boolean is
   begin
      return Divisor /= 0.0;
   end Verified_No_Zero_Division;

   function Verified_Full_Compliance
     (O : HFT_Engine.Order) return Boolean is
   begin
      return Verified_Is_Valid_Order (O)
        and then Verified_Price_In_Range (O.Price_Val)
        and then Verified_Quantity_In_Range (O.Qty)
        and then Verified_Symbol_Format (O.Symbol)
        and then Verified_Multiply_Safe (O.Price_Val, O.Qty)
        and then Verified_Order_Value_Within_Limit (O)
        and then Verified_Order_Size_Reasonable (O.Qty);
   end Verified_Full_Compliance;

   function Spark_Price_In_Range (P : HFT_Engine.Price) return Boolean is
   begin
      return P >= 0.01 and P <= 999_999_999.99;
   end Spark_Price_In_Range;

   function Spark_Quantity_Positive
     (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q > 0;
   end Spark_Quantity_Positive;

   function Spark_Quantity_In_Range
     (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q > 0 and Q <= 1_000_000_000;
   end Spark_Quantity_In_Range;

   function Spark_Price_Nonzero
     (P : HFT_Engine.Price) return Boolean is
   begin
      return P /= 0.0;
   end Spark_Price_Nonzero;

   function Spark_Timestamp_Initialized
     (T : HFT_Engine.Timestamp) return Boolean is
   begin
      return T /= 0;
   end Spark_Timestamp_Initialized;

   function Spark_Side_Valid (S : HFT_Engine.Side) return Boolean is
      pragma Unreferenced (S);
   begin
      return True;
   end Spark_Side_Valid;

   function Spark_Symbol_Uppercase (Symbol : String) return Boolean is
   begin
      for C of Symbol loop
         if not (C in 'A' .. 'Z' | ' ') then
            return False;
         end if;
      end loop;
      return True;
   end Spark_Symbol_Uppercase;

   function Spark_Symbol_Has_Content (Symbol : String) return Boolean is
   begin
      for C of Symbol loop
         if C /= ' ' then
            return True;
         end if;
      end loop;
      return False;
   end Spark_Symbol_Has_Content;

   function Spark_Symbol_Valid (Symbol : String) return Boolean is
   begin
      return Spark_Symbol_Uppercase (Symbol)
        and Spark_Symbol_Has_Content (Symbol);
   end Spark_Symbol_Valid;

   function Spark_Mul_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last);
   end Spark_Mul_Safe;

   function Spark_Add_Safe
     (P1 : HFT_Engine.Price; P2 : HFT_Engine.Price) return Boolean is
   begin
      return P1 + P2 <= 999_999_999.99;
   end Spark_Add_Safe;

   function Spark_Value_Under_Limit
     (P     : HFT_Engine.Price;
      Q     : HFT_Engine.Quantity;
      Limit : HFT_Engine.Price) return Boolean is
   begin
      return Float (P) * Float (Q) <= Float (Limit);
   end Spark_Value_Under_Limit;

   function Spark_Order_Invariant (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Price_Val > 0.0
        and O.Qty > 0
        and O.Order_ID > 0
        and O.Time_Stamp > 0;
   end Spark_Order_Invariant;

   function Spark_Orders_Match
     (Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order) return Boolean is
   begin
      return Buy_Order.Price_Val >= Sell_Order.Price_Val
        and Buy_Order.Symbol = Sell_Order.Symbol;
   end Spark_Orders_Match;

   function Spark_Full_Check (O : HFT_Engine.Order) return Spark_Result is
      R : Spark_Result;
   begin
      R.Type_Safety := Spark_Price_In_Range (O.Price_Val)
        and Spark_Quantity_In_Range (O.Qty);
      R.Contract_Valid := Spark_Order_Invariant (O);
      R.Range_Safe := Spark_Mul_Safe (O.Price_Val, O.Qty);
      R.Symbol_Ok := Spark_Symbol_Valid (O.Symbol);
      R.Value_Ok := Spark_Value_Under_Limit
        (O.Price_Val, O.Qty, 100_000_000.0);
      R.NIL_Safe := Spark_Price_Nonzero (O.Price_Val)
        and Spark_Quantity_Positive (O.Qty)
        and Spark_Timestamp_Initialized (O.Time_Stamp);
      R.Passed := R.Type_Safety and R.Contract_Valid and R.Range_Safe
        and R.Symbol_Ok and R.Value_Ok and R.NIL_Safe;
      return R;
   end Spark_Full_Check;
end HFT_Spark;
