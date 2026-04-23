-- Ada SPARK Formally-Verified HFT Compliance Implementation
pragma Ada_2022;
pragma SPARK_Mode (On);

package body HFT_Spark is

   function Spark_Price_In_Range (P : HFT_Engine.Price) return Boolean is
   begin
      return P >= 0.01 and P <= 999_999_999.99;
   end Spark_Price_In_Range;

   function Spark_Quantity_Positive (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q > 0;
   end Spark_Quantity_Positive;

   function Spark_Quantity_In_Range (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q > 0 and Q <= 1_000_000_000;
   end Spark_Quantity_In_Range;

   function Spark_Price_Nonzero (P : HFT_Engine.Price) return Boolean is
   begin
      return P /= 0.0;
   end Spark_Price_Nonzero;

   function Spark_Timestamp_Initialized (T : HFT_Engine.Timestamp) return Boolean is
   begin
      return T /= 0;
   end Spark_Timestamp_Initialized;

   function Spark_Side_Valid (S : HFT_Engine.Side) return Boolean is
   begin
      return True;
   end Spark_Side_Valid;

   -- ====================================================================
   -- Symbol checks with explicit loop bodies (GNATprove-friendly)
   -- ====================================================================

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

   -- ====================================================================
   -- Arithmetic safety
   -- ====================================================================

   function Spark_Mul_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last);
   end Spark_Mul_Safe;

   function Spark_Add_Safe (P1 : HFT_Engine.Price; P2 : HFT_Engine.Price)
      return Boolean is
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

   -- ====================================================================
   -- Order invariant
   -- ====================================================================

   function Spark_Order_Invariant (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Price_Val > 0.0
             and O.Qty > 0
             and O.Order_ID > 0
             and O.Time_Stamp > 0;
   end Spark_Order_Invariant;

   -- ====================================================================
   -- Order matching
   -- ====================================================================

   function Spark_Orders_Match
     (Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order) return Boolean is
   begin
      return Buy_Order.Price_Val >= Sell_Order.Price_Val
             and Buy_Order.Symbol = Sell_Order.Symbol;
   end Spark_Orders_Match;

   -- ====================================================================
   -- Full SPARK compliance check
   -- ====================================================================

   function Spark_Full_Check (O : HFT_Engine.Order) return Spark_Result is
      R : Spark_Result;
   begin
      R.Type_Safety    := Spark_Price_In_Range (O.Price_Val)
                          and Spark_Quantity_In_Range (O.Qty);

      R.Contract_Valid := Spark_Order_Invariant (O);

      R.Range_Safe     := Spark_Mul_Safe (O.Price_Val, O.Qty);

      R.Symbol_Ok      := Spark_Symbol_Valid (O.Symbol);

      R.Value_Ok       := Spark_Value_Under_Limit (O.Price_Val, O.Qty,
                                                   100_000_000.0);

      R.NIL_Safe       := Spark_Price_Nonzero (O.Price_Val)
                          and Spark_Quantity_Positive (O.Qty)
                          and Spark_Timestamp_Initialized (O.Time_Stamp);

      R.Passed := R.Type_Safety
                  and R.Contract_Valid
                  and R.Range_Safe
                  and R.Symbol_Ok
                  and R.Value_Ok
                  and R.NIL_Safe;

      return R;
   end Spark_Full_Check;

end HFT_Spark;
