-- Ada SPARK Formally-Verified HFT Compliance Interfaces
pragma Ada_2022;
pragma SPARK_Mode (On);

with HFT_Engine; use HFT_Engine;

package HFT_Spark is
   pragma Preelaborate;

   Min_Valid_Price    : constant HFT_Engine.Price := 0.01;
   Max_Valid_Price    : constant HFT_Engine.Price := 999_999_999.99;
   Max_Order_Value    : constant HFT_Engine.Price := 100_000_000.0;
   Max_Reasonable_Qty : constant HFT_Engine.Quantity := 10_000_000;

   function Verified_Is_Valid_Order
     (O : HFT_Engine.Order) return Boolean
      with Global => null,
           Post => Verified_Is_Valid_Order'Result =
             (O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0);

   function Verified_Price_In_Range
     (P : HFT_Engine.Price) return Boolean
      with Global => null,
           Post => Verified_Price_In_Range'Result =
             (P >= Min_Valid_Price and P <= Max_Valid_Price);

   function Verified_Quantity_In_Range
     (Q : HFT_Engine.Quantity) return Boolean
      with Global => null;

   function Verified_Calculate_Value
     (O : HFT_Engine.Order) return HFT_Engine.Price
      with Global => null,
           Pre => Verified_Is_Valid_Order (O)
             and Float (O.Price_Val) * Float (O.Qty) <=
               Float (HFT_Engine.Price'Last);

   function Verified_Multiply_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean
      with Global => null;

   function Verified_Can_Match
     (Buy_Order, Sell_Order : HFT_Engine.Order) return Boolean
      with Global => null,
           Pre => Verified_Is_Valid_Order (Buy_Order)
             and Verified_Is_Valid_Order (Sell_Order)
             and Buy_Order.Order_Side = HFT_Engine.Buy
             and Sell_Order.Order_Side = HFT_Engine.Sell;

   function Verified_Symbol_Format (Symbol : String) return Boolean
      with Global => null,
           Pre => Symbol'Length = HFT_Engine.Symbol_Length;

   function Verified_Order_Value_Within_Limit
     (O : HFT_Engine.Order) return Boolean
      with Global => null,
           Pre => Verified_Is_Valid_Order (O)
             and Verified_Multiply_Safe (O.Price_Val, O.Qty);

   function Verified_Order_Size_Reasonable
     (Q : HFT_Engine.Quantity) return Boolean
      with Global => null;

   function Verified_No_Zero_Division
     (Divisor : HFT_Engine.Price) return Boolean
      with Global => null;

   function Verified_Full_Compliance
     (O : HFT_Engine.Order) return Boolean
      with Global => null;

   function Spark_Price_In_Range (P : HFT_Engine.Price) return Boolean
      with Global => null,
           Post   => Spark_Price_In_Range'Result =
                       (P >= 0.01 and P <= 999_999_999.99);

   function Spark_Quantity_Positive (Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => Spark_Quantity_Positive'Result = (Q > 0);

   function Spark_Quantity_In_Range (Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => Spark_Quantity_In_Range'Result =
                       (Q > 0 and Q <= 1_000_000_000);

   function Spark_Price_Nonzero (P : HFT_Engine.Price) return Boolean
      with Global => null,
           Post   => Spark_Price_Nonzero'Result = (P /= 0.0);

   function Spark_Timestamp_Initialized
     (T : HFT_Engine.Timestamp) return Boolean
      with Global => null,
           Post   => Spark_Timestamp_Initialized'Result = (T /= 0);

   function Spark_Side_Valid (S : HFT_Engine.Side) return Boolean
      with Global => null,
           Post   => Spark_Side_Valid'Result = True;

   function Spark_Symbol_Uppercase (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length > 0,
           Post   => Spark_Symbol_Uppercase'Result =
                       (for all C of Symbol => C in 'A' .. 'Z' | ' ');

   function Spark_Symbol_Has_Content (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length > 0,
           Post   => Spark_Symbol_Has_Content'Result =
                       (for some C of Symbol => C /= ' ');

   function Spark_Symbol_Valid (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length = HFT_Engine.Symbol_Length,
           Post   => Spark_Symbol_Valid'Result =
                       (Spark_Symbol_Uppercase (Symbol)
                        and Spark_Symbol_Has_Content (Symbol));

   function Spark_Mul_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => (if Spark_Mul_Safe'Result then
                        Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last));

   function Spark_Add_Safe
     (P1 : HFT_Engine.Price; P2 : HFT_Engine.Price) return Boolean
      with Global => null,
           Post   => (if Spark_Add_Safe'Result then
                        P1 + P2 <= 999_999_999.99);

   function Spark_Value_Under_Limit
     (P     : HFT_Engine.Price;
      Q     : HFT_Engine.Quantity;
      Limit : HFT_Engine.Price) return Boolean
      with Global => null,
           Pre    => Limit > 0.0,
           Post   => (if Spark_Value_Under_Limit'Result then
                        Float (P) * Float (Q) <= Float (Limit));

   function Spark_Order_Invariant (O : HFT_Engine.Order) return Boolean
      with Global => null,
           Post   => Spark_Order_Invariant'Result =
                       (O.Price_Val > 0.0
                        and O.Qty > 0
                        and O.Order_ID > 0
                        and O.Time_Stamp > 0);

   function Spark_Orders_Match
     (Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order) return Boolean
      with Global => null,
           Pre    => Buy_Order.Order_Side = HFT_Engine.Buy
                     and Sell_Order.Order_Side = HFT_Engine.Sell,
           Post   => Spark_Orders_Match'Result =
                       (Buy_Order.Price_Val >= Sell_Order.Price_Val
                        and Buy_Order.Symbol = Sell_Order.Symbol);

   type Spark_Result is record
      Passed         : Boolean := False;
      Type_Safety    : Boolean := False;
      Contract_Valid : Boolean := False;
      Range_Safe     : Boolean := False;
      Symbol_Ok      : Boolean := False;
      Value_Ok       : Boolean := False;
      NIL_Safe       : Boolean := False;
   end record;

   function Spark_Full_Check (O : HFT_Engine.Order) return Spark_Result
      with Global => null,
           Post   => (if Spark_Full_Check'Result.Passed then
                        Spark_Order_Invariant (O)
                        and Spark_Symbol_Valid (O.Symbol));
end HFT_Spark;
