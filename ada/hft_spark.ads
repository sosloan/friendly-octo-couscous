-- SPARK Formal Verification Package for HFT Engine
-- Provides formally-verified, proof-ready interfaces over the HFT core
pragma Ada_2022;

with HFT_Engine; use HFT_Engine;

package HFT_SPARK
   with SPARK_Mode => On
is

   -- -----------------------------------------------------------------------
   -- Boundary constants used in contracts and implementations
   -- -----------------------------------------------------------------------
   Min_Valid_Price    : constant HFT_Engine.Price    := 0.01;
   Max_Valid_Price    : constant HFT_Engine.Price    := 999_999_999.99;
   Max_Order_Value    : constant HFT_Engine.Price    := 100_000_000.0;
   Max_Reasonable_Qty : constant HFT_Engine.Quantity := 10_000_000;

   -- -----------------------------------------------------------------------
   -- Ghost predicate: formally specifies a well-formed order (proof-only)
   -- -----------------------------------------------------------------------
   function Is_Well_Formed (O : HFT_Engine.Order) return Boolean
      with Ghost,
           Global => null;

   -- -----------------------------------------------------------------------
   -- SPARK-verified order validation
   -- -----------------------------------------------------------------------
   function Verified_Is_Valid_Order (O : HFT_Engine.Order) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_Is_Valid_Order'Result =
                           (O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0);

   -- -----------------------------------------------------------------------
   -- SPARK-verified price range check
   -- -----------------------------------------------------------------------
   function Verified_Price_In_Range (P : HFT_Engine.Price) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_Price_In_Range'Result =
                           (P >= Min_Valid_Price and P <= Max_Valid_Price);

   -- -----------------------------------------------------------------------
   -- SPARK-verified quantity range check
   -- -----------------------------------------------------------------------
   function Verified_Quantity_In_Range (Q : HFT_Engine.Quantity) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_Quantity_In_Range'Result =
                           (Q >= 0 and Q <= 1_000_000_000);

   -- -----------------------------------------------------------------------
   -- SPARK-verified order-value calculation (overflow-safe)
   -- -----------------------------------------------------------------------
   function Verified_Calculate_Value (O : HFT_Engine.Order) return HFT_Engine.Price
      with SPARK_Mode => On,
           Global     => null,
           Pre        => Verified_Is_Valid_Order (O)
                         and Float (O.Price_Val) * Float (O.Qty) <=
                               Float (HFT_Engine.Price'Last),
           Post       => Verified_Calculate_Value'Result >= 0.0;

   -- -----------------------------------------------------------------------
   -- SPARK-verified overflow guard for multiplication
   -- -----------------------------------------------------------------------
   function Verified_Multiply_Safe (
      P : HFT_Engine.Price;
      Q : HFT_Engine.Quantity
   ) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_Multiply_Safe'Result =
                           (Float (P) * Float (Q) <=
                              Float (HFT_Engine.Price'Last));

   -- -----------------------------------------------------------------------
   -- SPARK-verified order matching
   -- -----------------------------------------------------------------------
   function Verified_Can_Match (
      Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order
   ) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Pre        => Verified_Is_Valid_Order (Buy_Order)
                         and Verified_Is_Valid_Order (Sell_Order)
                         and Buy_Order.Order_Side  = HFT_Engine.Buy
                         and Sell_Order.Order_Side = HFT_Engine.Sell,
           Post       => (if Verified_Can_Match'Result then
                            Buy_Order.Price_Val >= Sell_Order.Price_Val
                            and Buy_Order.Symbol = Sell_Order.Symbol);

   -- -----------------------------------------------------------------------
   -- SPARK-verified symbol format check
   -- -----------------------------------------------------------------------
   function Verified_Symbol_Format (Symbol : String) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Pre        => Symbol'Length = HFT_Engine.Symbol_Length,
           Post       => (if Verified_Symbol_Format'Result then
                            (for all C of Symbol => C in 'A' .. 'Z' | ' '));

   -- -----------------------------------------------------------------------
   -- SPARK-verified order-value limit check (security boundary)
   -- -----------------------------------------------------------------------
   function Verified_Order_Value_Within_Limit (O : HFT_Engine.Order) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Pre        => Verified_Is_Valid_Order (O)
                         and Float (O.Price_Val) * Float (O.Qty) <=
                               Float (HFT_Engine.Price'Last),
           Post       => (if Verified_Order_Value_Within_Limit'Result then
                            Verified_Calculate_Value (O) <= Max_Order_Value);

   -- -----------------------------------------------------------------------
   -- SPARK-verified reasonable order-size check
   -- -----------------------------------------------------------------------
   function Verified_Order_Size_Reasonable (Q : HFT_Engine.Quantity) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_Order_Size_Reasonable'Result =
                           (Q >= 1 and Q <= Max_Reasonable_Qty);

   -- -----------------------------------------------------------------------
   -- SPARK-verified zero-division guard
   -- -----------------------------------------------------------------------
   function Verified_No_Zero_Division (Divisor : HFT_Engine.Price) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => Verified_No_Zero_Division'Result = (Divisor /= 0.0);

   -- -----------------------------------------------------------------------
   -- SPARK full-order compliance check
   -- -----------------------------------------------------------------------
   function Verified_Full_Compliance (O : HFT_Engine.Order) return Boolean
      with SPARK_Mode => On,
           Global     => null,
           Post       => (if Verified_Full_Compliance'Result then
                            Verified_Is_Valid_Order (O)
                            and Verified_Price_In_Range (O.Price_Val)
                            and Verified_Quantity_In_Range (O.Qty)
                            and Verified_Symbol_Format (O.Symbol)
                            and Verified_Order_Size_Reasonable (O.Qty));

end HFT_SPARK;
