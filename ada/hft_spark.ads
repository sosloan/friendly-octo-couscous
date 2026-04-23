-- Ada SPARK Formally-Verified HFT Compliance Interfaces
-- Provides GNATprove-provable contracts for the HFT engine
-- SPARK 2014 / 2022 subset: no side effects, full postconditions,
-- loop invariants, and Global => null on every subprogram.
pragma Ada_2022;
pragma SPARK_Mode (On);

with HFT_Engine; use HFT_Engine;

package HFT_Spark is

   pragma Preelaborate;

   -- ====================================================================
   -- Type-Safety Predicates
   -- Each function has a complete Boolean postcondition that GNATprove
   -- can discharge without additional assumptions.
   -- ====================================================================

   -- Price within the compliance range [0.01 .. 999_999_999.99]
   function Spark_Price_In_Range (P : HFT_Engine.Price) return Boolean
      with Global => null,
           Post   => Spark_Price_In_Range'Result =
                       (P >= 0.01 and P <= 999_999_999.99);

   -- Quantity is strictly positive (valid for an order)
   function Spark_Quantity_Positive (Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => Spark_Quantity_Positive'Result = (Q > 0);

   -- Quantity within the compliance range (1 .. 1_000_000_000)
   function Spark_Quantity_In_Range (Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => Spark_Quantity_In_Range'Result =
                       (Q > 0 and Q <= 1_000_000_000);

   -- Price is non-zero (guards against division by zero)
   function Spark_Price_Nonzero (P : HFT_Engine.Price) return Boolean
      with Global => null,
           Post   => Spark_Price_Nonzero'Result = (P /= 0.0);

   -- Timestamp is initialised (non-zero)
   function Spark_Timestamp_Initialized (T : HFT_Engine.Timestamp) return Boolean
      with Global => null,
           Post   => Spark_Timestamp_Initialized'Result = (T /= 0);

   -- Order side is a valid enumeration value (always True by type)
   function Spark_Side_Valid (S : HFT_Engine.Side) return Boolean
      with Global => null,
           Post   => Spark_Side_Valid'Result = True;

   -- ====================================================================
   -- Symbol Validation with Quantified Postconditions
   -- GNATprove can verify the loop contracts statically.
   -- ====================================================================

   -- Every character in Symbol is an uppercase letter or a space
   function Spark_Symbol_Uppercase (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length > 0,
           Post   => Spark_Symbol_Uppercase'Result =
                       (for all C of Symbol => C in 'A' .. 'Z' | ' ');

   -- Symbol contains at least one non-space character
   function Spark_Symbol_Has_Content (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length > 0,
           Post   => Spark_Symbol_Has_Content'Result =
                       (for some C of Symbol => C /= ' ');

   -- Symbol passes both format checks (precondition: correct length)
   function Spark_Symbol_Valid (Symbol : String) return Boolean
      with Global => null,
           Pre    => Symbol'Length = HFT_Engine.Symbol_Length,
           Post   => Spark_Symbol_Valid'Result =
                       (Spark_Symbol_Uppercase (Symbol)
                        and Spark_Symbol_Has_Content (Symbol));

   -- ====================================================================
   -- Arithmetic Safety
   -- ====================================================================

   -- P * Q does not exceed Price'Last (no fixed-point overflow)
   function Spark_Mul_Safe
     (P : HFT_Engine.Price; Q : HFT_Engine.Quantity) return Boolean
      with Global => null,
           Post   => (if Spark_Mul_Safe'Result then
                        Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last));

   -- P1 + P2 stays within compliance price range
   function Spark_Add_Safe (P1 : HFT_Engine.Price; P2 : HFT_Engine.Price)
      return Boolean
      with Global => null,
           Post   => (if Spark_Add_Safe'Result then P1 + P2 <= 999_999_999.99);

   -- P * Q does not exceed the supplied limit
   function Spark_Value_Under_Limit
     (P     : HFT_Engine.Price;
      Q     : HFT_Engine.Quantity;
      Limit : HFT_Engine.Price) return Boolean
      with Global => null,
           Pre    => Limit > 0.0,
           Post   => (if Spark_Value_Under_Limit'Result then
                        Float (P) * Float (Q) <= Float (Limit));

   -- ====================================================================
   -- Core Order Invariant (structural, timestamp-independent)
   -- ====================================================================

   function Spark_Order_Invariant (O : HFT_Engine.Order) return Boolean
      with Global => null,
           Post   => Spark_Order_Invariant'Result =
                       (O.Price_Val > 0.0
                        and O.Qty > 0
                        and O.Order_ID > 0
                        and O.Time_Stamp > 0);

   -- ====================================================================
   -- Order Matching Predicate
   -- ====================================================================

   -- Two orders can be matched: buy price >= sell price, same symbol
   function Spark_Orders_Match
     (Buy_Order  : HFT_Engine.Order;
      Sell_Order : HFT_Engine.Order) return Boolean
      with Global => null,
           Pre    => Buy_Order.Order_Side  = HFT_Engine.Buy
                     and Sell_Order.Order_Side = HFT_Engine.Sell,
           Post   => Spark_Orders_Match'Result =
                       (Buy_Order.Price_Val >= Sell_Order.Price_Val
                        and Buy_Order.Symbol = Sell_Order.Symbol);

   -- ====================================================================
   -- SPARK Compliance Result (pure record; no I/O, no global state)
   -- ====================================================================

   type Spark_Result is record
      Passed         : Boolean := False;
      Type_Safety    : Boolean := False;
      Contract_Valid : Boolean := False;
      Range_Safe     : Boolean := False;
      Symbol_Ok      : Boolean := False;
      Value_Ok       : Boolean := False;
      NIL_Safe       : Boolean := False;
   end record;

   -- Full compliance check with SPARK-provable postcondition
   function Spark_Full_Check (O : HFT_Engine.Order) return Spark_Result
      with Global => null,
           Post   => (if Spark_Full_Check'Result.Passed then
                        Spark_Order_Invariant (O)
                        and Spark_Symbol_Valid (O.Symbol));

end HFT_Spark;
