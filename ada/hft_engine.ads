-- Ada HFT Engine Specification
-- Provides type-safe, formally verified high-frequency trading core
pragma Ada_2022;

with Ada.Real_Time;

package HFT_Engine is
   pragma Pure;

   -- Price type with fixed-point precision for financial calculations
   type Price is delta 0.01 digits 15;
   
   -- Quantity type for order volumes
   type Quantity is range 0 .. 1_000_000_000;
   
   -- Symbol length constant
   Symbol_Length : constant := 10;
   
   -- Order side enumeration
   type Side is (Buy, Sell);
   
   -- Order type definition
   type Order is record
      Order_ID   : Positive;
      Symbol     : String (1 .. Symbol_Length);
      Price_Val  : Price;
      Qty        : Quantity;
      Order_Side : Side;
      Timestamp  : Ada.Real_Time.Time;
   end record;
   
   -- Order validation - ensures all constraints are met
   function Is_Valid_Order (O : Order) return Boolean
      with Post => (if Is_Valid_Order'Result then O.Qty > 0 and O.Price_Val > 0.0);
   
   -- Calculate order value
   function Calculate_Value (O : Order) return Price
      with Pre => Is_Valid_Order (O),
           Post => Calculate_Value'Result >= 0.0;
   
   -- Match orders - core HFT matching logic
   function Can_Match (Buy_Order : Order; Sell_Order : Order) return Boolean
      with Pre => Is_Valid_Order (Buy_Order) and Is_Valid_Order (Sell_Order)
                  and Buy_Order.Order_Side = Buy and Sell_Order.Order_Side = Sell;

end HFT_Engine;
