-- Ada Compliance Checking Specification
-- Provides comprehensive Ada compliance validation for HFT systems
pragma Ada_2022;

with HFT_Engine; use HFT_Engine;

package HFT_Compliance is

   -- Compliance check result type
   type Check_Result is record
      Passed      : Boolean;
      Check_Name  : String (1 .. 50);
      Description : String (1 .. 100);
   end record;

   -- Compliance categories
   type Compliance_Category is (
      Type_Safety,      -- Type system compliance
      Contract_Validity, -- Pre/post condition checks
      Range_Safety,     -- Range and overflow checks
      Coding_Standards, -- Ada coding standard compliance
      Security,         -- Security-related checks
      Performance       -- Performance compliance
   );

   -- Type Safety Checks
   function Check_Price_Range (P : HFT_Engine.Price) return Boolean
      with Post => Check_Price_Range'Result = (P >= 0.01 and P <= 999_999_999.99);

   function Check_Quantity_Range (Q : HFT_Engine.Quantity) return Boolean
      with Post => Check_Quantity_Range'Result = (Q >= 0 and Q <= 1_000_000_000);

   -- Check Order ID is valid (always true due to Positive type, kept for framework completeness)
   function Check_Order_ID_Valid (ID : Positive) return Boolean
      with Post => Check_Order_ID_Valid'Result = True;

   -- Contract Compliance Checks
   function Verify_Order_Preconditions (O : HFT_Engine.Order) return Boolean
      with Post => (if Verify_Order_Preconditions'Result then 
                     O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0);

   function Verify_Order_Postconditions (O : HFT_Engine.Order; Value : HFT_Engine.Price) return Boolean
      with Pre => HFT_Engine.Is_Valid_Order (O),
           Post => Verify_Order_Postconditions'Result = (Value >= 0.0);

   -- Range and Overflow Checks
   function Check_Multiplication_Overflow (
      P : HFT_Engine.Price;
      Q : HFT_Engine.Quantity
   ) return Boolean
      with Post => (if Check_Multiplication_Overflow'Result then 
                     Float (P) * Float (Q) <= Float (HFT_Engine.Price'Last));

   function Check_Price_Addition_Safe (
      P1 : HFT_Engine.Price;
      P2 : HFT_Engine.Price
   ) return Boolean
      with Post => (if Check_Price_Addition_Safe'Result then
                     Float (P1) + Float (P2) <= Float (HFT_Engine.Price'Last));

   -- Coding Standards Checks
   function Check_Symbol_Format (Symbol : String) return Boolean
      with Pre => Symbol'Length = HFT_Engine.Symbol_Length,
           Post => (if Check_Symbol_Format'Result then 
                     (for all C of Symbol => C in 'A' .. 'Z' | ' '));

   -- Check order side is valid (always true due to enumeration type, kept for framework completeness)
   function Check_Order_Side_Valid (S : HFT_Engine.Side) return Boolean
      with Post => Check_Order_Side_Valid'Result = True;

   -- Security Compliance Checks
   function Check_Order_Value_Limit (O : HFT_Engine.Order) return Boolean
      with Pre => HFT_Engine.Is_Valid_Order (O),
           Post => (if Check_Order_Value_Limit'Result then 
                     HFT_Engine.Calculate_Value (O) <= 100_000_000.0);

   function Check_No_Zero_Division (Divisor : HFT_Engine.Price) return Boolean
      with Post => Check_No_Zero_Division'Result = (Divisor /= 0.0);

   function Check_Timestamp_Valid (O : HFT_Engine.Order) return Boolean;

   -- Performance Compliance Checks
   function Check_Symbol_Length_Optimal (Symbol : String) return Boolean
      with Pre => Symbol'Length > 0;

   function Check_Order_Size_Reasonable (Q : HFT_Engine.Quantity) return Boolean
      with Post => (if Check_Order_Size_Reasonable'Result then 
                     Q >= 1 and Q <= 10_000_000);

   -- Comprehensive Order Compliance Check
   function Run_Full_Compliance_Check (O : HFT_Engine.Order) return Check_Result
      with Post => (if Run_Full_Compliance_Check'Result.Passed then 
                     HFT_Engine.Is_Valid_Order (O));

   -- Category-specific compliance check
   function Run_Category_Check (
      O : HFT_Engine.Order;
      Category : Compliance_Category
   ) return Check_Result;

   -- Compliance reporting
   procedure Print_Compliance_Report (O : HFT_Engine.Order);

   -- Statistics
   type Compliance_Stats is record
      Total_Checks     : Natural := 0;
      Passed_Checks    : Natural := 0;
      Failed_Checks    : Natural := 0;
      Type_Safety_Pass : Natural := 0;
      Contract_Pass    : Natural := 0;
      Range_Safety_Pass: Natural := 0;
      Coding_Std_Pass  : Natural := 0;
      Security_Pass    : Natural := 0;
      Performance_Pass : Natural := 0;
   end record;

   function Get_Compliance_Statistics (O : HFT_Engine.Order) return Compliance_Stats;

end HFT_Compliance;
