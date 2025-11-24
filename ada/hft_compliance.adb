-- Ada Compliance Checking Implementation
pragma Ada_2022;

with Ada.Text_IO;
with Ada.Real_Time;

package body HFT_Compliance is

   -- Type Safety Checks
   function Check_Price_Range (P : HFT_Engine.Price) return Boolean is
   begin
      return P >= 0.01 and P <= 999_999_999.99;
   end Check_Price_Range;

   function Check_Quantity_Range (Q : HFT_Engine.Quantity) return Boolean is
   begin
      return Q >= 0 and Q <= 1_000_000_000;
   end Check_Quantity_Range;

   function Check_Order_ID_Valid (ID : Positive) return Boolean is
      pragma Unreferenced (ID);
   begin
      -- Always true due to Positive constraint, but kept for framework completeness
      return True;
   end Check_Order_ID_Valid;

   -- Contract Compliance Checks
   function Verify_Order_Preconditions (O : HFT_Engine.Order) return Boolean is
   begin
      return O.Qty > 0 and O.Price_Val > 0.0 and O.Order_ID > 0;
   end Verify_Order_Preconditions;

   function Verify_Order_Postconditions (
      O : HFT_Engine.Order;
      Value : HFT_Engine.Price
   ) return Boolean is
      pragma Unreferenced (O);
   begin
      return Value >= 0.0;
   end Verify_Order_Postconditions;

   -- Range and Overflow Checks
   function Check_Multiplication_Overflow (
      P : HFT_Engine.Price;
      Q : HFT_Engine.Quantity
   ) return Boolean is
      Max_Safe_Value : constant Float := Float (HFT_Engine.Price'Last);
   begin
      -- Check if multiplication would overflow
      return Float (P) * Float (Q) <= Max_Safe_Value;
   end Check_Multiplication_Overflow;

   function Check_Price_Addition_Safe (
      P1 : HFT_Engine.Price;
      P2 : HFT_Engine.Price
   ) return Boolean is
      Max_Price : constant Float := Float (HFT_Engine.Price'Last);
   begin
      return Float (P1) + Float (P2) <= Max_Price;
   end Check_Price_Addition_Safe;

   -- Coding Standards Checks
   function Check_Symbol_Format (Symbol : String) return Boolean is
   begin
      -- Symbol should be uppercase letters or spaces only
      for C of Symbol loop
         if not (C in 'A' .. 'Z' | ' ') then
            return False;
         end if;
      end loop;
      
      -- Should have at least one non-space character
      for C of Symbol loop
         if C /= ' ' then
            return True;
         end if;
      end loop;
      
      return False; -- All spaces is invalid
   end Check_Symbol_Format;

   function Check_Order_Side_Valid (S : HFT_Engine.Side) return Boolean is
      pragma Unreferenced (S);
   begin
      -- Always true due to enumeration type constraint, but kept for framework completeness
      return True;
   end Check_Order_Side_Valid;

   -- Security Compliance Checks
   function Check_Order_Value_Limit (O : HFT_Engine.Order) return Boolean is
      Max_Order_Value : constant HFT_Engine.Price := 100_000_000.0;
      Order_Value : HFT_Engine.Price;
   begin
      Order_Value := HFT_Engine.Calculate_Value (O);
      return Order_Value <= Max_Order_Value;
   end Check_Order_Value_Limit;

   function Check_No_Zero_Division (Divisor : HFT_Engine.Price) return Boolean is
   begin
      return Divisor /= 0.0;
   end Check_No_Zero_Division;

   function Check_Timestamp_Valid (O : HFT_Engine.Order) return Boolean is
      use Ada.Real_Time;
      Current : Time := Clock;
      Time_Diff : Time_Span;
   begin
      -- Check that timestamp is not in the future
      if O.Timestamp > Current then
         return False;
      end if;
      
      -- Check that timestamp is not too old (e.g., > 24 hours)
      Time_Diff := Current - O.Timestamp;
      return Time_Diff <= Seconds (86400); -- 24 hours
   end Check_Timestamp_Valid;

   -- Performance Compliance Checks
   function Check_Symbol_Length_Optimal (Symbol : String) return Boolean is
   begin
      -- Optimal symbol length is between 1 and Symbol_Length (defined in HFT_Engine)
      return Symbol'Length >= 1 and Symbol'Length <= HFT_Engine.Symbol_Length;
   end Check_Symbol_Length_Optimal;

   function Check_Order_Size_Reasonable (Q : HFT_Engine.Quantity) return Boolean is
   begin
      -- Reasonable order size: not too small (avoid spam) or too large (risk)
      return Q >= 1 and Q <= 10_000_000;
   end Check_Order_Size_Reasonable;

   -- Helper function to create check result
   function Make_Check_Result (
      Passed : Boolean;
      Name : String;
      Desc : String
   ) return Check_Result is
      Result : Check_Result;
      Name_Len : constant Natural := Natural'Min (Name'Length, 50);
      Desc_Len : constant Natural := Natural'Min (Desc'Length, 100);
   begin
      Result.Passed := Passed;
      Result.Check_Name := (others => ' ');
      Result.Description := (others => ' ');
      Result.Check_Name (1 .. Name_Len) := Name (Name'First .. Name'First + Name_Len - 1);
      Result.Description (1 .. Desc_Len) := Desc (Desc'First .. Desc'First + Desc_Len - 1);
      return Result;
   end Make_Check_Result;

   -- Comprehensive Order Compliance Check
   function Run_Full_Compliance_Check (O : HFT_Engine.Order) return Check_Result is
      All_Passed : Boolean := True;
   begin
      -- Type Safety
      All_Passed := All_Passed and Check_Price_Range (O.Price_Val);
      All_Passed := All_Passed and Check_Quantity_Range (O.Qty);
      All_Passed := All_Passed and Check_Order_ID_Valid (O.Order_ID);
      
      -- Contract Compliance
      All_Passed := All_Passed and Verify_Order_Preconditions (O);
      
      -- Range Safety
      All_Passed := All_Passed and Check_Multiplication_Overflow (O.Price_Val, O.Qty);
      
      -- Coding Standards
      All_Passed := All_Passed and Check_Symbol_Format (O.Symbol);
      All_Passed := All_Passed and Check_Order_Side_Valid (O.Order_Side);
      
      -- Security
      if HFT_Engine.Is_Valid_Order (O) then
         All_Passed := All_Passed and Check_Order_Value_Limit (O);
      end if;
      All_Passed := All_Passed and Check_Timestamp_Valid (O);
      
      -- Performance
      All_Passed := All_Passed and Check_Symbol_Length_Optimal (O.Symbol);
      All_Passed := All_Passed and Check_Order_Size_Reasonable (O.Qty);
      
      if All_Passed then
         return Make_Check_Result (
            True,
            "Full Compliance",
            "All compliance checks passed successfully"
         );
      else
         return Make_Check_Result (
            False,
            "Full Compliance",
            "One or more compliance checks failed"
         );
      end if;
   end Run_Full_Compliance_Check;

   -- Category-specific compliance check
   function Run_Category_Check (
      O : HFT_Engine.Order;
      Category : Compliance_Category
   ) return Check_Result is
      Passed : Boolean;
   begin
      case Category is
         when Type_Safety =>
            Passed := Check_Price_Range (O.Price_Val) and
                     Check_Quantity_Range (O.Qty) and
                     Check_Order_ID_Valid (O.Order_ID);
            return Make_Check_Result (
               Passed,
               "Type Safety",
               "Price, quantity, and ID type constraints validated"
            );
            
         when Contract_Validity =>
            Passed := Verify_Order_Preconditions (O);
            if HFT_Engine.Is_Valid_Order (O) then
               Passed := Passed and Verify_Order_Postconditions (O, HFT_Engine.Calculate_Value (O));
            end if;
            return Make_Check_Result (
               Passed,
               "Contract Validity",
               "Pre and post conditions verified"
            );
            
         when Range_Safety =>
            Passed := Check_Multiplication_Overflow (O.Price_Val, O.Qty);
            return Make_Check_Result (
               Passed,
               "Range Safety",
               "No overflow in arithmetic operations"
            );
            
         when Coding_Standards =>
            Passed := Check_Symbol_Format (O.Symbol) and
                     Check_Order_Side_Valid (O.Order_Side);
            return Make_Check_Result (
               Passed,
               "Coding Standards",
               "Ada coding standards compliance verified"
            );
            
         when Security =>
            Passed := Check_Timestamp_Valid (O);
            if HFT_Engine.Is_Valid_Order (O) then
               Passed := Passed and Check_Order_Value_Limit (O);
            end if;
            return Make_Check_Result (
               Passed,
               "Security",
               "Security constraints validated"
            );
            
         when Performance =>
            Passed := Check_Symbol_Length_Optimal (O.Symbol) and
                     Check_Order_Size_Reasonable (O.Qty);
            return Make_Check_Result (
               Passed,
               "Performance",
               "Performance-optimal parameters validated"
            );
      end case;
   end Run_Category_Check;

   -- Compliance reporting
   procedure Print_Compliance_Report (O : HFT_Engine.Order) is
      use Ada.Text_IO;
      Result : Check_Result;
   begin
      Put_Line ("=== Ada Compliance Report ===");
      Put_Line ("");
      Put_Line ("Order ID: " & Positive'Image (O.Order_ID));
      Put_Line ("Symbol:   " & O.Symbol);
      Put_Line ("");
      
      for Cat in Compliance_Category loop
         Result := Run_Category_Check (O, Cat);
         if Result.Passed then
            Put_Line ("✓ " & Result.Check_Name & ": PASS");
         else
            Put_Line ("✗ " & Result.Check_Name & ": FAIL");
         end if;
         Put_Line ("  " & Result.Description);
         Put_Line ("");
      end loop;
      
      Result := Run_Full_Compliance_Check (O);
      Put_Line ("Overall Compliance: " & (if Result.Passed then "✓ PASS" else "✗ FAIL"));
      Put_Line ("============================");
   end Print_Compliance_Report;

   -- Statistics
   function Get_Compliance_Statistics (O : HFT_Engine.Order) return Compliance_Stats is
      Stats : Compliance_Stats;
      Result : Check_Result;
   begin
      for Cat in Compliance_Category loop
         Stats.Total_Checks := Stats.Total_Checks + 1;
         Result := Run_Category_Check (O, Cat);
         
         if Result.Passed then
            Stats.Passed_Checks := Stats.Passed_Checks + 1;
            case Cat is
               when Type_Safety      => Stats.Type_Safety_Pass := Stats.Type_Safety_Pass + 1;
               when Contract_Validity => Stats.Contract_Pass := Stats.Contract_Pass + 1;
               when Range_Safety     => Stats.Range_Safety_Pass := Stats.Range_Safety_Pass + 1;
               when Coding_Standards => Stats.Coding_Std_Pass := Stats.Coding_Std_Pass + 1;
               when Security         => Stats.Security_Pass := Stats.Security_Pass + 1;
               when Performance      => Stats.Performance_Pass := Stats.Performance_Pass + 1;
            end case;
         else
            Stats.Failed_Checks := Stats.Failed_Checks + 1;
         end if;
      end loop;
      
      return Stats;
   end Get_Compliance_Statistics;

end HFT_Compliance;
