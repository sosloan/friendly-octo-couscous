-- Ada Compliance Audit System Implementation
pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with HFT_Time_Util;

package body HFT_Audit is
   use Ada.Text_IO;
   

   -- Internal audit event storage
   package Audit_Event_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Audit_Event);

   Audit_History : Audit_Event_Vectors.Vector;
   Next_Event_ID : Positive := 1;
   -- Note: Not thread-safe. For multi-threaded use, add protected object or atomic operations
   Audit_Start_Time : HFT_Engine.Timestamp;
   Current_Config : Audit_Config;

   -- Internal statistics tracking
   Stats : Audit_Statistics;

   procedure Initialize_Audit_System is
   begin
      Audit_History.Clear;
      Next_Event_ID := 1;
      Audit_Start_Time := HFT_Engine.Timestamp (HFT_Time_Util.Get_Unix_Timestamp);
      Stats := (others => 0);
      Current_Config := (
         Enable_Audit => True,
         Log_All_Events => True,
         Log_Passed_Checks => False,
         Log_Failed_Checks => True,
         Max_History_Size => 10_000,
         Enable_Performance_Tracking => True
      );
   end Initialize_Audit_System;

   procedure Record_Audit_Event (
      Event_Type  : Audit_Event_Type;
      Severity    : Severity_Level;
      Order_ID    : Positive;
      Category    : HFT_Compliance.Compliance_Category;
      Description : String;
      Passed      : Boolean
   ) is
      Event : Audit_Event;
      Desc_Len : constant Natural := Natural'Min (Description'Length, 200);
   begin
      if not Current_Config.Enable_Audit then
         return;
      end if;

      -- Create event record
      Event.Event_ID := Next_Event_ID;
      Event.Time_Stamp := HFT_Engine.Timestamp (HFT_Time_Util.Get_Unix_Timestamp);
      Event.Event_Type := Event_Type;
      Event.Severity := Severity;
      Event.Order_ID := Order_ID;
      Event.Category := Category;
      Event.Description := (others => ' ');
      Event.Description (1 .. Desc_Len) := 
         Description (Description'First .. Description'First + Desc_Len - 1);
      Event.Passed := Passed;

      -- Store event
      if Natural (Audit_History.Length) < Current_Config.Max_History_Size then
         Audit_History.Append (Event);
      end if;

      Next_Event_ID := Next_Event_ID + 1;

      -- Update statistics
      Stats.Total_Events := Stats.Total_Events + 1;

      case Event_Type is
         when Compliance_Check_Started | Compliance_Check_Completed =>
            Stats.Total_Checks := Stats.Total_Checks + 1;
            if Passed then
               Stats.Total_Passed := Stats.Total_Passed + 1;
            else
               Stats.Total_Failed := Stats.Total_Failed + 1;
            end if;

         when Compliance_Check_Failed =>
            Stats.Total_Violations := Stats.Total_Violations + 1;

         when Type_Safety_Violation =>
            Stats.Type_Safety_Failures := Stats.Type_Safety_Failures + 1;

         when Contract_Violation =>
            Stats.Contract_Failures := Stats.Contract_Failures + 1;

         when Range_Safety_Violation =>
            Stats.Range_Safety_Failures := Stats.Range_Safety_Failures + 1;

         when Coding_Standards_Violation =>
            Stats.Coding_Std_Failures := Stats.Coding_Std_Failures + 1;

         when Security_Violation =>
            Stats.Security_Failures := Stats.Security_Failures + 1;

         when Performance_Warning =>
            Stats.Performance_Warnings := Stats.Performance_Warnings + 1;

         when Order_Accepted =>
            Stats.Orders_Accepted := Stats.Orders_Accepted + 1;

         when Order_Rejected =>
            Stats.Orders_Rejected := Stats.Orders_Rejected + 1;
      end case;
   end Record_Audit_Event;

   procedure Audit_Order_Compliance (
      O : HFT_Engine.Order;
      Result : out HFT_Compliance.Check_Result
   ) is
      use HFT_Compliance;
   begin
      Record_Audit_Event (
         Compliance_Check_Started,
         Info,
         O.Order_ID,
         Type_Safety,  -- Generic category for overall compliance
         "Starting compliance check for order",
         True
      );

      Result := Run_Full_Compliance_Check (O);

      if Result.Passed then
         Record_Audit_Event (
            Compliance_Check_Completed,
            Info,
            O.Order_ID,
            Type_Safety,  -- Generic category for overall compliance
            "Compliance check passed",
            True
         );
         Record_Audit_Event (
            Order_Accepted,
            Info,
            O.Order_ID,
            Type_Safety,  -- Generic category for overall compliance
            "Order accepted - all compliance checks passed",
            True
         );
      else
         Record_Audit_Event (
            Compliance_Check_Failed,
            Error,
            O.Order_ID,
            Type_Safety,  -- Generic category for overall compliance
            "Compliance check failed",
            False
         );
         Record_Audit_Event (
            Order_Rejected,
            Error,
            O.Order_ID,
            Type_Safety,  -- Generic category for overall compliance
            "Order rejected - compliance violations detected",
            False
         );
      end if;
   end Audit_Order_Compliance;

   function Get_Audit_Events_By_Order (Order_ID : Positive) return Natural is
      Count : Natural := 0;
   begin
      for Event of Audit_History loop
         if Event.Order_ID = Order_ID then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Get_Audit_Events_By_Order;

   function Get_Audit_Events_By_Type (
      Event_Type : Audit_Event_Type
   ) return Natural is
      Count : Natural := 0;
   begin
      for Event of Audit_History loop
         if Event.Event_Type = Event_Type then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Get_Audit_Events_By_Type;

   function Get_Audit_Events_By_Severity (
      Severity : Severity_Level
   ) return Natural is
      Count : Natural := 0;
   begin
      for Event of Audit_History loop
         if Event.Severity = Severity then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Get_Audit_Events_By_Severity;

   function Get_Audit_Statistics return Audit_Statistics is
   begin
      return Stats;
   end Get_Audit_Statistics;

   function Generate_Audit_Summary return Audit_Summary is
      Summary : Audit_Summary;
   begin
      Summary.Start_Time := Audit_Start_Time;
      Summary.End_Time := HFT_Engine.Timestamp (HFT_Time_Util.Get_Unix_Timestamp);
      Summary.Stats := Stats;
      
      if Stats.Total_Checks > 0 then
         Summary.Success_Rate := Float (Stats.Total_Passed) / Float (Stats.Total_Checks) * 100.0;
      else
         Summary.Success_Rate := 0.0;
      end if;

      Summary.Critical_Events := Get_Audit_Events_By_Severity (Critical);
      Summary.Error_Events := Get_Audit_Events_By_Severity (Error);
      Summary.Warning_Events := Get_Audit_Events_By_Severity (Warning);
      Summary.Info_Events := Get_Audit_Events_By_Severity (Info);

      return Summary;
   end Generate_Audit_Summary;

   procedure Print_Audit_Report is
      Summary : constant Audit_Summary := Generate_Audit_Summary;
   begin
      Put_Line ("╔════════════════════════════════════════════════════════╗");
      Put_Line ("║         Ada Compliance Audit Report                    ║");
      Put_Line ("╚════════════════════════════════════════════════════════╝");
      Put_Line ("");
      Put_Line ("Audit Period:");
      Put_Line ("  Start Time:     " & HFT_Engine.Timestamp'Image (Summary.Start_Time));
      Put_Line ("  End Time:       " & HFT_Engine.Timestamp'Image (Summary.End_Time));
      Put_Line ("");
      Put_Line ("Overall Statistics:");
      Put_Line ("  Total Events:   " & Natural'Image (Summary.Stats.Total_Events));
      Put_Line ("  Total Checks:   " & Natural'Image (Summary.Stats.Total_Checks));
      Put_Line ("  Passed:         " & Natural'Image (Summary.Stats.Total_Passed));
      Put_Line ("  Failed:         " & Natural'Image (Summary.Stats.Total_Failed));
      Put_Line ("  Success Rate:   " & Float'Image (Summary.Success_Rate) & "%");
      Put_Line ("");
      Put_Line ("Compliance Violations:");
      Put_Line ("  Type Safety:    " & Natural'Image (Summary.Stats.Type_Safety_Failures));
      Put_Line ("  Contracts:      " & Natural'Image (Summary.Stats.Contract_Failures));
      Put_Line ("  Range Safety:   " & Natural'Image (Summary.Stats.Range_Safety_Failures));
      Put_Line ("  Coding Stds:    " & Natural'Image (Summary.Stats.Coding_Std_Failures));
      Put_Line ("  Security:       " & Natural'Image (Summary.Stats.Security_Failures));
      Put_Line ("  Performance:    " & Natural'Image (Summary.Stats.Performance_Warnings));
      Put_Line ("");
      Put_Line ("Order Processing:");
      Put_Line ("  Accepted:       " & Natural'Image (Summary.Stats.Orders_Accepted));
      Put_Line ("  Rejected:       " & Natural'Image (Summary.Stats.Orders_Rejected));
      Put_Line ("");
      Put_Line ("Event Severity:");
      Put_Line ("  Critical:       " & Natural'Image (Summary.Critical_Events));
      Put_Line ("  Error:          " & Natural'Image (Summary.Error_Events));
      Put_Line ("  Warning:        " & Natural'Image (Summary.Warning_Events));
      Put_Line ("  Info:           " & Natural'Image (Summary.Info_Events));
      Put_Line ("════════════════════════════════════════════════════════");
   end Print_Audit_Report;

   procedure Print_Audit_History (Max_Events : Positive := 100) is
      Count : Natural := 0;
   begin
      Put_Line ("╔════════════════════════════════════════════════════════╗");
      Put_Line ("║         Audit Event History                            ║");
      Put_Line ("╚════════════════════════════════════════════════════════╝");
      Put_Line ("");

      for Event of reverse Audit_History loop
         exit when Count >= Max_Events;
         Count := Count + 1;

         Put_Line ("Event #" & Positive'Image (Event.Event_ID));
         Put_Line ("  Type:     " & Audit_Event_Type'Image (Event.Event_Type));
         Put_Line ("  Severity: " & Severity_Level'Image (Event.Severity));
         Put_Line ("  Order ID: " & Positive'Image (Event.Order_ID));
         Put_Line ("  Status:   " & (if Event.Passed then "PASS" else "FAIL"));
         Put_Line ("  Details:  " & Event.Description);
         Put_Line ("");
      end loop;

      Put_Line ("Showing " & Natural'Image (Count) & " most recent events");
      Put_Line ("════════════════════════════════════════════════════════");
   end Print_Audit_History;

   procedure Export_Audit_Log (Filename : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      
      Put_Line (File, "Ada Compliance Audit Log Export");
      Put_Line (File, "================================");
      Put_Line (File, "");

      for Event of Audit_History loop
         Put_Line (File, "Event ID: " & Positive'Image (Event.Event_ID));
         Put_Line (File, "Type: " & Audit_Event_Type'Image (Event.Event_Type));
         Put_Line (File, "Severity: " & Severity_Level'Image (Event.Severity));
         Put_Line (File, "Order ID: " & Positive'Image (Event.Order_ID));
         Put_Line (File, "Category: " & 
                   HFT_Compliance.Compliance_Category'Image (Event.Category));
         Put_Line (File, "Status: " & (if Event.Passed then "PASS" else "FAIL"));
         Put_Line (File, "Description: " & Event.Description);
         Put_Line (File, "---");
      end loop;

      Close (File);
   end Export_Audit_Log;

   procedure Clear_Audit_History is
   begin
      Audit_History.Clear;
      Next_Event_ID := 1;
      Stats := (others => 0);
   end Clear_Audit_History;

   function Analyze_Compliance_Trend return Trend_Direction is
      Recent_Success_Rate : Float := 0.0;
      Earlier_Success_Rate : Float := 0.0;
      Midpoint : Natural;
      Recent_Passed : Natural := 0;
      Recent_Total : Natural := 0;
      Earlier_Passed : Natural := 0;
      Earlier_Total : Natural := 0;
   begin
      if Natural (Audit_History.Length) < 10 then
         return Stable;
      end if;

      Midpoint := Natural (Audit_History.Length) / 2;

      -- Analyze first half
      for I in 1 .. Midpoint loop
         if Audit_History.Element (I).Event_Type = Compliance_Check_Completed then
            Earlier_Total := Earlier_Total + 1;
            if Audit_History.Element (I).Passed then
               Earlier_Passed := Earlier_Passed + 1;
            end if;
         end if;
      end loop;

      -- Analyze second half
      for I in Midpoint + 1 .. Natural (Audit_History.Length) loop
         if Audit_History.Element (I).Event_Type = Compliance_Check_Completed then
            Recent_Total := Recent_Total + 1;
            if Audit_History.Element (I).Passed then
               Recent_Passed := Recent_Passed + 1;
            end if;
         end if;
      end loop;

      if Earlier_Total > 0 then
         Earlier_Success_Rate := Float (Earlier_Passed) / Float (Earlier_Total);
      end if;

      if Recent_Total > 0 then
         Recent_Success_Rate := Float (Recent_Passed) / Float (Recent_Total);
      end if;

      if Recent_Success_Rate > Earlier_Success_Rate + 0.05 then
         return Improving;
      elsif Recent_Success_Rate < Earlier_Success_Rate - 0.05 then
         return Degrading;
      else
         return Stable;
      end if;
   end Analyze_Compliance_Trend;

   procedure Print_Top_Violations (Top_N : Positive := 10) is
      type Violation_Count is record
         Violation_Type : Audit_Event_Type;
         Count : Natural := 0;
      end record;

      Violations : array (1 .. 6) of Violation_Count := (
         others => (Violation_Type => Type_Safety_Violation, Count => 0)
      );
   begin
      Put_Line ("Top Compliance Violations:");
      Put_Line ("=========================");

      Violations (1) := (Type_Safety_Violation, Stats.Type_Safety_Failures);
      Violations (2) := (Contract_Violation, Stats.Contract_Failures);
      Violations (3) := (Range_Safety_Violation, Stats.Range_Safety_Failures);
      Violations (4) := (Coding_Standards_Violation, Stats.Coding_Std_Failures);
      Violations (5) := (Security_Violation, Stats.Security_Failures);
      Violations (6) := (Performance_Warning, Stats.Performance_Warnings);

      for V of Violations loop
         if V.Count > 0 then
            Put_Line ("  " & Audit_Event_Type'Image (V.Violation_Type) & ": " & 
                      Natural'Image (V.Count));
         end if;
      end loop;
   end Print_Top_Violations;

   procedure Configure_Audit (Config : Audit_Config) is
   begin
      Current_Config := Config;
   end Configure_Audit;

   function Get_Audit_Config return Audit_Config is
   begin
      return Current_Config;
   end Get_Audit_Config;

begin
   Initialize_Audit_System;
end HFT_Audit;
