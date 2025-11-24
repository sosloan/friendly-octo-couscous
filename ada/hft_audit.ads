-- Ada Compliance Audit System Specification
-- Provides comprehensive audit trails, history tracking, and compliance reporting
pragma Ada_2022;

with HFT_Engine;
with HFT_Compliance;
with Ada.Containers.Vectors;

package HFT_Audit is
   pragma Elaborate_Body;

   -- Audit Event Types
   type Audit_Event_Type is (
      Compliance_Check_Started,
      Compliance_Check_Completed,
      Compliance_Check_Failed,
      Type_Safety_Violation,
      Contract_Violation,
      Range_Safety_Violation,
      Coding_Standards_Violation,
      Security_Violation,
      Performance_Warning,
      Order_Accepted,
      Order_Rejected
   );

   -- Audit Severity Levels
   type Severity_Level is (Info, Warning, Error, Critical);

   -- Audit Event Record
   type Audit_Event is record
      Event_ID        : Positive;
      Time_Stamp      : HFT_Engine.Timestamp;
      Event_Type      : Audit_Event_Type;
      Severity        : Severity_Level;
      Order_ID        : Positive;
      Category        : HFT_Compliance.Compliance_Category;
      Description     : String (1 .. 200);
      Passed          : Boolean;
   end record;

   -- Audit Statistics
   type Audit_Statistics is record
      Total_Events           : Natural := 0;
      Total_Checks           : Natural := 0;
      Total_Passed           : Natural := 0;
      Total_Failed           : Natural := 0;
      Total_Violations       : Natural := 0;
      Type_Safety_Failures   : Natural := 0;
      Contract_Failures      : Natural := 0;
      Range_Safety_Failures  : Natural := 0;
      Coding_Std_Failures    : Natural := 0;
      Security_Failures      : Natural := 0;
      Performance_Warnings   : Natural := 0;
      Orders_Accepted        : Natural := 0;
      Orders_Rejected        : Natural := 0;
   end record;

   -- Audit Summary Report
   type Audit_Summary is record
      Start_Time      : HFT_Engine.Timestamp;
      End_Time        : HFT_Engine.Timestamp;
      Stats           : Audit_Statistics;
      Success_Rate    : Float;
      Critical_Events : Natural := 0;
      Error_Events    : Natural := 0;
      Warning_Events  : Natural := 0;
      Info_Events     : Natural := 0;
   end record;

   -- Initialize audit system
   procedure Initialize_Audit_System;

   -- Record audit events
   procedure Record_Audit_Event (
      Event_Type  : Audit_Event_Type;
      Severity    : Severity_Level;
      Order_ID    : Positive;
      Category    : HFT_Compliance.Compliance_Category;
      Description : String;
      Passed      : Boolean
   );

   -- Audit a single order compliance check
   procedure Audit_Order_Compliance (
      O : HFT_Engine.Order;
      Result : out HFT_Compliance.Check_Result
   );

   -- Note: For batch processing, call Audit_Order_Compliance in a loop
   -- Future enhancement: Add true batch processing with array of orders

   -- Query audit events
   function Get_Audit_Events_By_Order (Order_ID : Positive) return Natural;
   
   function Get_Audit_Events_By_Type (
      Event_Type : Audit_Event_Type
   ) return Natural;

   function Get_Audit_Events_By_Severity (
      Severity : Severity_Level
   ) return Natural;

   -- Get audit statistics
   function Get_Audit_Statistics return Audit_Statistics;

   -- Generate audit summary
   function Generate_Audit_Summary return Audit_Summary;

   -- Print audit report
   procedure Print_Audit_Report;

   -- Print detailed audit history
   procedure Print_Audit_History (Max_Events : Positive := 100);

   -- Export audit log
   procedure Export_Audit_Log (Filename : String);

   -- Clear audit history (for testing)
   procedure Clear_Audit_History;

   -- Compliance trend analysis
   type Trend_Direction is (Improving, Stable, Degrading);
   
   function Analyze_Compliance_Trend return Trend_Direction;

   -- Get most common violations
   procedure Print_Top_Violations (Top_N : Positive := 10);

   -- Audit configuration
   type Audit_Config is record
      Enable_Audit        : Boolean := True;
      Log_All_Events      : Boolean := True;
      Log_Passed_Checks   : Boolean := False;
      Log_Failed_Checks   : Boolean := True;
      Max_History_Size    : Positive := 10_000;
      Enable_Performance_Tracking : Boolean := True;
   end record;

   procedure Configure_Audit (Config : Audit_Config);
   function Get_Audit_Config return Audit_Config;

end HFT_Audit;
