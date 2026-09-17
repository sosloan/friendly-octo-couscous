-- Ordered audit evidence for Ada order checks, MiFID II best execution,
-- and MiFIR RTS 25 clock synchronization.
pragma Ada_2022;

with HFT_Engine;
with HFT_Compliance;
with HFT_MiFID;

package HFT_Audit is
   pragma Elaborate_Body;

   type Audit_Event_Type is
     (Compliance_Check_Started,
      Compliance_Check_Completed,
      Compliance_Check_Failed,
      Type_Safety_Violation,
      Contract_Violation,
      Range_Safety_Violation,
      Coding_Standards_Violation,
      Security_Violation,
      Performance_Warning,
      Order_Accepted,
      Order_Rejected,
      Market_Data_Received,
      Strategy_Decision_Recorded,
      Risk_Approval_Recorded,
      Gateway_Send_Recorded,
      Venue_Acknowledgement_Recorded,
      Partial_Fill_Recorded,
      Full_Fill_Recorded,
      Cancellation_Recorded,
      Correction_Recorded,
      Best_Execution_Assessed,
      Clock_Synchronization_Lost,
      Clock_Drift_Exceeded,
      Clock_Rollback_Detected,
      Clock_Source_Failed_Over,
      Stale_Market_Data_Detected,
      Reconciliation_Failed);

   subtype Clock_Audit_Event_Type is Audit_Event_Type
      with Static_Predicate =>
        Clock_Audit_Event_Type in Clock_Synchronization_Lost
          | Clock_Drift_Exceeded
          | Clock_Rollback_Detected
          | Clock_Source_Failed_Over;

   type Severity_Level is (Info, Warning, Error, Critical);
   type Regulatory_Domain is
     (Technical_Compliance, MiFID_II_Best_Execution, MiFIR_RTS_25);
   subtype Hash_Text is String (1 .. 64);

   type Audit_Event is record
      Schema_Version  : Positive := 1;
      Event_ID        : Positive := 1;
      Time_Stamp      : HFT_Engine.UTC_Timestamp_NS := 0;
      Monotonic_Time  : HFT_Engine.Monotonic_Timestamp_NS := 0;
      Event_Type      : Audit_Event_Type := Compliance_Check_Started;
      Severity        : Severity_Level := Info;
      Domain          : Regulatory_Domain := Technical_Compliance;
      Order_ID        : Natural := 0;
      Correlation_ID  : Natural := 0;
      Category        : HFT_Compliance.Compliance_Category :=
                          HFT_Compliance.Type_Safety;
      Description     : String (1 .. 200) := (others => ' ');
      Passed          : Boolean := False;
      Has_MiFID_Evidence : Boolean := False;
      MiFID_Evidence  : HFT_MiFID.Execution_Evidence;
      Previous_Hash   : Hash_Text := (others => '0');
      Record_Hash     : Hash_Text := (others => '0');
   end record;

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
      Best_Execution_Events  : Natural := 0;
      Clock_Events           : Natural := 0;
      Reconciliation_Failures : Natural := 0;
   end record;

   type Audit_Summary is record
      Start_Time      : HFT_Engine.UTC_Timestamp_NS;
      End_Time        : HFT_Engine.UTC_Timestamp_NS;
      Stats           : Audit_Statistics;
      Success_Rate    : Float;
      Critical_Events : Natural := 0;
      Error_Events    : Natural := 0;
      Warning_Events  : Natural := 0;
      Info_Events     : Natural := 0;
   end record;

   type Audit_Config is record
      Enable_Audit        : Boolean := True;
      Log_All_Events      : Boolean := True;
      Log_Passed_Checks   : Boolean := False;
      Log_Failed_Checks   : Boolean := True;
      Max_History_Size    : Positive := 10_000;
      Enable_Performance_Tracking : Boolean := True;
   end record;

   Audit_Capacity_Error : exception;
   Audit_Persistence_Error : exception;

   procedure Initialize_Audit_System;

   procedure Record_Audit_Event
     (Event_Type  : Audit_Event_Type;
      Severity    : Severity_Level;
      Order_ID    : Natural;
      Category    : HFT_Compliance.Compliance_Category;
      Description : String;
      Passed      : Boolean);

   procedure Audit_Order_Compliance
     (O      : HFT_Engine.Order;
      Result : out HFT_Compliance.Check_Result);

   procedure Record_Execution_Evidence
     (Evidence : HFT_MiFID.Execution_Evidence;
      Accepted : out Boolean);

   procedure Record_Clock_Event
     (Event_Type  : Clock_Audit_Event_Type;
      Clock       : HFT_MiFID.Clock_Evidence;
      Description : String);

   function Get_Audit_Events_By_Order (Order_ID : Positive) return Natural;
   function Get_Audit_Events_By_Type
     (Event_Type : Audit_Event_Type) return Natural;
   function Get_Audit_Events_By_Severity
     (Severity : Severity_Level) return Natural;
   function Get_Audit_Statistics return Audit_Statistics;
   function Generate_Audit_Summary return Audit_Summary;
   function Audit_Event_Count return Natural;
   function Get_Audit_Event (Index : Positive) return Audit_Event;
   function Verify_Audit_Chain return Boolean;
   function Get_Chain_Head return Hash_Text;
   function Evidence_Digest
     (Evidence : HFT_MiFID.Execution_Evidence) return Hash_Text;

   procedure Print_Audit_Report;
   procedure Print_Audit_History (Max_Events : Positive := 100);
   procedure Export_Audit_Log (Filename : String);
   procedure Clear_Audit_History;
   procedure Configure_Durable_Log
     (Filename : String; Enabled : Boolean := True);

   type Trend_Direction is (Improving, Stable, Degrading);
   function Analyze_Compliance_Trend return Trend_Direction;
   procedure Print_Top_Violations (Top_N : Positive := 10);

   procedure Configure_Audit (Config : Audit_Config);
   function Get_Audit_Config return Audit_Config;
end HFT_Audit;
