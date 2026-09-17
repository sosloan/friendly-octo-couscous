pragma Ada_2022;

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces;
with HFT_Time_Util;

package body HFT_Audit is
   use Ada.Text_IO;
   use type Interfaces.Unsigned_64;

   Max_Audit_Events : constant Positive := 10_000;
   type Event_Array is array (Positive range 1 .. Max_Audit_Events)
      of Audit_Event;
   subtype Path_Buffer is String (1 .. 256);

   Audit_Start_Time : HFT_Engine.UTC_Timestamp_NS :=
      HFT_Time_Util.Get_UTC_Timestamp_NS;

   function Image (Value : Long_Long_Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Long_Long_Integer'Image (Value), Ada.Strings.Both);
   end Image;

   function Image (Value : Natural) return String is
   begin
      return Ada.Strings.Fixed.Trim (Natural'Image (Value), Ada.Strings.Both);
   end Image;

   function Boolean_Image (Value : Boolean) return String is
   begin
      return (if Value then "TRUE" else "FALSE");
   end Boolean_Image;

   function Hex (Value : Interfaces.Unsigned_64) return Hash_Text is
      Digits : constant String := "0123456789ABCDEF";
      Result : Hash_Text := (others => '0');
      Work   : Interfaces.Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digits (Natural (Work mod 16) + 1);
         Work := Work / 16;
      end loop;
      return Result;
   end Hex;

   function Hash
     (Value : String; Previous : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
   is
      Result : Interfaces.Unsigned_64 :=
        16#CBF29CE484222325# xor Previous;
      Prime  : constant Interfaces.Unsigned_64 := 16#100000001B3#;
   begin
      for C of Value loop
         Result :=
           (Result xor Interfaces.Unsigned_64 (Character'Pos (C))) * Prime;
      end loop;
      return Result;
   end Hash;

   function Canonical (Event : Audit_Event) return String is
      E : HFT_MiFID.Execution_Evidence renames Event.MiFID_Evidence;
   begin
      return
        Image (Event.Event_ID) & "|" &
        Image (Long_Long_Integer (Event.Time_Stamp)) & "|" &
        Image (Long_Long_Integer (Event.Monotonic_Time)) & "|" &
        Audit_Event_Type'Image (Event.Event_Type) & "|" &
        Severity_Level'Image (Event.Severity) & "|" &
        Regulatory_Domain'Image (Event.Domain) & "|" &
        Image (Event.Order_ID) & "|" &
        Image (Event.Correlation_ID) & "|" &
        HFT_Compliance.Compliance_Category'Image (Event.Category) & "|" &
        Event.Description & "|" & Boolean_Image (Event.Passed) & "|" &
        Boolean_Image (Event.Has_MiFID_Evidence) & "|" &
        Image (E.Parent_Order_ID) & "|" & Image (E.Child_Order_ID) & "|" &
        HFT_MiFID.Lifecycle_Stage'Image (E.Stage) & "|" &
        HFT_MiFID.Venue'Image (E.Selected_Venue) & "|" &
        HFT_MiFID.Instrument_Class'Image (E.Asset_Class) & "|" &
        E.Instrument & "|" & E.Related_Instrument & "|" & E.Currency & "|" &
        E.Futures_Expiry & "|" &
        HFT_MiFID.Trading_Session'Image (E.Session) & "|" &
        HFT_MiFID.Execution_Order_Type'Image (E.Order_Type) & "|" &
        E.Signal_ID & "|" & E.Client_Mandate & "|" &
        E.Strategy_Constraints & "|" & E.Hedge_Objective & "|" &
        E.Roll_Decision & "|" & E.Routing_Rationale & "|" &
        E.Override_Identity & "|" & E.Override_Reason & "|" &
        E.Policy.Version & "|" & E.Build_ID & "|" &
        HFT_Engine.Price'Image (E.Market.Best_Bid) & "|" &
        HFT_Engine.Price'Image (E.Market.Best_Ask) & "|" &
        Image (Long_Long_Integer (E.Market.Exchange_Timestamp)) & "|" &
        Image (Long_Long_Integer (E.Market.Local_Receipt_Timestamp)) & "|" &
        Image (E.Market.Feed_Sequence) & "|" &
        Boolean_Image (E.Market.Is_Stale) & "|" &
        HFT_Engine.Price'Image (E.Metrics.Arrival_Price) & "|" &
        HFT_Engine.Price'Image (E.Metrics.Execution_Price) & "|" &
        Image (Natural (E.Metrics.Ordered_Quantity)) & "|" &
        Image (Natural (E.Metrics.Filled_Quantity)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Execution_Latency_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.UTC_Offset_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.Uncertainty_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.Granularity_NS)) & "|" &
        HFT_MiFID.RTS25_Tier'Image (E.Clock.Tier) & "|" &
        HFT_MiFID.Synchronization_State'Image (E.Clock.State) & "|" &
        HFT_MiFID.Clock_Source'Image (E.Clock.Source) & "|" &
        E.Reconciliation.Exchange_Order_ID & "|" &
        E.Reconciliation.Drop_Copy_ID & "|" &
        E.Reconciliation.Clearing_ID & "|" &
        Boolean_Image (E.Reconciliation.Quantity_Matches) & "|" &
        Boolean_Image (E.Reconciliation.Price_Matches);
   end Canonical;

   function Serialized (Event : Audit_Event) return String is
   begin
      return Event.Previous_Hash & "|" & Event.Record_Hash & "|" &
        Canonical (Event);
   end Serialized;

   procedure Append_Line (Filename, Value : String) is
      File : File_Type;
   begin
      if Ada.Directories.Exists (Filename) then
         Open (File, Append_File, Filename);
      else
         Create (File, Out_File, Filename);
         Put_Line
           (File,
            "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD");
      end if;
      Put_Line (File, Value);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Append_Line;

   procedure Truncate_Log (Filename : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      Put_Line
        (File, "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD");
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Truncate_Log;

   protected Store is
      procedure Reset;
      procedure Append (Candidate : Audit_Event; Stored : out Boolean);
      procedure Set_Config (Config : Audit_Config);
      function Config return Audit_Config;
      procedure Set_Log (Filename : String; Enabled : Boolean);
      function Count return Natural;
      function Element (Index : Positive) return Audit_Event;
      function Statistics return Audit_Statistics;
      function Count_Order (Order_ID : Positive) return Natural;
      function Count_Type (Event_Type : Audit_Event_Type) return Natural;
      function Count_Severity (Severity : Severity_Level) return Natural;
      function Chain_Valid return Boolean;
      function Chain_Head_Text return Hash_Text;
   private
      Events       : Event_Array;
      Event_Count  : Natural := 0;
      Next_ID      : Positive := 1;
      Current_Stats : Audit_Statistics;
      Current_Config : Audit_Config;
      Chain_Head   : Interfaces.Unsigned_64 := 0;
      Durable_Log_Enabled : Boolean := True;
      Log_Path     : Path_Buffer :=
        "hft_audit_chain.log" & (1 .. 237 => ' ');
      Log_Path_Length : Natural := 19;
   end Store;

   protected body Store is
      procedure Reset is
      begin
         Event_Count := 0;
         Next_ID := 1;
         Current_Stats := (others => 0);
         Chain_Head := 0;
         if Durable_Log_Enabled then
            Truncate_Log (Log_Path (1 .. Log_Path_Length));
         end if;
      end Reset;

      procedure Append (Candidate : Audit_Event; Stored : out Boolean) is
         Event : Audit_Event := Candidate;
         Should_Log : constant Boolean :=
           Current_Config.Log_All_Events
           or else (Candidate.Passed and Current_Config.Log_Passed_Checks)
           or else ((not Candidate.Passed)
                    and Current_Config.Log_Failed_Checks);
         New_Hash : Interfaces.Unsigned_64;
      begin
         Stored := False;
         if not Current_Config.Enable_Audit or else not Should_Log then
            return;
         end if;
         if Event_Count >= Current_Config.Max_History_Size
           or else Event_Count = Max_Audit_Events
         then
            raise Audit_Capacity_Error;
         end if;

         Event.Event_ID := Next_ID;
         Event.Previous_Hash := Hex (Chain_Head);
         New_Hash := Hash (Canonical (Event), Chain_Head);
         Event.Record_Hash := Hex (New_Hash);

         if Durable_Log_Enabled then
            Append_Line
              (Log_Path (1 .. Log_Path_Length), Serialized (Event));
         end if;

         Event_Count := Event_Count + 1;
         Events (Event_Count) := Event;
         Chain_Head := New_Hash;
         if Next_ID = Positive'Last then
            raise Audit_Capacity_Error;
         end if;
         Next_ID := Next_ID + 1;
         Current_Stats.Total_Events := Current_Stats.Total_Events + 1;

         case Event.Event_Type is
            when Compliance_Check_Completed =>
               Current_Stats.Total_Checks :=
                 Current_Stats.Total_Checks + 1;
               Current_Stats.Total_Passed :=
                 Current_Stats.Total_Passed + 1;
            when Compliance_Check_Failed =>
               Current_Stats.Total_Checks :=
                 Current_Stats.Total_Checks + 1;
               Current_Stats.Total_Failed :=
                 Current_Stats.Total_Failed + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Type_Safety_Violation =>
               Current_Stats.Type_Safety_Failures :=
                 Current_Stats.Type_Safety_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Contract_Violation =>
               Current_Stats.Contract_Failures :=
                 Current_Stats.Contract_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Range_Safety_Violation =>
               Current_Stats.Range_Safety_Failures :=
                 Current_Stats.Range_Safety_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Coding_Standards_Violation =>
               Current_Stats.Coding_Std_Failures :=
                 Current_Stats.Coding_Std_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Security_Violation =>
               Current_Stats.Security_Failures :=
                 Current_Stats.Security_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Performance_Warning =>
               Current_Stats.Performance_Warnings :=
                 Current_Stats.Performance_Warnings + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Order_Accepted =>
               Current_Stats.Orders_Accepted :=
                 Current_Stats.Orders_Accepted + 1;
            when Order_Rejected =>
               Current_Stats.Orders_Rejected :=
                 Current_Stats.Orders_Rejected + 1;
            when Reconciliation_Failed =>
               Current_Stats.Reconciliation_Failures :=
                 Current_Stats.Reconciliation_Failures + 1;
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when Clock_Synchronization_Lost
               | Clock_Drift_Exceeded | Clock_Rollback_Detected =>
               Current_Stats.Total_Violations :=
                 Current_Stats.Total_Violations + 1;
            when others =>
               null;
         end case;

         if Event.Domain = MiFID_II_Best_Execution then
            Current_Stats.Best_Execution_Events :=
              Current_Stats.Best_Execution_Events + 1;
         elsif Event.Domain = MiFIR_RTS_25 then
            Current_Stats.Clock_Events :=
              Current_Stats.Clock_Events + 1;
         end if;
         Stored := True;
      end Append;

      procedure Set_Config (Config : Audit_Config) is
      begin
         Current_Config := Config;
      end Set_Config;

      function Config return Audit_Config is
      begin
         return Current_Config;
      end Config;

      procedure Set_Log (Filename : String; Enabled : Boolean) is
      begin
         if Enabled
           and then (Filename'Length = 0
                     or else Filename'Length > Log_Path'Length)
         then
            raise Constraint_Error with "invalid audit log filename";
         end if;
         Durable_Log_Enabled := Enabled;
         if Enabled then
            Log_Path := (others => ' ');
            Log_Path_Length := Filename'Length;
            Log_Path (1 .. Log_Path_Length) := Filename;
            if not Ada.Directories.Exists (Filename) then
               Truncate_Log (Filename);
            end if;
         end if;
      end Set_Log;

      function Count return Natural is
      begin
         return Event_Count;
      end Count;

      function Element (Index : Positive) return Audit_Event is
      begin
         if Index > Event_Count then
            raise Constraint_Error with "audit event index out of range";
         end if;
         return Events (Index);
      end Element;

      function Statistics return Audit_Statistics is
      begin
         return Current_Stats;
      end Statistics;

      function Count_Order (Order_ID : Positive) return Natural is
         Result : Natural := 0;
      begin
         for I in 1 .. Event_Count loop
            if Events (I).Order_ID = Order_ID then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Order;

      function Count_Type (Event_Type : Audit_Event_Type) return Natural is
         Result : Natural := 0;
      begin
         for I in 1 .. Event_Count loop
            if Events (I).Event_Type = Event_Type then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Type;

      function Count_Severity (Severity : Severity_Level) return Natural is
         Result : Natural := 0;
      begin
         for I in 1 .. Event_Count loop
            if Events (I).Severity = Severity then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Severity;

      function Chain_Valid return Boolean is
         Previous : Interfaces.Unsigned_64 := 0;
         Expected : Interfaces.Unsigned_64;
      begin
         for I in 1 .. Event_Count loop
            if Events (I).Previous_Hash /= Hex (Previous) then
               return False;
            end if;
            Expected := Hash (Canonical (Events (I)), Previous);
            if Events (I).Record_Hash /= Hex (Expected) then
               return False;
            end if;
            Previous := Expected;
         end loop;
         return Previous = Chain_Head;
      end Chain_Valid;

      function Chain_Head_Text return Hash_Text is
      begin
         return Hex (Chain_Head);
      end Chain_Head_Text;
   end Store;

   function Fixed_Description (Value : String) return String is
      Result : String (1 .. 200) := (others => ' ');
      Length : constant Natural := Natural'Min (Value'Length, Result'Length);
   begin
      if Length > 0 then
         Result (1 .. Length) :=
           Value (Value'First .. Value'First + Length - 1);
      end if;
      return Result;
   end Fixed_Description;

   procedure Record_Internal
     (Event_Type  : Audit_Event_Type;
      Severity    : Severity_Level;
      Domain      : Regulatory_Domain;
      Order_ID    : Natural;
      Correlation_ID : Natural;
      Category    : HFT_Compliance.Compliance_Category;
      Description : String;
      Passed      : Boolean;
      Has_Evidence : Boolean := False;
      Evidence    : HFT_MiFID.Execution_Evidence :=
                      (others => <>))
   is
      Event  : Audit_Event;
      Stored : Boolean;
   begin
      Event.Time_Stamp := HFT_Time_Util.Get_UTC_Timestamp_NS;
      Event.Monotonic_Time := HFT_Time_Util.Get_Monotonic_Timestamp_NS;
      Event.Event_Type := Event_Type;
      Event.Severity := Severity;
      Event.Domain := Domain;
      Event.Order_ID := Order_ID;
      Event.Correlation_ID := Correlation_ID;
      Event.Category := Category;
      Event.Description := Fixed_Description (Description);
      Event.Passed := Passed;
      Event.Has_MiFID_Evidence := Has_Evidence;
      Event.MiFID_Evidence := Evidence;
      Store.Append (Event, Stored);
   end Record_Internal;

   procedure Initialize_Audit_System is
   begin
      Audit_Start_Time := HFT_Time_Util.Get_UTC_Timestamp_NS;
      Store.Reset;
   end Initialize_Audit_System;

   procedure Record_Audit_Event
     (Event_Type  : Audit_Event_Type;
      Severity    : Severity_Level;
      Order_ID    : Natural;
      Category    : HFT_Compliance.Compliance_Category;
      Description : String;
      Passed      : Boolean) is
   begin
      Record_Internal
        (Event_Type, Severity, Technical_Compliance, Order_ID, 0,
         Category, Description, Passed);
   end Record_Audit_Event;

   function Violation_Event
     (Category : HFT_Compliance.Compliance_Category)
      return Audit_Event_Type is
   begin
      case Category is
         when HFT_Compliance.Type_Safety =>
            return Type_Safety_Violation;
         when HFT_Compliance.Contract_Validity =>
            return Contract_Violation;
         when HFT_Compliance.Range_Safety =>
            return Range_Safety_Violation;
         when HFT_Compliance.Coding_Standards =>
            return Coding_Standards_Violation;
         when HFT_Compliance.Security =>
            return Security_Violation;
         when HFT_Compliance.Performance =>
            return Performance_Warning;
         when HFT_Compliance.NIL_Safety =>
            return Contract_Violation;
      end case;
   end Violation_Event;

   procedure Audit_Order_Compliance
     (O      : HFT_Engine.Order;
      Result : out HFT_Compliance.Check_Result)
   is
      Category_Result : HFT_Compliance.Check_Result;
   begin
      Record_Audit_Event
        (Compliance_Check_Started, Info, O.Order_ID,
         HFT_Compliance.Type_Safety, "Compliance check started", True);
      Result := HFT_Compliance.Run_Full_Compliance_Check (O);

      if Result.Passed then
         Record_Audit_Event
           (Compliance_Check_Completed, Info, O.Order_ID,
            HFT_Compliance.Type_Safety, "Compliance check passed", True);
         Record_Audit_Event
           (Order_Accepted, Info, O.Order_ID,
            HFT_Compliance.Type_Safety, "Order accepted", True);
      else
         for Category in HFT_Compliance.Compliance_Category loop
            Category_Result :=
              HFT_Compliance.Run_Category_Check (O, Category);
            if not Category_Result.Passed then
               Record_Audit_Event
                 (Violation_Event (Category), Error, O.Order_ID, Category,
                  Category_Result.Description, False);
            end if;
         end loop;
         Record_Audit_Event
           (Compliance_Check_Failed, Error, O.Order_ID,
            HFT_Compliance.Type_Safety, "Compliance check failed", False);
         Record_Audit_Event
           (Order_Rejected, Error, O.Order_ID,
            HFT_Compliance.Type_Safety, "Order rejected", False);
      end if;
   end Audit_Order_Compliance;

   function Lifecycle_Event
     (Stage : HFT_MiFID.Lifecycle_Stage) return Audit_Event_Type is
   begin
      case Stage is
         when HFT_MiFID.Market_Data_Receipt =>
            return Market_Data_Received;
         when HFT_MiFID.Strategy_Decision =>
            return Strategy_Decision_Recorded;
         when HFT_MiFID.Risk_Approval =>
            return Risk_Approval_Recorded;
         when HFT_MiFID.Gateway_Send =>
            return Gateway_Send_Recorded;
         when HFT_MiFID.Venue_Acknowledgement =>
            return Venue_Acknowledgement_Recorded;
         when HFT_MiFID.Partial_Fill =>
            return Partial_Fill_Recorded;
         when HFT_MiFID.Full_Fill =>
            return Full_Fill_Recorded;
         when HFT_MiFID.Cancellation =>
            return Cancellation_Recorded;
         when HFT_MiFID.Correction =>
            return Correction_Recorded;
      end case;
   end Lifecycle_Event;

   procedure Record_Execution_Evidence
     (Evidence : HFT_MiFID.Execution_Evidence;
      Accepted : out Boolean)
   is
      Order_ID : constant Natural :=
        (if Evidence.Child_Order_ID > 0
         then Evidence.Child_Order_ID else Evidence.Parent_Order_ID);
      Clock_OK : constant Boolean :=
        HFT_MiFID.Is_Clock_Compliant (Evidence.Clock);
      Best_OK : constant Boolean :=
        HFT_MiFID.Is_Best_Execution_Evidence_Complete (Evidence);
      Reconciled : constant Boolean :=
        HFT_MiFID.Is_Reconciled (Evidence.Reconciliation);
   begin
      Accepted := Best_OK and Clock_OK and Reconciled;
      Record_Internal
        (Lifecycle_Event (Evidence.Stage),
         (if Accepted then Info else Error),
         MiFID_II_Best_Execution, Order_ID, Evidence.Correlation_ID,
         HFT_Compliance.Performance, "Execution lifecycle evidence",
         Accepted, True, Evidence);
      Record_Internal
        (Best_Execution_Assessed,
         (if Best_OK then Info else Error),
         MiFID_II_Best_Execution, Order_ID, Evidence.Correlation_ID,
         HFT_Compliance.Performance, "Best-execution evidence assessment",
         Best_OK, True, Evidence);

      if Evidence.Market.Is_Stale then
         Record_Internal
           (Stale_Market_Data_Detected, Critical,
            MiFID_II_Best_Execution, Order_ID, Evidence.Correlation_ID,
            HFT_Compliance.Performance, "Stale market data", False,
            True, Evidence);
      end if;
      if not Clock_OK then
         Record_Internal
           (Clock_Drift_Exceeded, Critical, MiFIR_RTS_25, Order_ID,
            Evidence.Correlation_ID, HFT_Compliance.Security,
            "RTS 25 clock evidence outside configured tier", False,
            True, Evidence);
      end if;
      if not Reconciled then
         Record_Internal
           (Reconciliation_Failed, Error, MiFID_II_Best_Execution,
            Order_ID, Evidence.Correlation_ID, HFT_Compliance.Security,
            "Exchange, drop-copy, and clearing records do not reconcile",
            False, True, Evidence);
      end if;
   end Record_Execution_Evidence;

   procedure Record_Clock_Event
     (Event_Type  : Clock_Audit_Event_Type;
      Clock       : HFT_MiFID.Clock_Evidence;
      Description : String)
   is
      Evidence : HFT_MiFID.Execution_Evidence;
   begin
      Evidence.Clock := Clock;
      Record_Internal
        (Event_Type,
         (if Event_Type = Clock_Source_Failed_Over then Warning
          else Critical),
         MiFIR_RTS_25, 0, 0, HFT_Compliance.Security, Description,
         Event_Type = Clock_Source_Failed_Over, True, Evidence);
   end Record_Clock_Event;

   function Get_Audit_Events_By_Order
     (Order_ID : Positive) return Natural is
   begin
      return Store.Count_Order (Order_ID);
   end Get_Audit_Events_By_Order;

   function Get_Audit_Events_By_Type
     (Event_Type : Audit_Event_Type) return Natural is
   begin
      return Store.Count_Type (Event_Type);
   end Get_Audit_Events_By_Type;

   function Get_Audit_Events_By_Severity
     (Severity : Severity_Level) return Natural is
   begin
      return Store.Count_Severity (Severity);
   end Get_Audit_Events_By_Severity;

   function Get_Audit_Statistics return Audit_Statistics is
   begin
      return Store.Statistics;
   end Get_Audit_Statistics;

   function Audit_Event_Count return Natural is
   begin
      return Store.Count;
   end Audit_Event_Count;

   function Get_Audit_Event (Index : Positive) return Audit_Event is
   begin
      return Store.Element (Index);
   end Get_Audit_Event;

   function Verify_Audit_Chain return Boolean is
   begin
      return Store.Chain_Valid;
   end Verify_Audit_Chain;

   function Get_Chain_Head return Hash_Text is
   begin
      return Store.Chain_Head_Text;
   end Get_Chain_Head;

   function Generate_Audit_Summary return Audit_Summary is
      Summary : Audit_Summary;
   begin
      Summary.Start_Time := Audit_Start_Time;
      Summary.End_Time := HFT_Time_Util.Get_UTC_Timestamp_NS;
      Summary.Stats := Store.Statistics;
      Summary.Success_Rate :=
        (if Summary.Stats.Total_Checks = 0 then 0.0
         else Float (Summary.Stats.Total_Passed)
           / Float (Summary.Stats.Total_Checks) * 100.0);
      Summary.Critical_Events := Store.Count_Severity (Critical);
      Summary.Error_Events := Store.Count_Severity (Error);
      Summary.Warning_Events := Store.Count_Severity (Warning);
      Summary.Info_Events := Store.Count_Severity (Info);
      return Summary;
   end Generate_Audit_Summary;

   procedure Print_Audit_Report is
      Summary : constant Audit_Summary := Generate_Audit_Summary;
   begin
      Put_Line ("Ada MiFID Audit Report");
      Put_Line ("Events: " & Natural'Image (Summary.Stats.Total_Events));
      Put_Line ("Checks: " & Natural'Image (Summary.Stats.Total_Checks));
      Put_Line ("Passed: " & Natural'Image (Summary.Stats.Total_Passed));
      Put_Line ("Failed: " & Natural'Image (Summary.Stats.Total_Failed));
      Put_Line
        ("Best-execution events:" &
         Natural'Image (Summary.Stats.Best_Execution_Events));
      Put_Line
        ("RTS 25 clock events:" &
         Natural'Image (Summary.Stats.Clock_Events));
      Put_Line ("Integrity chain head: " & Get_Chain_Head);
   end Print_Audit_Report;

   procedure Print_Audit_History (Max_Events : Positive := 100) is
      Total : constant Natural := Store.Count;
      First : constant Positive :=
        (if Total = 0 then 1
         else Positive (Natural'Max (1, Total - Max_Events + 1)));
   begin
      if Total = 0 then
         Put_Line ("No audit events");
         return;
      end if;
      for I in reverse First .. Total loop
         declare
            Event : constant Audit_Event := Store.Element (I);
         begin
            Put_Line
              ("Event" & Positive'Image (Event.Event_ID) & " " &
               Audit_Event_Type'Image (Event.Event_Type) & " " &
               Event.Record_Hash);
         end;
      end loop;
   end Print_Audit_History;

   procedure Export_Audit_Log (Filename : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      Put_Line
        (File, "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD");
      for I in 1 .. Store.Count loop
         Put_Line (File, Serialized (Store.Element (I)));
      end loop;
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Export_Audit_Log;

   procedure Clear_Audit_History is
   begin
      Store.Reset;
      Audit_Start_Time := HFT_Time_Util.Get_UTC_Timestamp_NS;
   end Clear_Audit_History;

   procedure Configure_Durable_Log
     (Filename : String; Enabled : Boolean := True) is
   begin
      Store.Set_Log (Filename, Enabled);
   end Configure_Durable_Log;

   function Analyze_Compliance_Trend return Trend_Direction is
      Outcome_Count : Natural := 0;
      Earlier_Total, Earlier_Passed : Natural := 0;
      Recent_Total, Recent_Passed : Natural := 0;
   begin
      for I in 1 .. Store.Count loop
         declare
            Event : constant Audit_Event := Store.Element (I);
         begin
            if Event.Event_Type in Compliance_Check_Completed
              | Compliance_Check_Failed
            then
               Outcome_Count := Outcome_Count + 1;
               if Outcome_Count <= Store.Statistics.Total_Checks / 2 then
                  Earlier_Total := Earlier_Total + 1;
                  if Event.Passed then
                     Earlier_Passed := Earlier_Passed + 1;
                  end if;
               else
                  Recent_Total := Recent_Total + 1;
                  if Event.Passed then
                     Recent_Passed := Recent_Passed + 1;
                  end if;
               end if;
            end if;
         end;
      end loop;
      if Earlier_Total = 0 or Recent_Total = 0 then
         return Stable;
      elsif Float (Recent_Passed) / Float (Recent_Total) >
        Float (Earlier_Passed) / Float (Earlier_Total) + 0.05
      then
         return Improving;
      elsif Float (Recent_Passed) / Float (Recent_Total) <
        Float (Earlier_Passed) / Float (Earlier_Total) - 0.05
      then
         return Degrading;
      else
         return Stable;
      end if;
   end Analyze_Compliance_Trend;

   procedure Print_Top_Violations (Top_N : Positive := 10) is
      type Violation_Count is record
         Violation_Type : Audit_Event_Type;
         Count : Natural;
      end record;
      type Violation_Array is array (Positive range <>) of Violation_Count;
      Values : Violation_Array :=
        [(Type_Safety_Violation,
          Store.Count_Type (Type_Safety_Violation)),
         (Contract_Violation, Store.Count_Type (Contract_Violation)),
         (Range_Safety_Violation,
          Store.Count_Type (Range_Safety_Violation)),
         (Coding_Standards_Violation,
          Store.Count_Type (Coding_Standards_Violation)),
         (Security_Violation, Store.Count_Type (Security_Violation)),
         (Performance_Warning, Store.Count_Type (Performance_Warning)),
         (Reconciliation_Failed, Store.Count_Type (Reconciliation_Failed))];
      Printed : Natural := 0;
   begin
      for I in Values'First .. Values'Last loop
         for J in I + 1 .. Values'Last loop
            if Values (J).Count > Values (I).Count then
               declare
                  Temporary : constant Violation_Count := Values (I);
               begin
                  Values (I) := Values (J);
                  Values (J) := Temporary;
               end;
            end if;
         end loop;
      end loop;
      for Value of Values loop
         exit when Printed = Top_N;
         if Value.Count > 0 then
            Put_Line
              (Audit_Event_Type'Image (Value.Violation_Type) & ":" &
               Natural'Image (Value.Count));
            Printed := Printed + 1;
         end if;
      end loop;
   end Print_Top_Violations;

   procedure Configure_Audit (Config : Audit_Config) is
   begin
      Store.Set_Config (Config);
   end Configure_Audit;

   function Get_Audit_Config return Audit_Config is
   begin
      return Store.Config;
   end Get_Audit_Config;

begin
   Initialize_Audit_System;
end HFT_Audit;
