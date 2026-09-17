pragma Ada_2022;

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces.C;
with HFT_Time_Util;
with HFT_SHA256;

package body HFT_Audit is
   use Ada.Text_IO;
   use type GNAT.OS_Lib.File_Descriptor;
   use type Interfaces.C.int;

   function C_Flock
     (FD : Interfaces.C.int; Operation : Interfaces.C.int)
      return Interfaces.C.int
      with Import, Convention => C, External_Name => "flock";

   function C_Fsync (FD : Interfaces.C.int) return Interfaces.C.int
      with Import, Convention => C, External_Name => "fsync";

   function Acquire_Lock (Filename : String)
      return GNAT.OS_Lib.File_Descriptor
   is
      FD : GNAT.OS_Lib.File_Descriptor :=
        GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary);
   begin
      if FD = GNAT.OS_Lib.Invalid_FD then
         FD := GNAT.OS_Lib.Create_New_File
           (Filename, GNAT.OS_Lib.Binary);
         if FD = GNAT.OS_Lib.Invalid_FD then
            FD := GNAT.OS_Lib.Open_Read_Write
              (Filename, GNAT.OS_Lib.Binary);
         end if;
      end if;
      if FD = GNAT.OS_Lib.Invalid_FD
        or else C_Flock (Interfaces.C.int (FD), 2) /= 0
      then
         if FD /= GNAT.OS_Lib.Invalid_FD then
            GNAT.OS_Lib.Close (FD);
         end if;
         raise Audit_Persistence_Error;
      end if;
      return FD;
   end Acquire_Lock;

   procedure Release_Lock
     (FD : in out GNAT.OS_Lib.File_Descriptor) is
   begin
      if FD /= GNAT.OS_Lib.Invalid_FD then
         GNAT.OS_Lib.Close (FD);
         FD := GNAT.OS_Lib.Invalid_FD;
      end if;
   end Release_Lock;

   procedure Sync_File (Filename : String) is
      FD : constant GNAT.OS_Lib.File_Descriptor :=
        GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary);
   begin
      if FD = GNAT.OS_Lib.Invalid_FD
        or else C_Fsync (Interfaces.C.int (FD)) /= 0
      then
         if FD /= GNAT.OS_Lib.Invalid_FD then
            GNAT.OS_Lib.Close (FD);
         end if;
         raise Audit_Persistence_Error;
      end if;
      GNAT.OS_Lib.Close (FD);
   end Sync_File;

   procedure Sync_Parent_Directory (Filename : String) is
      Directory : constant String :=
        Ada.Directories.Containing_Directory (Filename);
      FD : constant GNAT.OS_Lib.File_Descriptor :=
        GNAT.OS_Lib.Open_Read (Directory, GNAT.OS_Lib.Binary);
   begin
      if FD = GNAT.OS_Lib.Invalid_FD
        or else C_Fsync (Interfaces.C.int (FD)) /= 0
      then
         if FD /= GNAT.OS_Lib.Invalid_FD then
            GNAT.OS_Lib.Close (FD);
         end if;
         raise Audit_Persistence_Error;
      end if;
      GNAT.OS_Lib.Close (FD);
   end Sync_Parent_Directory;

   Max_Audit_Events : constant Positive := 10_000;
   type Event_Array is array (Positive range 1 .. Max_Audit_Events)
      of Audit_Event;
   type Pending_Event_Array is array (Positive range <>) of Audit_Event;
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

   function Canonical (Event : Audit_Event) return String is
      E : HFT_MiFID.Execution_Evidence renames Event.MiFID_Evidence;
   begin
      return
        Image (Event.Schema_Version) & "|" &
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
        Boolean_Image (E.Policy.NYSE_Approved) & "|" &
        Boolean_Image (E.Policy.CME_Approved) & "|" &
        Boolean_Image (E.Policy.Other_Approved) & "|" &
        Image (E.Policy.Weights.Price) & "|" &
        Image (E.Policy.Weights.Cost) & "|" &
        Image (E.Policy.Weights.Speed) & "|" &
        Image (E.Policy.Weights.Fill) & "|" &
        Image (E.Policy.Weights.Size_Nature) & "|" &
        HFT_Engine.Price'Image (E.Market.Best_Bid) & "|" &
        HFT_Engine.Price'Image (E.Market.Best_Ask) & "|" &
        Image (Natural (E.Market.Bid_Depth)) & "|" &
        Image (Natural (E.Market.Ask_Depth)) & "|" &
        Image (E.Market.Estimated_Fee_Micros) & "|" &
        Image (Long_Long_Integer (E.Market.Estimated_Latency_NS)) & "|" &
        Image (E.Market.Expected_Fill_BPS) & "|" &
        Image (E.Market.Liquidity_Score_BPS) & "|" &
        Image (Long_Long_Integer (E.Market.Exchange_Timestamp)) & "|" &
        Image (Long_Long_Integer (E.Market.Local_Receipt_Timestamp)) & "|" &
        Image (E.Market.Feed_Sequence) & "|" &
        Boolean_Image (E.Market.Is_Stale) & "|" &
        HFT_Engine.Price'Image (E.Metrics.Arrival_Price) & "|" &
        HFT_Engine.Price'Image (E.Metrics.Execution_Price) & "|" &
        HFT_Engine.Price'Image (E.Metrics.Benchmark_Price) & "|" &
        Image (Natural (E.Metrics.Ordered_Quantity)) & "|" &
        Image (Natural (E.Metrics.Filled_Quantity)) & "|" &
        Image (E.Metrics.Fees_Micros) & "|" &
        Image (Long_Long_Integer
          (E.Metrics.Implementation_Shortfall_BPS)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Spread_Capture_BPS)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Slippage_BPS)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Market_Impact_BPS)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Opportunity_Cost_BPS)) & "|" &
        Image (Long_Long_Integer (E.Metrics.Execution_Latency_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.UTC_Time)) & "|" &
        Image (Long_Long_Integer (E.Clock.Monotonic_Time)) & "|" &
        Image (Long_Long_Integer (E.Clock.UTC_Offset_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.Uncertainty_NS)) & "|" &
        Image (Long_Long_Integer (E.Clock.Granularity_NS)) & "|" &
        HFT_MiFID.RTS25_Tier'Image (E.Clock.Tier) & "|" &
        HFT_MiFID.Synchronization_State'Image (E.Clock.State) & "|" &
        HFT_MiFID.Clock_Source'Image (E.Clock.Source) & "|" &
        HFT_MiFID.Timestamp_Origin'Image (E.Clock.Origin) & "|" &
        Image (Long_Long_Integer (E.Clock.Last_Synchronized_At)) & "|" &
        E.Reconciliation.Exchange_Order_ID & "|" &
        E.Reconciliation.Drop_Copy_ID & "|" &
        E.Reconciliation.Clearing_ID & "|" &
        Boolean_Image (E.Reconciliation.Quantity_Matches) & "|" &
        Boolean_Image (E.Reconciliation.Price_Matches) & "|" &
        Image (Long_Long_Integer
          (E.Reconciliation.Timestamp_Delta_NS));
   end Canonical;

   function Serialized (Event : Audit_Event) return String is
   begin
      return Event.Previous_Hash & "|" & Event.Record_Hash & "|" &
        Canonical (Event);
   end Serialized;

   Log_Header : constant String :=
     "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD";

   function Is_Upper_Hex (Value : String) return Boolean;

   procedure Write_Checkpoint
     (Filename : String; Head : Hash_Text; Next_ID : Positive)
   is
      File : File_Type;
      Renamed : Boolean;
      Temporary : constant String := Filename & ".checkpoint.tmp";
   begin
      Create (File, Out_File, Temporary);
      Put_Line (File, Head & "|" & Image (Natural (Next_ID)));
      Close (File);
      Sync_File (Temporary);
      GNAT.OS_Lib.Rename_File
        (Temporary, Filename & ".checkpoint", Renamed);
      if not Renamed then
         raise Audit_Persistence_Error;
      end if;
      Sync_File (Filename & ".checkpoint");
      Sync_Parent_Directory (Filename);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Write_Checkpoint;

   procedure Read_Checkpoint
     (Filename : String;
      Exists   : out Boolean;
      Head     : out Hash_Text;
      Next_ID  : out Positive)
   is
      File   : File_Type;
      Buffer : String (1 .. 128);
      Last   : Natural;
   begin
      Exists := Ada.Directories.Exists (Filename & ".checkpoint");
      Head := (others => '0');
      Next_ID := 1;
      if not Exists then
         return;
      end if;
      Open (File, In_File, Filename & ".checkpoint");
      Get_Line (File, Buffer, Last);
      Close (File);
      if Last < 66 or else Buffer (65) /= '|'
        or else not Is_Upper_Hex (Buffer (1 .. 64))
      then
         raise Audit_Persistence_Error;
      end if;
      Head := Buffer (1 .. 64);
      Next_ID := Positive'Value (Buffer (66 .. Last));
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Read_Checkpoint;

   procedure Truncate_Log (Filename : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      Put_Line (File, Log_Header);
      Close (File);
      Sync_File (Filename);
      Write_Checkpoint (Filename, (others => '0'), 1);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Truncate_Log;

   function Is_Upper_Hex (Value : String) return Boolean is
   begin
      for C of Value loop
         if not (C in '0' .. '9' or else C in 'A' .. 'F') then
            return False;
         end if;
      end loop;
      return True;
   end Is_Upper_Hex;

   procedure Recover_Log_State
     (Filename : String;
      Head     : out Hash_Text;
      Next_ID  : out Positive)
   is
      File      : File_Type;
      Buffer    : String (1 .. 8_192);
      Last      : Natural;
      Separator : Natural;
      Last_ID   : Natural := 0;
      Line_Number : Natural := 0;
      Expected_Head : Hash_Text := (others => '0');
      Previous_Head : Hash_Text;
      Record_Hash   : Hash_Text;
      Expected_Hash : Hash_Text;
      Current_ID    : Positive;
      Checkpoint_Exists : Boolean;
      Checkpoint_Head   : Hash_Text;
      Checkpoint_ID     : Positive;
      Checkpoint_Matched : Boolean := False;
   begin
      Head := (others => '0');
      Next_ID := 1;
      if not Ada.Directories.Exists (Filename) then
         if Ada.Directories.Exists (Filename & ".checkpoint") then
            raise Audit_Persistence_Error;
         end if;
         Truncate_Log (Filename);
         return;
      end if;
      Read_Checkpoint
        (Filename, Checkpoint_Exists, Checkpoint_Head, Checkpoint_ID);
      Checkpoint_Matched :=
        not Checkpoint_Exists
        or else (Checkpoint_Head = (Hash_Text'(others => '0'))
                 and then Checkpoint_ID = 1);
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         Line_Number := Line_Number + 1;
         if Line_Number = 1 then
            if Buffer (1 .. Last) /= Log_Header then
               raise Audit_Persistence_Error;
            end if;
         else
            if Last < 134 or else Last = Buffer'Last
              or else Buffer (65) /= '|'
              or else Buffer (130) /= '|'
              or else Buffer (131) /= '1'
              or else Buffer (132) /= '|'
            then
               raise Audit_Persistence_Error;
            end if;
            Previous_Head := Buffer (1 .. 64);
            Record_Hash := Buffer (66 .. 129);
            if not Is_Upper_Hex (Previous_Head)
              or else not Is_Upper_Hex (Record_Hash)
              or else Previous_Head /= Expected_Head
            then
               raise Audit_Persistence_Error;
            end if;
            Expected_Hash :=
              HFT_SHA256.Digest
                (Expected_Head & Buffer (131 .. Last));
            if Record_Hash /= Expected_Hash then
               raise Audit_Persistence_Error;
            end if;

            Separator := 133;
            while Separator <= Last
              and then Buffer (Separator) /= '|'
            loop
               Separator := Separator + 1;
            end loop;
            if Separator = 133 or else Separator > Last then
               raise Audit_Persistence_Error;
            end if;
            Current_ID :=
              Positive'Value (Buffer (133 .. Separator - 1));
            if Last_ID = Positive'Last then
               raise Audit_Capacity_Error;
            end if;
            if (Last_ID = 0 and then Current_ID /= 1)
              or else (Last_ID > 0 and then Current_ID /= Last_ID + 1)
            then
               raise Audit_Persistence_Error;
            end if;
            Last_ID := Current_ID;
            Expected_Head := Record_Hash;
            if Checkpoint_Exists
              and then Checkpoint_Head = Expected_Head
              and then Last_ID < Positive'Last
              and then Checkpoint_ID = Last_ID + 1
            then
               Checkpoint_Matched := True;
            end if;
         end if;
      end loop;
      Close (File);
      if Line_Number = 0 then
         raise Audit_Persistence_Error;
      end if;
      Head := Expected_Head;
      if Last_ID = Positive'Last then
         raise Audit_Capacity_Error;
      elsif Last_ID > 0 then
         Next_ID := Last_ID + 1;
      end if;
      if not Checkpoint_Matched then
         raise Audit_Persistence_Error;
      end if;
      if not Checkpoint_Exists
        or else Checkpoint_Head /= Head
        or else Checkpoint_ID /= Next_ID
      then
         Write_Checkpoint (Filename, Head, Next_ID);
      end if;
   exception
      when Audit_Capacity_Error =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Audit_Persistence_Error;
   end Recover_Log_State;

   procedure Recover_Log_State_Locked
     (Filename : String;
      Head     : out Hash_Text;
      Next_ID  : out Positive)
   is
      Handle : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   begin
      Handle := Acquire_Lock (Filename & ".lock");
      Recover_Log_State (Filename, Head, Next_ID);
      Release_Lock (Handle);
   exception
      when others =>
         Release_Lock (Handle);
         raise Audit_Persistence_Error;
   end Recover_Log_State_Locked;

   procedure Copy_Verified_Log
     (Source, Destination : String)
   is
      Source_Path    : constant String := Ada.Directories.Full_Name (Source);
      Destination_Path : constant String :=
        Ada.Directories.Full_Name (Destination);
      Input, Output : File_Type;
      Buffer        : String (1 .. 8_192);
      Last          : Natural;
      Head          : Hash_Text;
      Next_ID       : Positive;
      Source_Lock   : constant String := Source_Path & ".lock";
      Destination_Lock : constant String := Destination_Path & ".lock";
      Source_Handle : GNAT.OS_Lib.File_Descriptor :=
        GNAT.OS_Lib.Invalid_FD;
      Destination_Handle : GNAT.OS_Lib.File_Descriptor :=
        GNAT.OS_Lib.Invalid_FD;
      Temporary : constant String := Destination_Path & ".export.tmp";
      Renamed   : Boolean;
   begin
      if Source_Path = Destination_Path
        or else Destination_Path = Source_Path & ".checkpoint"
        or else Destination_Path = Source_Path & ".checkpoint.tmp"
        or else Destination_Path = Source_Path & ".lock"
        or else Destination_Path = Source_Path & ".export.tmp"
      then
         raise Constraint_Error with "audit export destination is source";
      end if;
      if Source_Lock < Destination_Lock then
         Source_Handle := Acquire_Lock (Source_Lock);
         Destination_Handle := Acquire_Lock (Destination_Lock);
      else
         Destination_Handle := Acquire_Lock (Destination_Lock);
         Source_Handle := Acquire_Lock (Source_Lock);
      end if;
      if Ada.Directories.Exists (Destination_Path)
        or else Ada.Directories.Exists (Destination_Path & ".checkpoint")
      then
         raise Constraint_Error with
           "audit export destination already exists";
      end if;
      Recover_Log_State (Source_Path, Head, Next_ID);
      Open (Input, In_File, Source_Path);
      Create (Output, Out_File, Temporary);
      while not End_Of_File (Input) loop
         Get_Line (Input, Buffer, Last);
         Put_Line (Output, Buffer (1 .. Last));
      end loop;
      Close (Input);
      Close (Output);
      Sync_File (Temporary);
      GNAT.OS_Lib.Rename_File (Temporary, Destination_Path, Renamed);
      if not Renamed then
         raise Audit_Persistence_Error;
      end if;
      Sync_Parent_Directory (Destination_Path);
      Write_Checkpoint (Destination_Path, Head, Next_ID);
      Release_Lock (Destination_Handle);
      Release_Lock (Source_Handle);
   exception
      when Constraint_Error =>
         if Is_Open (Input) then
            Close (Input);
         end if;
         if Is_Open (Output) then
            Close (Output);
         end if;
         Release_Lock (Destination_Handle);
         Release_Lock (Source_Handle);
         raise;
      when others =>
         if Is_Open (Input) then
            Close (Input);
         end if;
         if Is_Open (Output) then
            Close (Output);
         end if;
         Release_Lock (Destination_Handle);
         Release_Lock (Source_Handle);
         raise Audit_Persistence_Error;
   end Copy_Verified_Log;

   procedure Append_Line
     (Filename      : String;
      Value         : String;
      Expected_Head : Hash_Text;
      Expected_ID   : Positive)
   is
      File      : File_Type;
      Disk_Head : Hash_Text;
      Disk_ID   : Positive;
      Handle    : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   begin
      Handle := Acquire_Lock (Filename & ".lock");
      Recover_Log_State (Filename, Disk_Head, Disk_ID);
      if Disk_Head /= Expected_Head or else Disk_ID /= Expected_ID then
         raise Audit_Persistence_Error;
      end if;
      Open (File, Append_File, Filename);
      Put_Line (File, Value);
      Close (File);
      Sync_File (Filename);
      Write_Checkpoint
        (Filename,
         Value (Value'First + 65 .. Value'First + 128),
         Expected_ID + 1);
      Release_Lock (Handle);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Release_Lock (Handle);
         raise Audit_Persistence_Error;
   end Append_Line;

   protected Store is
      procedure Initialize;
      procedure Reset;
      procedure Reserve (Slots : Positive);
      procedure Append
        (Candidate : Audit_Event;
         Stored    : out Boolean;
         Reserved  : Boolean := False);
      procedure Append_Batch
        (Candidates   : Pending_Event_Array;
         Stored_Count : out Natural);
      procedure Set_Config (Config : Audit_Config);
      function Config return Audit_Config;
      procedure Set_Log (Filename : String; Enabled : Boolean);
      procedure Export_Log (Filename : String);
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
      Chain_Head   : Hash_Text := (others => '0');
      Session_Base_Hash : Hash_Text := (others => '0');
      Reserved_Slots : Natural := 0;
      Durable_Log_Enabled : Boolean := True;
      Log_Path     : Path_Buffer :=
        "hft_audit_chain.log" & (1 .. 237 => ' ');
      Log_Path_Length : Natural := 19;
   end Store;

   protected body Store is
      procedure Initialize is
         Recovered_Head : Hash_Text := (others => '0');
         Recovered_ID   : Positive := 1;
      begin
         if Durable_Log_Enabled then
            Recover_Log_State_Locked
              (Log_Path (1 .. Log_Path_Length),
               Recovered_Head, Recovered_ID);
         end if;
         Event_Count := 0;
         Reserved_Slots := 0;
         Current_Stats := (others => 0);
         Chain_Head := Recovered_Head;
         Next_ID := Recovered_ID;
         Session_Base_Hash := Recovered_Head;
      end Initialize;

      procedure Reset is
      begin
         Event_Count := 0;
         Reserved_Slots := 0;
         Current_Stats := (others => 0);
         if Durable_Log_Enabled then
            Session_Base_Hash := Chain_Head;
         else
            Next_ID := 1;
            Chain_Head := (others => '0');
            Session_Base_Hash := (others => '0');
         end if;
      end Reset;

      procedure Reserve (Slots : Positive) is
      begin
         if Event_Count + Reserved_Slots + Slots >
           Current_Config.Max_History_Size
           or else Event_Count + Reserved_Slots + Slots >
             Max_Audit_Events
           or else Next_ID > Positive'Last - Slots
         then
            raise Audit_Capacity_Error;
         end if;
         Reserved_Slots := Reserved_Slots + Slots;
      end Reserve;

      procedure Append
        (Candidate : Audit_Event;
         Stored    : out Boolean;
         Reserved  : Boolean := False)
      is
         Event : Audit_Event := Candidate;
         Should_Log : constant Boolean :=
           Current_Config.Log_All_Events
           or else (Candidate.Passed and Current_Config.Log_Passed_Checks)
           or else ((not Candidate.Passed)
                    and Current_Config.Log_Failed_Checks);
         New_Hash : Hash_Text;
      begin
         Stored := False;
         if Reserved and then Reserved_Slots = 0 then
            raise Program_Error with "missing audit reservation";
         end if;
         if not Current_Config.Enable_Audit or else not Should_Log then
            if Reserved then
               Reserved_Slots := Reserved_Slots - 1;
            end if;
            return;
         end if;
         if not Reserved then
            if Event_Count + Reserved_Slots >=
              Current_Config.Max_History_Size
              or else Event_Count + Reserved_Slots = Max_Audit_Events
              or else Next_ID = Positive'Last
            then
               raise Audit_Capacity_Error;
            end if;
         end if;

         Event.Event_ID := Next_ID;
         Event.Previous_Hash := Chain_Head;
         New_Hash := HFT_SHA256.Digest (Chain_Head & Canonical (Event));
         Event.Record_Hash := New_Hash;

         if Durable_Log_Enabled then
            Append_Line
              (Log_Path (1 .. Log_Path_Length), Serialized (Event),
               Chain_Head, Next_ID);
         end if;

         Event_Count := Event_Count + 1;
         Events (Event_Count) := Event;
         Chain_Head := New_Hash;
         if Reserved then
            Reserved_Slots := Reserved_Slots - 1;
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

      procedure Append_Batch
        (Candidates   : Pending_Event_Array;
         Stored_Count : out Natural)
      is
         Stored : Boolean;
         Storable : Natural := 0;

         function Will_Log (Candidate : Audit_Event) return Boolean is
         begin
            return Current_Config.Enable_Audit
              and then
                (Current_Config.Log_All_Events
                 or else
                   (Candidate.Passed
                    and Current_Config.Log_Passed_Checks)
                 or else
                   ((not Candidate.Passed)
                    and Current_Config.Log_Failed_Checks));
         end Will_Log;
      begin
         Stored_Count := 0;
         for Candidate of Candidates loop
            if Will_Log (Candidate) then
               Storable := Storable + 1;
            end if;
         end loop;
         if Storable > 0 then
            Reserve (Storable);
         end if;
         begin
            for Candidate of Candidates loop
               Append
                 (Candidate, Stored,
                  Reserved => Will_Log (Candidate));
               if Stored then
                  Stored_Count := Stored_Count + 1;
               end if;
            end loop;
         exception
            when others =>
               Reserved_Slots := 0;
               raise;
         end;
      end Append_Batch;

      procedure Set_Config (Config : Audit_Config) is
      begin
         Current_Config := Config;
      end Set_Config;

      function Config return Audit_Config is
      begin
         return Current_Config;
      end Config;

      procedure Set_Log (Filename : String; Enabled : Boolean) is
         Recovered_Head : Hash_Text;
         Recovered_ID   : Positive;
         New_Path       : Path_Buffer := (others => ' ');
      begin
         if Enabled
           and then (Filename'Length = 0
                     or else Filename'Length > Log_Path'Length)
         then
            raise Constraint_Error with "invalid audit log filename";
         end if;
         if Enabled and then Event_Count > 0 then
            raise Constraint_Error with
              "configure durable log before recording audit events";
         end if;
         if Enabled then
            New_Path (1 .. Filename'Length) := Filename;
            Recover_Log_State_Locked
              (Filename, Recovered_Head, Recovered_ID);
            Log_Path := New_Path;
            Log_Path_Length := Filename'Length;
            Chain_Head := Recovered_Head;
            Next_ID := Recovered_ID;
            Session_Base_Hash := Recovered_Head;
         end if;
         Durable_Log_Enabled := Enabled;
      end Set_Log;

      procedure Export_Log (Filename : String) is
      begin
         if not Durable_Log_Enabled then
            raise Audit_Persistence_Error;
         end if;
         Copy_Verified_Log
           (Log_Path (1 .. Log_Path_Length), Filename);
      end Export_Log;

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
         Previous : Hash_Text := Session_Base_Hash;
         Expected : Hash_Text;
      begin
         for I in 1 .. Event_Count loop
            if Events (I).Previous_Hash /= Previous then
               return False;
            end if;
            Expected :=
              HFT_SHA256.Digest (Previous & Canonical (Events (I)));
            if Events (I).Record_Hash /= Expected then
               return False;
            end if;
            Previous := Expected;
         end loop;
         return Previous = Chain_Head;
      end Chain_Valid;

      function Chain_Head_Text return Hash_Text is
      begin
         return Chain_Head;
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

   function Safe_Field (Value : String) return Boolean is
   begin
      for C of Value loop
         if C = '|' or else Character'Pos (C) < 32
           or else Character'Pos (C) = 127
         then
            return False;
         end if;
      end loop;
      return True;
   end Safe_Field;

   function Evidence_Fields_Safe
     (Evidence : HFT_MiFID.Execution_Evidence) return Boolean is
   begin
      return Safe_Field (Evidence.Instrument)
        and then Safe_Field (Evidence.Related_Instrument)
        and then Safe_Field (Evidence.Currency)
        and then Safe_Field (Evidence.Futures_Expiry)
        and then Safe_Field (Evidence.Signal_ID)
        and then Safe_Field (Evidence.Client_Mandate)
        and then Safe_Field (Evidence.Strategy_Constraints)
        and then Safe_Field (Evidence.Hedge_Objective)
        and then Safe_Field (Evidence.Roll_Decision)
        and then Safe_Field (Evidence.Routing_Rationale)
        and then Safe_Field (Evidence.Override_Identity)
        and then Safe_Field (Evidence.Override_Reason)
        and then Safe_Field (Evidence.Policy.Version)
        and then Safe_Field (Evidence.Build_ID)
        and then Safe_Field (Evidence.Reconciliation.Exchange_Order_ID)
        and then Safe_Field (Evidence.Reconciliation.Drop_Copy_ID)
        and then Safe_Field (Evidence.Reconciliation.Clearing_ID);
   end Evidence_Fields_Safe;

   function Make_Event
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
      return Audit_Event
   is
      Event : Audit_Event;
   begin
      if not Safe_Field (Description)
        or else (Has_Evidence and then not Evidence_Fields_Safe (Evidence))
      then
         raise Constraint_Error with
           "audit text contains a reserved delimiter or control character";
      end if;
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
      return Event;
   end Make_Event;

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
      Event : constant Audit_Event :=
        Make_Event
          (Event_Type, Severity, Domain, Order_ID, Correlation_ID,
           Category, Description, Passed, Has_Evidence, Evidence);
      Stored : Boolean;
   begin
      Store.Append (Event, Stored);
   end Record_Internal;

   procedure Initialize_Audit_System is
   begin
      Audit_Start_Time := HFT_Time_Util.Get_UTC_Timestamp_NS;
      Store.Initialize;
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
      Recorded_At : constant HFT_Engine.UTC_Timestamp_NS :=
        HFT_Time_Util.Get_UTC_Timestamp_NS;
      Order_ID : constant Natural :=
        (if Evidence.Child_Order_ID > 0
         then Evidence.Child_Order_ID else Evidence.Parent_Order_ID);
      Clock_OK : constant Boolean :=
        HFT_MiFID.Is_Clock_Compliant (Evidence.Clock)
        and then HFT_MiFID.Is_Recording_Time_Valid
          (Evidence, Recorded_At);
      Best_OK : constant Boolean :=
        HFT_MiFID.Is_Best_Execution_Evidence_Complete (Evidence);
      Reconciled : constant Boolean :=
        not HFT_MiFID.Requires_Reconciliation (Evidence)
        or else HFT_MiFID.Is_Reconciled (Evidence.Reconciliation);
      Pending : Pending_Event_Array (1 .. 5);
      Pending_Count : Natural := 0;
      Stored_Count : Natural;

      procedure Add
        (Event_Type  : Audit_Event_Type;
         Severity    : Severity_Level;
         Domain      : Regulatory_Domain;
         Category    : HFT_Compliance.Compliance_Category;
         Description : String;
         Passed      : Boolean) is
      begin
         Pending_Count := Pending_Count + 1;
         Pending (Pending_Count) :=
           Make_Event
             (Event_Type, Severity, Domain, Order_ID,
              Evidence.Correlation_ID, Category, Description, Passed,
              True, Evidence);
      end Add;
   begin
      Accepted := Best_OK and then Clock_OK and then Reconciled;
      Add
        (Lifecycle_Event (Evidence.Stage),
         (if Accepted then Info else Error),
         MiFID_II_Best_Execution, HFT_Compliance.Performance,
         "Execution lifecycle evidence", Accepted);
      Add
        (Best_Execution_Assessed,
         (if Best_OK then Info else Error),
         MiFID_II_Best_Execution, HFT_Compliance.Performance,
         "Best-execution evidence assessment", Best_OK);

      if Evidence.Market.Is_Stale then
         Add
           (Stale_Market_Data_Detected, Critical,
            MiFID_II_Best_Execution, HFT_Compliance.Performance,
            "Stale market data", False);
      end if;
      if not Clock_OK then
         Add
           (Clock_Drift_Exceeded, Critical, MiFIR_RTS_25,
            HFT_Compliance.Security,
            "RTS 25 clock evidence outside configured tier", False);
      end if;
      if not Reconciled then
         Add
           (Reconciliation_Failed, Error, MiFID_II_Best_Execution,
            HFT_Compliance.Security,
            "Exchange, drop-copy, and clearing records do not reconcile",
            False);
      end if;
      Store.Append_Batch (Pending (1 .. Pending_Count), Stored_Count);
      if Stored_Count > Pending_Count then
         raise Program_Error with "invalid audit batch result";
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

   function Evidence_Digest
     (Evidence : HFT_MiFID.Execution_Evidence) return Hash_Text
   is
      Event : Audit_Event;
      Zero_Head : constant Hash_Text := (others => '0');
   begin
      if not Evidence_Fields_Safe (Evidence) then
         raise Constraint_Error with
           "audit text contains a reserved delimiter or control character";
      end if;
      Event.Domain := MiFID_II_Best_Execution;
      Event.Event_Type := Best_Execution_Assessed;
      Event.Description :=
        Fixed_Description ("Deterministic evidence digest");
      Event.Has_MiFID_Evidence := True;
      Event.MiFID_Evidence := Evidence;
      Event.Correlation_ID := Evidence.Correlation_ID;
      Event.Order_ID :=
        (if Evidence.Child_Order_ID > 0
         then Evidence.Child_Order_ID else Evidence.Parent_Order_ID);
      return HFT_SHA256.Digest (Zero_Head & Canonical (Event));
   end Evidence_Digest;

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
   begin
      Store.Export_Log (Filename);
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
