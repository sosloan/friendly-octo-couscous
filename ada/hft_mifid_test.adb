with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with HFT_Audit; use HFT_Audit;
with HFT_Engine;
with HFT_MiFID; use HFT_MiFID;
with HFT_SHA256;
with HFT_Time_Util;

procedure HFT_MiFID_Test is
   use type HFT_Engine.Monotonic_Timestamp_NS;
   use type HFT_Engine.Quantity;
   use type HFT_Engine.UTC_Timestamp_NS;
   Tests  : Natural := 0;
   Passed : Natural := 0;

   procedure Assert (Condition : Boolean; Name : String) is
   begin
      Tests := Tests + 1;
      if Condition then
         Passed := Passed + 1;
         Put_Line ("  PASS " & Name);
      else
         Put_Line ("  FAIL " & Name);
      end if;
   end Assert;

   procedure Assert_Digest_Changes
     (Baseline : Hash_Text;
      Mutant   : Execution_Evidence;
      Name     : String) is
   begin
      Assert (Evidence_Digest (Mutant) /= Baseline, Name);
   end Assert_Digest_Changes;

   procedure Delete_If_Exists (Filename : String) is
   begin
      if Ada.Directories.Exists (Filename) then
         Ada.Directories.Delete_File (Filename);
      end if;
   end Delete_If_Exists;

   function Valid_Clock return Clock_Evidence is
      Result : Clock_Evidence;
   begin
      Result.UTC_Time := HFT_Time_Util.Get_UTC_Timestamp_NS;
      Result.Monotonic_Time :=
        HFT_Time_Util.Get_Monotonic_Timestamp_NS;
      if Result.Monotonic_Time = 0 then
         Result.Monotonic_Time := 1;
      end if;
      Result.Tier := High_Frequency_Electronic;
      Result.State := In_Sync;
      Result.Source := PTP_Primary;
      Result.Origin := Hardware;
      Result.UTC_Offset_NS := 50_000;
      Result.Uncertainty_NS := 40_000;
      Result.Granularity_NS := 1_000;
      Result.Last_Synchronized_At := Result.UTC_Time;
      return Result;
   end Valid_Clock;

   function NYSE_Evidence return Execution_Evidence is
      Result : Execution_Evidence;
      Now    : constant HFT_Engine.UTC_Timestamp_NS :=
        HFT_Time_Util.Get_UTC_Timestamp_NS;
   begin
      Result.Parent_Order_ID := 10_001;
      Result.Child_Order_ID := 10_002;
      Result.Correlation_ID := 90_001;
      Result.Stage := Full_Fill;
      Result.Selected_Venue := NYSE;
      Result.Asset_Class := Cash_Equity;
      Result.Instrument := To_Instrument ("AAPL");
      Result.Related_Instrument := To_Instrument ("ESZ6");
      Result.Session := NYSE_Continuous;
      Result.Order_Type := Limit;
      Result.Signal_ID := To_Short_Text ("FUNDAMENTAL-2026-09");
      Result.Client_Mandate :=
        To_Long_Text ("Seek best total consideration");
      Result.Strategy_Constraints :=
        To_Long_Text ("NYSE cash equity; limit price 250.00");
      Result.Hedge_Objective :=
        To_Long_Text ("Portfolio beta hedge is assessed separately");
      Result.Routing_Rationale :=
        To_Long_Text ("Best displayed total consideration and fill likelihood");
      Result.Policy.Version := To_Short_Text ("BEST-EXEC-2026.09");
      Result.Build_ID := To_Short_Text ("ada-mifid-test");
      Result.Market.Best_Bid := 229.90;
      Result.Market.Best_Ask := 230.00;
      Result.Market.Bid_Depth := 1_000;
      Result.Market.Ask_Depth := 1_200;
      Result.Market.Estimated_Fee_Micros := 25_000;
      Result.Market.Estimated_Latency_NS := 80_000;
      Result.Market.Expected_Fill_BPS := 9_500;
      Result.Market.Liquidity_Score_BPS := 9_000;
      Result.Market.Exchange_Timestamp := Now - 10_000;
      Result.Market.Local_Receipt_Timestamp := Now;
      Result.Market.Feed_Sequence := 1_234_567;
      Result.Market.Is_Stale := False;
      Result.Metrics.Arrival_Price := 230.00;
      Result.Metrics.Execution_Price := 229.99;
      Result.Metrics.Benchmark_Price := 230.00;
      Result.Metrics.Ordered_Quantity := 100;
      Result.Metrics.Filled_Quantity := 100;
      Result.Metrics.Fees_Micros := 25_000;
      Result.Metrics.Implementation_Shortfall_BPS := -1;
      Result.Metrics.Spread_Capture_BPS := 1;
      Result.Metrics.Slippage_BPS := -1;
      Result.Metrics.Market_Impact_BPS := 0;
      Result.Metrics.Opportunity_Cost_BPS := 0;
      Result.Metrics.Execution_Latency_NS := 95_000;
      Result.Clock := Valid_Clock;
      Result.Reconciliation.Exchange_Order_ID :=
        To_Short_Text ("NYSE-ORDER-1");
      Result.Reconciliation.Drop_Copy_ID :=
        To_Short_Text ("NYSE-DROP-1");
      Result.Reconciliation.Clearing_ID :=
        To_Short_Text ("NYSE-CLEAR-1");
      Result.Reconciliation.Quantity_Matches := True;
      Result.Reconciliation.Price_Matches := True;
      Result.Reconciliation.Timestamp_Delta_NS := 20_000;
      return Result;
   end NYSE_Evidence;

   Equity, Future_Order : Execution_Evidence;
   Clock : Clock_Evidence;
   Accepted : Boolean;
begin
   Put_Line ("MiFID II best-execution and RTS 25 tests");
   Delete_If_Exists ("/tmp/hft_mifid_audit.log");
   Delete_If_Exists ("/tmp/hft_mifid_audit.log.checkpoint");
   Delete_If_Exists ("/tmp/hft_mifid_export.log");
   Delete_If_Exists ("/tmp/hft_mifid_export.log.checkpoint");
   Delete_If_Exists ("/tmp/hft_mifid_tampered.log");
   Delete_If_Exists ("/tmp/hft_mifid_missing.log");
   Delete_If_Exists ("/tmp/hft_mifid_missing.log.checkpoint");
   Configure_Durable_Log ("/tmp/hft_mifid_audit.log");
   Clear_Audit_History;

   Assert
     (HFT_SHA256.Digest ("abc") =
      "BA7816BF8F01CFEA414140DE5DAE2223" &
      "B00361A396177A9CB410FF61F20015AD",
      "SHA-256 known-answer vector");

   Clock := Valid_Clock;
   Assert (Is_Clock_Compliant (Clock), "PTP clock within HFT tier");
   Clock.UTC_Offset_NS := 60_001;
   Assert
     (not Is_Clock_Compliant (Clock),
      "offset plus uncertainty beyond 100 microseconds rejected");
   Clock := Valid_Clock;
   Clock.State := Out_Of_Sync;
   Assert
     (not Is_Clock_Compliant (Clock), "loss of synchronization rejected");
   Clock := Valid_Clock;
   Clock.Granularity_NS := 1_001;
   Assert
     (not Is_Clock_Compliant (Clock), "coarse HFT timestamp rejected");

   Equity := NYSE_Evidence;
   Assert
     (Is_Best_Execution_Evidence_Complete (Equity),
      "NYSE best-execution evidence complete");
   Assert (Is_Audit_Ready (Equity), "NYSE evidence audit ready");

   declare
      Baseline : constant Hash_Text := Evidence_Digest (Equity);
      Mutant   : Execution_Evidence;
   begin
      Mutant := Equity;
      Mutant.Policy.NYSE_Approved := False;
      Assert_Digest_Changes
        (Baseline, Mutant, "venue approvals protected by digest");
      Mutant := Equity;
      Mutant.Policy.Weights.Price := 3_999;
      Assert_Digest_Changes
        (Baseline, Mutant, "policy weights protected by digest");
      Mutant := Equity;
      Mutant.Market.Bid_Depth := Mutant.Market.Bid_Depth + 1;
      Assert_Digest_Changes
        (Baseline, Mutant, "market depth protected by digest");
      Mutant := Equity;
      Mutant.Market.Estimated_Fee_Micros :=
        Mutant.Market.Estimated_Fee_Micros + 1;
      Assert_Digest_Changes
        (Baseline, Mutant, "estimated fees protected by digest");
      Mutant := Equity;
      Mutant.Market.Liquidity_Score_BPS :=
        Mutant.Market.Liquidity_Score_BPS - 1;
      Assert_Digest_Changes
        (Baseline, Mutant, "liquidity protected by digest");
      Mutant := Equity;
      Mutant.Metrics.Benchmark_Price := 230.01;
      Assert_Digest_Changes
        (Baseline, Mutant, "benchmark protected by digest");
      Mutant := Equity;
      Mutant.Metrics.Implementation_Shortfall_BPS := 2;
      Assert_Digest_Changes
        (Baseline, Mutant, "transaction costs protected by digest");
      Mutant := Equity;
      Mutant.Clock.Origin := Kernel;
      Assert_Digest_Changes
        (Baseline, Mutant, "clock origin protected by digest");
      Mutant := Equity;
      Mutant.Clock.Last_Synchronized_At :=
        Mutant.Clock.Last_Synchronized_At - 1;
      Assert_Digest_Changes
        (Baseline, Mutant, "clock synchronization time protected by digest");
      Mutant := Equity;
      Mutant.Reconciliation.Timestamp_Delta_NS := 20_001;
      Assert_Digest_Changes
        (Baseline, Mutant, "reconciliation delta protected by digest");
   end;

   Future_Order := Equity;
   Future_Order.Parent_Order_ID := 20_001;
   Future_Order.Child_Order_ID := 20_002;
   Future_Order.Correlation_ID := 90_002;
   Future_Order.Selected_Venue := CME;
   Future_Order.Asset_Class := Futures;
   Future_Order.Instrument := To_Instrument ("ESZ6");
   Future_Order.Related_Instrument := To_Instrument ("AAPL");
   Future_Order.Futures_Expiry := To_Short_Text ("2026-12");
   Future_Order.Session := CME_Globex;
   Future_Order.Order_Type := Spread;
   Future_Order.Roll_Decision :=
     To_Long_Text ("December contract selected; no roll required");
   Future_Order.Reconciliation.Exchange_Order_ID :=
     To_Short_Text ("CME-ORDER-1");
   Future_Order.Reconciliation.Drop_Copy_ID :=
     To_Short_Text ("CME-DROP-1");
   Future_Order.Reconciliation.Clearing_ID :=
     To_Short_Text ("CME-CLEAR-1");
   Assert (Is_Audit_Ready (Future_Order), "CME futures evidence audit ready");
   Assert
     (not Economically_Equivalent (Equity, Future_Order),
      "cash equity and futures are not equivalent alternatives");

   declare
       Wrong_Session : Execution_Evidence := Future_Order;
   begin
       Wrong_Session.Session := NYSE_Continuous;
       Assert
         (not Is_Audit_Ready (Wrong_Session),
          "CME futures with NYSE session rejected");
   end;

   declare
       Incomplete_Spread : Execution_Evidence := Future_Order;
   begin
       Incomplete_Spread.Related_Instrument := To_Instrument ("");
       Assert
         (not Is_Audit_Ready (Incomplete_Spread),
          "spread without related instrument rejected");
   end;

   declare
      Stale : Execution_Evidence := Equity;
   begin
      Stale.Market.Is_Stale := True;
      Assert
        (not Is_Best_Execution_Evidence_Complete (Stale),
         "stale quote evidence rejected");
   end;

   declare
      Bad_Reconciliation : Execution_Evidence := Equity;
   begin
      Bad_Reconciliation.Reconciliation.Price_Matches := False;
      Assert
        (not Is_Audit_Ready (Bad_Reconciliation),
         "exchange reconciliation mismatch rejected");
   end;

   declare
      Bad_Fill : Execution_Evidence := Equity;
   begin
      Bad_Fill.Metrics.Filled_Quantity := 0;
      Assert
        (not Is_Audit_Ready (Bad_Fill),
         "zero-quantity full fill rejected");
   end;

   declare
      Old_Sync : Execution_Evidence := Equity;
   begin
      Old_Sync.Clock.Last_Synchronized_At :=
        Old_Sync.Clock.UTC_Time -
          HFT_Engine.UTC_Timestamp_NS (Maximum_Synchronization_Age_NS) - 1;
      Assert
        (not Is_Audit_Ready (Old_Sync),
         "stale synchronization evidence rejected");
   end;

   declare
      Unsafe : Execution_Evidence := Equity;
      Before : constant Natural := Audit_Event_Count;
   begin
      Unsafe.Routing_Rationale (1) := ASCII.LF;
      begin
         Record_Execution_Evidence (Unsafe, Accepted);
         Assert (False, "control character rejected");
      exception
         when Constraint_Error =>
            Assert
              (Audit_Event_Count = Before,
               "control character rejected before persistence");
      end;
   end;

   declare
      Config : Audit_Config := Get_Audit_Config;
   begin
      Config.Max_History_Size := 1;
      Config.Enable_Audit := False;
      Configure_Audit (Config);
      Record_Execution_Evidence (NYSE_Evidence, Accepted);
      Assert
        (Audit_Event_Count = 0,
         "disabled audit does not consume batch capacity");
      Config.Enable_Audit := True;
      Configure_Audit (Config);
      begin
         Record_Execution_Evidence (NYSE_Evidence, Accepted);
         Assert (False, "multi-event capacity reserved atomically");
      exception
         when Audit_Capacity_Error =>
            Assert
              (Audit_Event_Count = 0,
               "multi-event capacity reserved atomically");
      end;
      Config.Max_History_Size := 10_000;
      Configure_Audit (Config);
   end;

   for Stage in Lifecycle_Stage loop
      declare
         Stage_Evidence : Execution_Evidence := NYSE_Evidence;
      begin
         Stage_Evidence.Stage := Stage;
         case Stage is
            when Market_Data_Receipt | Strategy_Decision |
                 Risk_Approval | Gateway_Send =>
               Stage_Evidence.Metrics.Filled_Quantity := 0;
               Stage_Evidence.Metrics.Execution_Price := 0.0;
               Stage_Evidence.Reconciliation := (others => <>);
            when Venue_Acknowledgement =>
               Stage_Evidence.Metrics.Filled_Quantity := 0;
               Stage_Evidence.Metrics.Execution_Price := 0.0;
               Stage_Evidence.Reconciliation := (others => <>);
               Stage_Evidence.Reconciliation.Exchange_Order_ID :=
                 To_Short_Text ("NYSE-ORDER-1");
            when Partial_Fill =>
               Stage_Evidence.Metrics.Filled_Quantity := 50;
            when Full_Fill | Correction =>
               Stage_Evidence.Metrics.Filled_Quantity := 100;
            when Cancellation =>
               Stage_Evidence.Metrics.Filled_Quantity := 0;
               Stage_Evidence.Metrics.Execution_Price := 0.0;
               Stage_Evidence.Reconciliation := (others => <>);
               Stage_Evidence.Reconciliation.Exchange_Order_ID :=
                 To_Short_Text ("NYSE-ORDER-1");
         end case;
         Record_Execution_Evidence (Stage_Evidence, Accepted);
         Assert (Accepted, Lifecycle_Stage'Image (Stage) & " recorded");
      end;
   end loop;

   Future_Order.Clock := Valid_Clock;
   Future_Order.Market.Exchange_Timestamp :=
     Future_Order.Clock.UTC_Time - 10_000;
   Future_Order.Market.Local_Receipt_Timestamp :=
     Future_Order.Clock.UTC_Time;
   Record_Execution_Evidence (Future_Order, Accepted);
   Assert (Accepted, "CME evidence recorded");
   Assert
     (Get_Audit_Events_By_Type (Best_Execution_Assessed) = 10,
      "best-execution assessments counted");

   declare
      Replay : Execution_Evidence := NYSE_Evidence;
      Two_Seconds : constant HFT_Engine.UTC_Timestamp_NS :=
        2_000_000_000;
   begin
      Replay.Clock.UTC_Time := Replay.Clock.UTC_Time - Two_Seconds;
      Replay.Clock.Last_Synchronized_At :=
        Replay.Clock.Last_Synchronized_At - Two_Seconds;
      Replay.Market.Exchange_Timestamp :=
        Replay.Market.Exchange_Timestamp - Two_Seconds;
      Replay.Market.Local_Receipt_Timestamp :=
        Replay.Market.Local_Receipt_Timestamp - Two_Seconds;
      Record_Execution_Evidence (Replay, Accepted);
      Assert (not Accepted, "replayed stale evidence rejected at recording");
   end;
   Assert (Verify_Audit_Chain, "in-memory SHA-256 chain verifies");
   Assert
     (Get_Chain_Head /= (Hash_Text'(others => '0')),
      "integrity chain advances");
   Assert
     (Get_Audit_Event (1).Has_MiFID_Evidence,
      "canonical event retains replay evidence");

   Clock := Valid_Clock;
   Clock.State := Out_Of_Sync;
   Record_Clock_Event
     (Clock_Synchronization_Lost, Clock, "PTP source unavailable");
   Record_Clock_Event
     (Clock_Rollback_Detected, Clock, "UTC clock moved backwards");
   Assert
     (Get_Audit_Events_By_Type (Clock_Synchronization_Lost) = 1,
      "clock-loss event explicit");
   Assert
     (Get_Audit_Events_By_Type (Clock_Rollback_Detected) = 1,
      "clock-rollback event explicit");
   Assert (Verify_Audit_Chain, "chain verifies after clock events");

   declare
      Previous_Head : constant Hash_Text := Get_Chain_Head;
      Previous_ID   : constant Positive :=
        Get_Audit_Event (Audit_Event_Count).Event_ID;
   begin
      Initialize_Audit_System;
      Assert
        (Get_Chain_Head = Previous_Head,
         "durable chain head recovered after reinitialization");
      Record_Clock_Event
        (Clock_Source_Failed_Over, Valid_Clock,
         "PTP primary changed to redundant source");
      Assert
        (Get_Audit_Event (1).Event_ID = Previous_ID + 1,
         "durable event sequence continues after reinitialization");
      Assert (Verify_Audit_Chain, "continued durable chain verifies");
      declare
         Current_Head : constant Hash_Text := Get_Chain_Head;
         Stale_Checkpoint : File_Type;
      begin
         Create
           (Stale_Checkpoint, Out_File,
            "/tmp/hft_mifid_audit.log.checkpoint");
         Put_Line
           (Stale_Checkpoint,
            Previous_Head & "|" & Positive'Image (Previous_ID + 1));
         Close (Stale_Checkpoint);
         Initialize_Audit_System;
         Assert
           (Get_Chain_Head = Current_Head,
            "valid log ahead of crash-stale checkpoint recovered");
      end;
   end;

   begin
      Export_Audit_Log ("/tmp/../tmp/hft_mifid_audit.log");
      Assert (False, "canonical source alias export rejected");
   exception
      when Constraint_Error =>
         Assert (True, "canonical source alias export rejected");
   end;

   Export_Audit_Log ("/tmp/hft_mifid_export.log");
   Configure_Durable_Log ("/tmp/hft_mifid_export.log");
   Assert
     (Get_Chain_Head /= (Hash_Text'(others => '0')),
      "export contains complete recoverable durable chain");
   Configure_Durable_Log ("/tmp/hft_mifid_audit.log");

   declare
      Truncated : File_Type;
   begin
      Create (Truncated, Out_File, "/tmp/hft_mifid_audit.log");
      Put_Line
        (Truncated,
         "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD");
      Close (Truncated);
      begin
         Initialize_Audit_System;
         Assert (False, "truncated durable history rejected");
      exception
         when Audit_Persistence_Error =>
            Assert (True, "truncated durable history rejected");
      end;
   end;

   Clear_Audit_History;
   declare
      Forged : File_Type;
   begin
      Create (Forged, Out_File, "/tmp/hft_mifid_tampered.log");
      Put_Line
        (Forged,
         "PREVIOUS_HASH|RECORD_HASH|CANONICAL_MIFID_AUDIT_RECORD");
      Put_Line (Forged, "forged");
      Close (Forged);
      begin
         Configure_Durable_Log ("/tmp/hft_mifid_tampered.log");
         Assert (False, "forged durable history rejected");
      exception
         when Audit_Persistence_Error =>
            Assert (True, "forged durable history rejected");
      end;
   end;

   declare
      Orphaned_Checkpoint : File_Type;
   begin
      Create
        (Orphaned_Checkpoint, Out_File,
         "/tmp/hft_mifid_missing.log.checkpoint");
      Put_Line
        (Orphaned_Checkpoint,
         (Hash_Text'(others => 'A')) & "|2");
      Close (Orphaned_Checkpoint);
      begin
         Configure_Durable_Log ("/tmp/hft_mifid_missing.log");
         Assert (False, "missing log with checkpoint rejected");
      exception
         when Audit_Persistence_Error =>
            Assert (True, "missing log with checkpoint rejected");
      end;
   end;
   Put_Line
     ("Results:" & Natural'Image (Passed) & "/" & Natural'Image (Tests));
   if Passed /= Tests then
      raise Program_Error with "MiFID audit tests failed";
   end if;
end HFT_MiFID_Test;
