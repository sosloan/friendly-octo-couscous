-- MiFID II best-execution and MiFIR RTS 25 evidence model.
-- The predicates validate evidence completeness; they are not legal certification.
pragma Ada_2022;

with HFT_Engine;

package HFT_MiFID is
   subtype Short_Text is String (1 .. 32);
   subtype Long_Text is String (1 .. 160);
   subtype Currency_Code is String (1 .. 3);
   subtype Instrument_Code is String (1 .. 24);

   function To_Short_Text (Value : String) return Short_Text;
   function To_Long_Text (Value : String) return Long_Text;
   function To_Currency (Value : String) return Currency_Code;
   function To_Instrument (Value : String) return Instrument_Code;
   function Has_Content (Value : String) return Boolean;

   type Venue is (NYSE, CME, Other_Venue);
   type Instrument_Class is (Cash_Equity, Futures, Other_Instrument);
   type Trading_Session is
     (NYSE_Continuous, NYSE_Auction, CME_Globex, Other_Session);
   type Execution_Order_Type is
     (Market, Limit, Stop, Pegged, Auction, Spread, Other_Order_Type);
   type Lifecycle_Stage is
     (Market_Data_Receipt, Strategy_Decision, Risk_Approval, Gateway_Send,
      Venue_Acknowledgement, Partial_Fill, Full_Fill, Cancellation, Correction);

   type RTS25_Tier is
     (High_Frequency_Electronic, Standard_Electronic, Non_Electronic);
   type Synchronization_State is
     (Synchronized, Degraded, Unsynchronized);
   type Clock_Source is
     (PTP_Primary, PTP_Secondary, NTP, Holdover, Unknown_Source);
   type Timestamp_Origin is
     (Hardware, Kernel, Application, Exchange_Native);

   subtype Nonnegative_NS is
      Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   subtype Signed_NS is
      Long_Long_Integer range -9_000_000_000_000_000_000 ..
                               9_000_000_000_000_000_000;
   subtype Basis_Points is Integer range -1_000_000 .. 1_000_000;
   subtype Probability_BPS is Natural range 0 .. 10_000;

   function Maximum_UTC_Divergence_NS
     (Tier : RTS25_Tier) return Nonnegative_NS;
   function Maximum_Timestamp_Granularity_NS
     (Tier : RTS25_Tier) return Nonnegative_NS;

   type Clock_Evidence is record
      UTC_Time             : HFT_Engine.UTC_Timestamp_NS := 0;
      Monotonic_Time       : HFT_Engine.Monotonic_Timestamp_NS := 0;
      Tier                 : RTS25_Tier := High_Frequency_Electronic;
      State                : Synchronization_State := Unsynchronized;
      Source               : Clock_Source := Unknown_Source;
      Origin               : Timestamp_Origin := Application;
      UTC_Offset_NS        : Signed_NS := 0;
      Uncertainty_NS       : Nonnegative_NS := 0;
      Granularity_NS       : Nonnegative_NS := 1_000_000_000;
      Last_Synchronized_At : HFT_Engine.UTC_Timestamp_NS := 0;
   end record;

   function Is_Clock_Compliant (Evidence : Clock_Evidence) return Boolean;

   type Factor_Weights is record
      Price       : Natural range 0 .. 10_000 := 4_000;
      Cost        : Natural range 0 .. 10_000 := 2_000;
      Speed       : Natural range 0 .. 10_000 := 1_500;
      Fill        : Natural range 0 .. 10_000 := 1_500;
      Size_Nature : Natural range 0 .. 10_000 := 1_000;
   end record;

   type Venue_Policy is record
      NYSE_Approved : Boolean := True;
      CME_Approved  : Boolean := True;
      Other_Approved : Boolean := False;
      Version       : Short_Text := (others => ' ');
      Weights       : Factor_Weights;
   end record;

   function Is_Policy_Valid (Policy : Venue_Policy) return Boolean;
   function Is_Venue_Approved
     (Policy : Venue_Policy; Selected_Venue : Venue) return Boolean;

   type Market_Context is record
      Best_Bid                : HFT_Engine.Price := 0.0;
      Best_Ask                : HFT_Engine.Price := 0.0;
      Bid_Depth               : HFT_Engine.Quantity := 0;
      Ask_Depth               : HFT_Engine.Quantity := 0;
      Estimated_Fee_Micros    : Long_Long_Integer := 0;
      Estimated_Latency_NS    : Nonnegative_NS := 0;
      Expected_Fill_BPS       : Probability_BPS := 0;
      Liquidity_Score_BPS     : Probability_BPS := 0;
      Exchange_Timestamp      : HFT_Engine.UTC_Timestamp_NS := 0;
      Local_Receipt_Timestamp : HFT_Engine.UTC_Timestamp_NS := 0;
      Feed_Sequence           : Long_Long_Integer := 0;
      Is_Stale                : Boolean := True;
   end record;

   type Execution_Metrics is record
      Arrival_Price                  : HFT_Engine.Price := 0.0;
      Execution_Price                : HFT_Engine.Price := 0.0;
      Benchmark_Price                : HFT_Engine.Price := 0.0;
      Ordered_Quantity               : HFT_Engine.Quantity := 0;
      Filled_Quantity                : HFT_Engine.Quantity := 0;
      Fees_Micros                    : Long_Long_Integer := 0;
      Implementation_Shortfall_BPS   : Basis_Points := 0;
      Spread_Capture_BPS             : Basis_Points := 0;
      Slippage_BPS                   : Basis_Points := 0;
      Market_Impact_BPS              : Basis_Points := 0;
      Opportunity_Cost_BPS           : Basis_Points := 0;
      Execution_Latency_NS           : Nonnegative_NS := 0;
   end record;

   type Reconciliation_Evidence is record
      Exchange_Order_ID : Short_Text := (others => ' ');
      Drop_Copy_ID      : Short_Text := (others => ' ');
      Clearing_ID       : Short_Text := (others => ' ');
      Quantity_Matches  : Boolean := False;
      Price_Matches     : Boolean := False;
      Timestamp_Delta_NS : Signed_NS := 0;
   end record;

   type Execution_Evidence is record
      Parent_Order_ID      : Natural := 0;
      Child_Order_ID       : Natural := 0;
      Correlation_ID       : Natural := 0;
      Stage                : Lifecycle_Stage := Market_Data_Receipt;
      Selected_Venue       : Venue := Other_Venue;
      Asset_Class          : Instrument_Class := Other_Instrument;
      Instrument           : Instrument_Code := (others => ' ');
      Related_Instrument   : Instrument_Code := (others => ' ');
      Currency             : Currency_Code := "USD";
      Futures_Expiry       : Short_Text := (others => ' ');
      Session              : Trading_Session := Other_Session;
      Order_Type           : Execution_Order_Type := Other_Order_Type;
      Signal_ID            : Short_Text := (others => ' ');
      Client_Mandate       : Long_Text := (others => ' ');
      Strategy_Constraints : Long_Text := (others => ' ');
      Hedge_Objective      : Long_Text := (others => ' ');
      Roll_Decision        : Long_Text := (others => ' ');
      Routing_Rationale    : Long_Text := (others => ' ');
      Override_Identity    : Short_Text := (others => ' ');
      Override_Reason      : Long_Text := (others => ' ');
      Policy               : Venue_Policy;
      Build_ID             : Short_Text := (others => ' ');
      Market               : Market_Context;
      Metrics              : Execution_Metrics;
      Clock                : Clock_Evidence;
      Reconciliation       : Reconciliation_Evidence;
   end record;

   function Economically_Equivalent
     (Left, Right : Execution_Evidence) return Boolean;
   function Is_Reconciled
     (Evidence : Reconciliation_Evidence) return Boolean;
   function Is_Best_Execution_Evidence_Complete
     (Evidence : Execution_Evidence) return Boolean;
   function Is_Audit_Ready
     (Evidence : Execution_Evidence) return Boolean;
end HFT_MiFID;
