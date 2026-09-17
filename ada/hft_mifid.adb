pragma Ada_2022;

package body HFT_MiFID is
   use type HFT_Engine.Price;
   use type HFT_Engine.Quantity;
   use type HFT_Engine.UTC_Timestamp_NS;
   use type HFT_Engine.Monotonic_Timestamp_NS;
   function Copy_Short (Value : String; Length : Positive) return String is
      Result : String (1 .. Length) := (others => ' ');
      Count  : constant Natural := Natural'Min (Value'Length, Length);
   begin
      if Count > 0 then
         Result (1 .. Count) :=
           Value (Value'First .. Value'First + Count - 1);
      end if;
      return Result;
   end Copy_Short;

   function To_Short_Text (Value : String) return Short_Text is
   begin
      return Copy_Short (Value, Short_Text'Length);
   end To_Short_Text;

   function To_Long_Text (Value : String) return Long_Text is
   begin
      return Copy_Short (Value, Long_Text'Length);
   end To_Long_Text;

   function To_Currency (Value : String) return Currency_Code is
   begin
      return Copy_Short (Value, Currency_Code'Length);
   end To_Currency;

   function To_Instrument (Value : String) return Instrument_Code is
   begin
      return Copy_Short (Value, Instrument_Code'Length);
   end To_Instrument;

   function Has_Content (Value : String) return Boolean is
   begin
      for C of Value loop
         if C /= ' ' then
            return True;
         end if;
      end loop;
      return False;
   end Has_Content;

   function Maximum_UTC_Divergence_NS
     (Tier : RTS25_Tier) return Nonnegative_NS is
   begin
      case Tier is
         when High_Frequency_Electronic => return 100_000;
         when Standard_Electronic       => return 1_000_000;
         when Non_Electronic            => return 1_000_000_000;
      end case;
   end Maximum_UTC_Divergence_NS;

   function Maximum_Timestamp_Granularity_NS
     (Tier : RTS25_Tier) return Nonnegative_NS is
   begin
      case Tier is
         when High_Frequency_Electronic => return 1_000;
         when Standard_Electronic       => return 1_000_000;
         when Non_Electronic            => return 1_000_000_000;
      end case;
   end Maximum_Timestamp_Granularity_NS;

   function Is_Clock_Compliant (Evidence : Clock_Evidence) return Boolean is
      Absolute_Offset : Nonnegative_NS;
      Maximum_Offset  : constant Nonnegative_NS :=
        Maximum_UTC_Divergence_NS (Evidence.Tier);
   begin
      Absolute_Offset :=
        (if Evidence.UTC_Offset_NS < 0
         then -Evidence.UTC_Offset_NS
         else Evidence.UTC_Offset_NS);
      if Evidence.State /= In_Sync
        or else Evidence.Source = Unknown_Source
        or else Evidence.UTC_Time = 0
        or else Evidence.Monotonic_Time = 0
        or else Evidence.Last_Synchronized_At = 0
        or else Evidence.Last_Synchronized_At > Evidence.UTC_Time
        or else Absolute_Offset > Maximum_Offset
      then
         return False;
      end if;
      return Evidence.Uncertainty_NS <= Maximum_Offset - Absolute_Offset
        and then Evidence.Granularity_NS <=
          Maximum_Timestamp_Granularity_NS (Evidence.Tier)
        and then Long_Long_Integer
          (Evidence.UTC_Time - Evidence.Last_Synchronized_At) <=
            Maximum_Synchronization_Age_NS;
   end Is_Clock_Compliant;

   function Is_Policy_Valid (Policy : Venue_Policy) return Boolean is
      Total : constant Natural :=
        Policy.Weights.Price + Policy.Weights.Cost + Policy.Weights.Speed
        + Policy.Weights.Fill + Policy.Weights.Size_Nature;
   begin
      return Has_Content (Policy.Version)
        and Total = 10_000
        and (Policy.NYSE_Approved or Policy.CME_Approved
             or Policy.Other_Approved);
   end Is_Policy_Valid;

   function Is_Venue_Approved
     (Policy : Venue_Policy; Selected_Venue : Venue) return Boolean is
   begin
      case Selected_Venue is
         when NYSE        => return Policy.NYSE_Approved;
         when CME         => return Policy.CME_Approved;
         when Other_Venue => return Policy.Other_Approved;
      end case;
   end Is_Venue_Approved;

   function Economically_Equivalent
     (Left, Right : Execution_Evidence) return Boolean is
   begin
      return Left.Asset_Class = Right.Asset_Class
        and Left.Instrument = Right.Instrument
        and Left.Currency = Right.Currency
        and Left.Metrics.Ordered_Quantity = Right.Metrics.Ordered_Quantity;
   end Economically_Equivalent;

   function Is_Reconciled
     (Evidence : Reconciliation_Evidence) return Boolean is
      Absolute_Delta : constant Nonnegative_NS :=
        (if Evidence.Timestamp_Delta_NS < 0
         then -Evidence.Timestamp_Delta_NS
         else Evidence.Timestamp_Delta_NS);
   begin
      return Has_Content (Evidence.Exchange_Order_ID)
        and then Has_Content (Evidence.Drop_Copy_ID)
        and then Has_Content (Evidence.Clearing_ID)
        and then Evidence.Quantity_Matches
        and then Evidence.Price_Matches
        and then Absolute_Delta <= Maximum_Reconciliation_Delta_NS;
   end Is_Reconciled;

   function Requires_Reconciliation
     (Evidence : Execution_Evidence) return Boolean is
   begin
      return Evidence.Stage in Partial_Fill | Full_Fill | Correction
        or else Evidence.Metrics.Filled_Quantity > 0;
   end Requires_Reconciliation;

   function Is_Recording_Time_Valid
     (Evidence    : Execution_Evidence;
      Recorded_At : HFT_Engine.UTC_Timestamp_NS) return Boolean
   is
      Clock_Difference    : HFT_Engine.UTC_Timestamp_NS;
      Receipt_Difference  : HFT_Engine.UTC_Timestamp_NS;
      Exchange_Difference : HFT_Engine.UTC_Timestamp_NS;
   begin
      Clock_Difference :=
        (if Evidence.Clock.UTC_Time >= Recorded_At
         then Evidence.Clock.UTC_Time - Recorded_At
         else Recorded_At - Evidence.Clock.UTC_Time);
      Receipt_Difference :=
        (if Evidence.Market.Local_Receipt_Timestamp >= Recorded_At
         then Evidence.Market.Local_Receipt_Timestamp - Recorded_At
         else Recorded_At - Evidence.Market.Local_Receipt_Timestamp);
      Exchange_Difference :=
        (if Evidence.Market.Exchange_Timestamp >= Recorded_At
         then Evidence.Market.Exchange_Timestamp - Recorded_At
         else Recorded_At - Evidence.Market.Exchange_Timestamp);
      return Long_Long_Integer (Clock_Difference) <=
        Maximum_Market_Data_Age_NS
        and then Long_Long_Integer (Receipt_Difference) <=
          Maximum_Market_Data_Age_NS
        and then Long_Long_Integer (Exchange_Difference) <=
          Maximum_Market_Data_Age_NS;
   end Is_Recording_Time_Valid;

   function Is_Best_Execution_Evidence_Complete
     (Evidence : Execution_Evidence) return Boolean is
      Market_Age : Nonnegative_NS;
   begin
      if Evidence.Market.Local_Receipt_Timestamp <
        Evidence.Market.Exchange_Timestamp
      then
         return False;
      end if;
      Market_Age := Long_Long_Integer
        (Evidence.Market.Local_Receipt_Timestamp -
         Evidence.Market.Exchange_Timestamp);
      return Evidence.Parent_Order_ID > 0
        and then Evidence.Child_Order_ID > 0
        and then Evidence.Correlation_ID > 0
        and then Has_Content (Evidence.Instrument)
        and then Has_Content (Evidence.Signal_ID)
        and then Has_Content (Evidence.Client_Mandate)
        and then Has_Content (Evidence.Strategy_Constraints)
        and then Has_Content (Evidence.Routing_Rationale)
        and then Has_Content (Evidence.Build_ID)
        and then Is_Policy_Valid (Evidence.Policy)
        and then Is_Venue_Approved (Evidence.Policy, Evidence.Selected_Venue)
        and then Evidence.Metrics.Ordered_Quantity > 0
        and then Evidence.Metrics.Arrival_Price > 0.0
        and then Evidence.Metrics.Benchmark_Price > 0.0
        and then Evidence.Metrics.Filled_Quantity <=
          Evidence.Metrics.Ordered_Quantity
        and then
          (if Evidence.Stage in
             Market_Data_Receipt | Strategy_Decision |
             Risk_Approval | Gateway_Send
           then
             Evidence.Metrics.Filled_Quantity = 0
             and then Evidence.Metrics.Execution_Price = 0.0
             and then not Has_Content
               (Evidence.Reconciliation.Exchange_Order_ID)
             and then not Has_Content
               (Evidence.Reconciliation.Drop_Copy_ID)
             and then not Has_Content
               (Evidence.Reconciliation.Clearing_ID)
           elsif Evidence.Stage = Venue_Acknowledgement then
             Evidence.Metrics.Filled_Quantity = 0
             and then Evidence.Metrics.Execution_Price = 0.0
             and then Has_Content
               (Evidence.Reconciliation.Exchange_Order_ID)
             and then not Has_Content
               (Evidence.Reconciliation.Drop_Copy_ID)
             and then not Has_Content
               (Evidence.Reconciliation.Clearing_ID)
           elsif Evidence.Stage = Partial_Fill then
             Evidence.Metrics.Filled_Quantity > 0
             and then Evidence.Metrics.Filled_Quantity <
               Evidence.Metrics.Ordered_Quantity
             and then Evidence.Metrics.Execution_Price > 0.0
           elsif Evidence.Stage = Full_Fill then
             Evidence.Metrics.Filled_Quantity =
               Evidence.Metrics.Ordered_Quantity
             and then Evidence.Metrics.Execution_Price > 0.0
           elsif Evidence.Stage = Cancellation
             and then Evidence.Metrics.Filled_Quantity = 0
           then
             Evidence.Metrics.Execution_Price = 0.0
             and then Has_Content
               (Evidence.Reconciliation.Exchange_Order_ID)
           elsif Evidence.Metrics.Filled_Quantity > 0 then
             Evidence.Metrics.Execution_Price > 0.0
           else True)
        and then Evidence.Market.Best_Bid > 0.0
        and then Evidence.Market.Best_Ask >= Evidence.Market.Best_Bid
        and then Evidence.Market.Exchange_Timestamp > 0
        and then Market_Age <= Maximum_Market_Data_Age_NS
        and then Evidence.Clock.UTC_Time >=
          Evidence.Market.Local_Receipt_Timestamp
        and then Long_Long_Integer
          (Evidence.Clock.UTC_Time -
           Evidence.Market.Local_Receipt_Timestamp) <=
             Maximum_Market_Data_Age_NS
        and then Evidence.Market.Feed_Sequence > 0
        and then not Evidence.Market.Is_Stale
        and then
          (if Evidence.Asset_Class = Futures then
             Evidence.Selected_Venue = CME
             and then Evidence.Session = CME_Globex
             and then Has_Content (Evidence.Futures_Expiry)
             and then Evidence.Order_Type /= Auction
           elsif Evidence.Asset_Class = Cash_Equity then
             Evidence.Selected_Venue = NYSE
             and then Evidence.Session in
               NYSE_Continuous | NYSE_Auction
             and then
               ((Evidence.Session = NYSE_Auction
                  and then Evidence.Order_Type = Auction)
                or else
                (Evidence.Session = NYSE_Continuous
                  and then Evidence.Order_Type /= Auction))
           else True)
        and then
          (if Evidence.Order_Type = Spread then
             Evidence.Asset_Class = Futures
             and then Has_Content (Evidence.Related_Instrument)
             and then Has_Content (Evidence.Roll_Decision)
           else True)
        and then
          (if Has_Content (Evidence.Related_Instrument) then
             Has_Content (Evidence.Hedge_Objective)
           else True)
        and then
          (Has_Content (Evidence.Override_Identity) =
           Has_Content (Evidence.Override_Reason));
   end Is_Best_Execution_Evidence_Complete;

   function Is_Audit_Ready
     (Evidence : Execution_Evidence) return Boolean is
   begin
      return Is_Best_Execution_Evidence_Complete (Evidence)
        and then Is_Clock_Compliant (Evidence.Clock)
        and then
          (not Requires_Reconciliation (Evidence)
           or else Is_Reconciled (Evidence.Reconciliation));
   end Is_Audit_Ready;
end HFT_MiFID;
