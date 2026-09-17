pragma Ada_2022;

package body HFT_MiFID is
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
   begin
      Absolute_Offset :=
        (if Evidence.UTC_Offset_NS < 0
         then -Evidence.UTC_Offset_NS
         else Evidence.UTC_Offset_NS);
      return Evidence.State = Synchronized
        and Evidence.Source /= Unknown_Source
        and Evidence.UTC_Time > 0
        and Evidence.Monotonic_Time > 0
        and Evidence.Last_Synchronized_At > 0
        and Absolute_Offset <= Maximum_UTC_Divergence_NS (Evidence.Tier)
        and Evidence.Uncertainty_NS <=
          Maximum_UTC_Divergence_NS (Evidence.Tier) - Absolute_Offset
        and Evidence.Granularity_NS <=
          Maximum_Timestamp_Granularity_NS (Evidence.Tier);
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
   begin
      return Has_Content (Evidence.Exchange_Order_ID)
        and Has_Content (Evidence.Drop_Copy_ID)
        and Has_Content (Evidence.Clearing_ID)
        and Evidence.Quantity_Matches
        and Evidence.Price_Matches;
   end Is_Reconciled;

   function Is_Best_Execution_Evidence_Complete
     (Evidence : Execution_Evidence) return Boolean is
   begin
      return Evidence.Parent_Order_ID > 0
        and Evidence.Child_Order_ID > 0
        and Evidence.Correlation_ID > 0
        and Has_Content (Evidence.Instrument)
        and Has_Content (Evidence.Signal_ID)
        and Has_Content (Evidence.Client_Mandate)
        and Has_Content (Evidence.Strategy_Constraints)
        and Has_Content (Evidence.Routing_Rationale)
        and Has_Content (Evidence.Build_ID)
        and Is_Policy_Valid (Evidence.Policy)
        and Is_Venue_Approved (Evidence.Policy, Evidence.Selected_Venue)
        and Evidence.Metrics.Ordered_Quantity > 0
        and Evidence.Metrics.Filled_Quantity <=
          Evidence.Metrics.Ordered_Quantity
        and Evidence.Market.Best_Bid > 0.0
        and Evidence.Market.Best_Ask >= Evidence.Market.Best_Bid
        and Evidence.Market.Exchange_Timestamp > 0
        and Evidence.Market.Local_Receipt_Timestamp >=
          Evidence.Market.Exchange_Timestamp
        and Evidence.Market.Feed_Sequence > 0
        and not Evidence.Market.Is_Stale
        and (if Evidence.Asset_Class = Futures
             then Evidence.Selected_Venue = CME
               and Has_Content (Evidence.Futures_Expiry)
             else True)
        and (if Evidence.Asset_Class = Cash_Equity
             then Evidence.Selected_Venue = NYSE
             else True);
   end Is_Best_Execution_Evidence_Complete;

   function Is_Audit_Ready
     (Evidence : Execution_Evidence) return Boolean is
   begin
      return Is_Best_Execution_Evidence_Complete (Evidence)
        and Is_Clock_Compliant (Evidence.Clock)
        and Is_Reconciled (Evidence.Reconciliation);
   end Is_Audit_Ready;
end HFT_MiFID;
