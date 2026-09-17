-- Time Utility Package Implementation
pragma Ada_2022;

with Ada.Calendar;
with Ada.Real_Time;

package body HFT_Time_Util is

   Unix_Epoch : constant Ada.Calendar.Time :=
      Ada.Calendar.Time_Of (1970, 1, 1, 0.0);
   Monotonic_Epoch : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   function Duration_To_NS (Value : Duration) return Long_Long_Integer is
      Whole_Seconds : constant Long_Long_Integer :=
        Long_Long_Integer (Value);
      Fraction : constant Duration :=
        Value - Duration (Whole_Seconds);
   begin
      return Whole_Seconds * 1_000_000_000
        + Long_Long_Integer (Fraction * 1_000_000_000);
   end Duration_To_NS;

   function Get_Unix_Timestamp return Long_Integer is
      use Ada.Calendar;
      Diff : constant Duration := Clock - Unix_Epoch;
   begin
      return Long_Integer (Diff);
   end Get_Unix_Timestamp;

   function Get_UTC_Timestamp_NS return HFT_Engine.UTC_Timestamp_NS is
      use Ada.Calendar;
      Diff : constant Duration := Clock - Unix_Epoch;
   begin
      return HFT_Engine.UTC_Timestamp_NS
       (Duration_To_NS (Diff));
   end Get_UTC_Timestamp_NS;

   function Get_Monotonic_Timestamp_NS
      return HFT_Engine.Monotonic_Timestamp_NS
   is
      use Ada.Real_Time;
      Diff : constant Duration := To_Duration (Clock - Monotonic_Epoch);
   begin
      return HFT_Engine.Monotonic_Timestamp_NS
       (Long_Long_Integer'Max (1, Duration_To_NS (Diff)));
   end Get_Monotonic_Timestamp_NS;

end HFT_Time_Util;
