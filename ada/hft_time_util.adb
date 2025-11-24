-- Time Utility Package Implementation
pragma Ada_2022;

with Ada.Calendar;

package body HFT_Time_Util is
   
   -- Unix epoch: January 1, 1970 00:00:00 UTC
   -- Ada epoch: January 1, 1901 00:00:00 UTC (or implementation dependent)
   
   function Get_Unix_Timestamp return Long_Integer is
      use Ada.Calendar;
      
      -- Unix epoch as Ada time
      Unix_Epoch_Year : constant Year_Number := 1970;
      Unix_Epoch_Month : constant Month_Number := 1;
      Unix_Epoch_Day : constant Day_Number := 1;
      Unix_Epoch : constant Time := Time_Of (Unix_Epoch_Year, Unix_Epoch_Month, Unix_Epoch_Day);
      
      Current : constant Time := Clock;
      Diff : Duration;
   begin
      Diff := Current - Unix_Epoch;
      return Long_Integer (Diff);
   end Get_Unix_Timestamp;
   
end HFT_Time_Util;
