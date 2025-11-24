-- Time Utility Package
-- Provides Unix timestamp functionality for the HFT system
pragma Ada_2022;

with Ada.Calendar;

package HFT_Time_Util is
   
   -- Get current Unix timestamp (seconds since epoch)
   function Get_Unix_Timestamp return Long_Integer;
   
end HFT_Time_Util;
