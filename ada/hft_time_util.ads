-- Time Utility Package
-- Provides Unix timestamp functionality for the HFT system
pragma Ada_2022;

with HFT_Engine;

package HFT_Time_Util is
   
   -- Legacy seconds-resolution API.  Regulatory records must use
   -- Get_UTC_Timestamp_NS instead.
   function Get_Unix_Timestamp return Long_Integer;

   function Get_UTC_Timestamp_NS return HFT_Engine.UTC_Timestamp_NS;

   function Get_Monotonic_Timestamp_NS
      return HFT_Engine.Monotonic_Timestamp_NS;
   
end HFT_Time_Util;
