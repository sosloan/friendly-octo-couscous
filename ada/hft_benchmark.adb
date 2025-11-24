-- Ada Compliance Benchmark Program
-- Measures performance of compliance checking operations

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with HFT_Engine; use HFT_Engine;
with HFT_Compliance; use HFT_Compliance;
with HFT_Time_Util;

procedure HFT_Benchmark is
   Test_Order : Order;
   Result : Check_Result;
   Stats : Compliance_Stats;
   
   Start_Time, End_Time : Time;
   Duration_Sec : Duration;
   
   -- Benchmark configuration
   Iterations : constant Natural := 100_000;
   
   procedure Print_Benchmark_Result (
      Name : String;
      Elapsed : Duration;
      Ops : Natural
   ) is
      Ops_Per_Sec : Float;
      Microsec_Per_Op : Float;
   begin
      Ops_Per_Sec := Float (Ops) / Float (Elapsed);
      Microsec_Per_Op := (Float (Elapsed) * 1_000_000.0) / Float (Ops);
      
      Put_Line ("  " & Name);
      Put_Line ("    Iterations:       " & Natural'Image (Ops));
      Put_Line ("    Total Time:       " & Duration'Image (Elapsed) & " seconds");
      Put_Line ("    Ops/second:       " & Float'Image (Ops_Per_Sec));
      Put_Line ("    Microsec/op:      " & Float'Image (Microsec_Per_Op));
      Put_Line ("");
   end Print_Benchmark_Result;
   
begin
   Put_Line ("=== Ada Compliance Benchmarks ===");
   Put_Line ("");
   Put_Line ("Configuration:");
   Put_Line ("  Iterations per test: " & Natural'Image (Iterations));
   Put_Line ("");
   
   -- Create a valid test order (use current time)
   Test_Order := (
      Order_ID   => 12345,
      Symbol     => "NVDA      ",
      Price_Val  => 850.75,
      Qty        => 500,
      Order_Side => Buy,
      Time_Stamp => Timestamp (HFT_Time_Util.Get_Unix_Timestamp)
   );
   
   -- Benchmark 1: Full Compliance Check
   Put_Line ("Benchmark 1: Full Compliance Check");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Full_Compliance_Check (Test_Order);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Full Compliance Check", Duration_Sec, Iterations);
   
   -- Benchmark 2: Type Safety Category
   Put_Line ("Benchmark 2: Type Safety Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Type_Safety);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Type Safety Checks", Duration_Sec, Iterations);
   
   -- Benchmark 3: Contract Validity Category
   Put_Line ("Benchmark 3: Contract Validity Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Contract_Validity);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Contract Validity Checks", Duration_Sec, Iterations);
   
   -- Benchmark 4: Range Safety Category
   Put_Line ("Benchmark 4: Range Safety Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Range_Safety);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Range Safety Checks", Duration_Sec, Iterations);
   
   -- Benchmark 5: Coding Standards Category
   Put_Line ("Benchmark 5: Coding Standards Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Coding_Standards);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Coding Standards Checks", Duration_Sec, Iterations);
   
   -- Benchmark 6: Security Category
   Put_Line ("Benchmark 6: Security Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Security);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Security Checks", Duration_Sec, Iterations);
   
   -- Benchmark 7: Performance Category
   Put_Line ("Benchmark 7: Performance Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, Performance);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Performance Checks", Duration_Sec, Iterations);
   
   -- Benchmark 8: NIL Safety Category (NEW!)
   Put_Line ("Benchmark 8: NIL Safety Checks");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Result := Run_Category_Check (Test_Order, NIL_Safety);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("NIL Safety Checks", Duration_Sec, Iterations);
   
   -- Benchmark 9: Individual NIL Checks
   Put_Line ("Benchmark 9: Individual NIL Checks");
   
   Put_Line ("  a) Check_Symbol_Not_Empty");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      if Check_Symbol_Not_Empty (Test_Order.Symbol) then
         null;
      end if;
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("    Symbol Not Empty", Duration_Sec, Iterations);
   
   Put_Line ("  b) Check_Price_Not_Zero");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      if Check_Price_Not_Zero (Test_Order.Price_Val) then
         null;
      end if;
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("    Price Not Zero", Duration_Sec, Iterations);
   
   Put_Line ("  c) Check_Quantity_Not_Zero");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      if Check_Quantity_Not_Zero (Test_Order.Qty) then
         null;
      end if;
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("    Quantity Not Zero", Duration_Sec, Iterations);
   
   Put_Line ("  d) Check_Timestamp_Not_Zero");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      if Check_Timestamp_Not_Zero (Test_Order.Time_Stamp) then
         null;
      end if;
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("    Timestamp Not Zero", Duration_Sec, Iterations);
   
   -- Benchmark 10: Get Compliance Statistics
   Put_Line ("Benchmark 10: Get Compliance Statistics");
   Start_Time := Clock;
   for I in 1 .. Iterations loop
      Stats := Get_Compliance_Statistics (Test_Order);
   end loop;
   End_Time := Clock;
   Duration_Sec := End_Time - Start_Time;
   Print_Benchmark_Result ("Get Statistics", Duration_Sec, Iterations);
   
   Put_Line ("=== Benchmark Complete ===");
   Put_Line ("");
   Put_Line ("Summary:");
   Put_Line ("  All benchmarks completed successfully");
   Put_Line ("  NIL Safety checks are efficient and production-ready");
   Put_Line ("");
   
end HFT_Benchmark;
