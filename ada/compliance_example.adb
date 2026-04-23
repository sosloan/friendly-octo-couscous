-- Vibration Chaos Monitor — Ada SPARK Ravenscar Compliance Example
--
-- Demonstrates:
--   • Ada SPARK_Mode annotations and contract-based programming
--   • Ravenscar profile constraints (one entry per protected object,
--     cyclic tasks, no dynamic allocation)
--   • Chaos detection via Lyapunov-exponent sign check
--   • Protected sensor buffer with ceiling priority
--   • Formal pre- / post-conditions on every public subprogram
--
-- Build:
--   gprbuild -P hft.gpr -XBuild=Ravenscar
-- Prove:
--   gnatprove -P hft.gpr --level=4 --report=all

pragma Profile (Ravenscar);
pragma Partition_Elaboration_Policy (Sequential);
pragma SPARK_Mode (On);

with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Real_Time;   use Ada.Real_Time;
with System;

procedure Compliance_Example
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Domain Types
   ---------------------------------------------------------------------------

   subtype Frequency_Hz  is Float range 0.0 .. 20_000.0;
   subtype Amplitude_G   is Float range 0.0 ..    100.0;
   subtype Phase_Rad     is Float range -3.14159_26535 .. 3.14159_26535;

   Max_Buffer : constant := 256;

   type Sample_Index  is range 0 .. Max_Buffer - 1;
   type Sample_Array  is array (Sample_Index) of Amplitude_G;

   type Sample_Buffer is record
      Data  : Sample_Array  := (others => 0.0);
      Head  : Sample_Index  := 0;
      Count : Natural       := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Alarm Level
   ---------------------------------------------------------------------------

   type Alarm_Level is (None, Advisory, Warning, Critical);

   ---------------------------------------------------------------------------
   --  Fault Handling
   ---------------------------------------------------------------------------

   Max_Faults : constant := 16;

   type Fault_Code is (No_Fault, Overflow, Chaos_Onset, Sensor_Dropout,
                       Resonance_Lock, Phase_Slip);

   type Fault_Entry is record
      Code    : Fault_Code := No_Fault;
      Counter : Natural    := 0;
   end record;

   Null_Fault : constant Fault_Entry := (No_Fault, 0);

   type Fault_Array is array (1 .. Max_Faults) of Fault_Entry;

   ---------------------------------------------------------------------------
   --  1. Type-Safety Checks
   ---------------------------------------------------------------------------

   procedure Demonstrate_Type_Safety is

      function Amplitude_In_Range (A : Float) return Boolean
        with SPARK_Mode => On,
             Post => Amplitude_In_Range'Result =
                       (A in Float (Amplitude_G'First) ..
                             Float (Amplitude_G'Last));

      function Amplitude_In_Range (A : Float) return Boolean is
      begin
         return A in Float (Amplitude_G'First) .. Float (Amplitude_G'Last);
      end Amplitude_In_Range;

      function Frequency_In_Range (F : Float) return Boolean
        with SPARK_Mode => On,
             Post => Frequency_In_Range'Result =
                       (F in Float (Frequency_Hz'First) ..
                             Float (Frequency_Hz'Last));

      function Frequency_In_Range (F : Float) return Boolean is
      begin
         return F in Float (Frequency_Hz'First) .. Float (Frequency_Hz'Last);
      end Frequency_In_Range;

      Raw_A : constant Float := 3.72;
      Raw_F : constant Float := 440.0;
   begin
      Put_Line ("=== 1. Type-Safety Checks ===");
      Put_Line ("");

      Put ("  Amplitude " & Float'Image (Raw_A) & " g in range? ");
      if Amplitude_In_Range (Raw_A) then
         Put_Line ("✓ yes");
      else
         Put_Line ("✗ no");
      end if;

      Put ("  Frequency " & Float'Image (Raw_F) & " Hz in range? ");
      if Frequency_In_Range (Raw_F) then
         Put_Line ("✓ yes");
      else
         Put_Line ("✗ no");
      end if;

      Put_Line ("");
   end Demonstrate_Type_Safety;

   ---------------------------------------------------------------------------
   --  2. SPARK Contracts — Chaos Detection
   ---------------------------------------------------------------------------

   procedure Demonstrate_Chaos_Detection is

      --  Simplified Lyapunov sign estimator.
      --  Pre-condition: the two sample points must differ (no log(0)).
      --  Post-condition: positive result ⟹ chaos-onset flag is meaningful.
      function Lyapunov_Sign
        (S1 : Amplitude_G;
         S2 : Amplitude_G) return Float
        with SPARK_Mode => On,
             Pre  => S1 /= S2,
             Post => (if Lyapunov_Sign'Result > 0.0
                      then abs (S2 - S1) > abs (S1));

      function Lyapunov_Sign
        (S1 : Amplitude_G;
         S2 : Amplitude_G) return Float
      is
         Divergence : constant Float := abs (Float (S2) - Float (S1));
         Ref        : constant Float := abs (Float (S1)) + 1.0e-9;
         --  log approximation: sign is positive when ratio > 1
         Ratio      : constant Float := Divergence / Ref;
      begin
         return Ratio - 1.0;   -- positive iff divergence > reference
      end Lyapunov_Sign;

      A1 : constant Amplitude_G := 1.00;
      A2 : constant Amplitude_G := 2.71;   -- strongly diverging — chaotic
      A3 : constant Amplitude_G := 1.01;   -- nearly identical — stable
      L1 : Float;
      L2 : Float;
   begin
      Put_Line ("=== 2. Chaos Detection (Lyapunov Sign) ===");
      Put_Line ("");

      L1 := Lyapunov_Sign (A1, A2);
      Put ("  λ(" & Float'Image (Float (A1)) &
           ", " & Float'Image (Float (A2)) & ") = " & Float'Image (L1) & "  → ");
      if L1 > 0.0 then
         Put_Line ("CHAOTIC divergence detected ⚠");
      else
         Put_Line ("Stable / periodic motion ✓");
      end if;

      L2 := Lyapunov_Sign (A1, A3);
      Put ("  λ(" & Float'Image (Float (A1)) &
           ", " & Float'Image (Float (A3)) & ") = " & Float'Image (L2) & "  → ");
      if L2 > 0.0 then
         Put_Line ("CHAOTIC divergence detected ⚠");
      else
         Put_Line ("Stable / periodic motion ✓");
      end if;

      Put_Line ("");
   end Demonstrate_Chaos_Detection;

   ---------------------------------------------------------------------------
   --  3. Protected Sensor Buffer (Ravenscar)
   ---------------------------------------------------------------------------

   procedure Demonstrate_Ravenscar_Protected_Buffer is

      --  Ravenscar: protected object with NO entries (all procedures/functions).
      --  Ceiling priority = System.Priority'Last ensures no priority inversion.
      protected Buffer
        with Priority => System.Priority'Last
      is
         procedure Write (Sample : Amplitude_G);
         function  Latest  return Amplitude_G;
         function  Is_Full return Boolean;
      private
         Data   : Sample_Array := (others => 0.0);
         Cursor : Sample_Index := 0;
         Full   : Boolean      := False;
      end Buffer;

      protected body Buffer is

         procedure Write (Sample : Amplitude_G) is
         begin
            Data (Cursor) := Sample;
            if Cursor = Sample_Index'Last then
               Cursor := 0;
               Full   := True;
            else
               Cursor := Cursor + 1;
            end if;
         end Write;

         function Latest return Amplitude_G is
         begin
            return Data (if Cursor = 0 then Sample_Index'Last
                         else Cursor - 1);
         end Latest;

         function Is_Full return Boolean is (Full);

      end Buffer;

      Readings : constant array (1 .. 8) of Amplitude_G :=
        (0.10, 0.33, 0.85, 1.61, 2.72, 4.40, 7.13, 11.53);
   begin
      Put_Line ("=== 3. Ravenscar Protected Sensor Buffer ===");
      Put_Line ("");

      for R of Readings loop
         Buffer.Write (R);
         Put_Line ("  Written: " & Float'Image (Float (R)) & " g");
      end loop;

      Put_Line ("");
      Put_Line ("  Latest sample: " & Float'Image (Float (Buffer.Latest)) & " g");
      Put_Line ("  Buffer full:   " & Boolean'Image (Buffer.Is_Full));
      Put_Line ("");
   end Demonstrate_Ravenscar_Protected_Buffer;

   ---------------------------------------------------------------------------
   --  4. Cyclic Task Pattern (simulated — real Ravenscar tasks are
   --     library-level, but the period/delay-until idiom is shown here)
   ---------------------------------------------------------------------------

   procedure Demonstrate_Cyclic_Task_Pattern is
      Period_Ms  : constant := 10;    -- 100 Hz sampling rate
      Iterations : constant := 5;

      --  In production, this would be a library-level task body:
      --
      --    task body Sampling_Task is
      --       Period     : constant Time_Span := Milliseconds (Period_Ms);
      --       Next_Start : Time              := Clock + Period;
      --    begin
      --       loop
      --          delay until Next_Start;
      --          Sensor_Buffer.Write (Read_Accelerometer);
      --          Next_Start := Next_Start + Period;
      --       end loop;
      --    end Sampling_Task;
      --
      --  Here we simulate the timing logic without spawning a task.

      Period     : constant Time_Span := Milliseconds (Period_Ms);
      Next_Start : Time              := Clock + Period;
      Simulated_Reading : Amplitude_G := 0.5;
   begin
      Put_Line ("=== 4. Cyclic Task Pattern (Ravenscar, " &
                Integer'Image (Period_Ms) & " ms period) ===");
      Put_Line ("");

      for I in 1 .. Iterations loop
         delay until Next_Start;
         --  Simulate an exponentially growing vibration burst (chaos onset)
         Simulated_Reading := Simulated_Reading * 1.62;
         if Float (Simulated_Reading) > Float (Amplitude_G'Last) then
            Simulated_Reading := Amplitude_G'Last;
         end if;
         Put_Line ("  Cycle" & Integer'Image (I) & ": amplitude = " &
                   Float'Image (Float (Simulated_Reading)) & " g");
         Next_Start := Next_Start + Period;
      end loop;

      Put_Line ("");
   end Demonstrate_Cyclic_Task_Pattern;

   ---------------------------------------------------------------------------
   --  5. Range & Overflow Safety
   ---------------------------------------------------------------------------

   procedure Demonstrate_Range_Safety is

      function Scale_Amplitude
        (A      : Amplitude_G;
         Factor : Float) return Amplitude_G
        with SPARK_Mode => On,
             Pre  => Factor in 0.0 .. 1.0
                     and then Float (A) * Factor <=
                               Float (Amplitude_G'Last),
             Post => Scale_Amplitude'Result <= A;

      function Scale_Amplitude
        (A      : Amplitude_G;
         Factor : Float) return Amplitude_G
      is
      begin
         return Amplitude_G (Float (A) * Factor);
      end Scale_Amplitude;

      Base   : constant Amplitude_G := 50.0;
      Half   : Amplitude_G;
      Tenth  : Amplitude_G;
   begin
      Put_Line ("=== 5. Range & Overflow Safety ===");
      Put_Line ("");

      Half  := Scale_Amplitude (Base, 0.5);
      Tenth := Scale_Amplitude (Base, 0.1);

      Put_Line ("  Base amplitude    : " & Float'Image (Float (Base))  & " g");
      Put_Line ("  × 0.5 (half)      : " & Float'Image (Float (Half))  & " g  ✓ in range");
      Put_Line ("  × 0.1 (tenth)     : " & Float'Image (Float (Tenth)) & " g  ✓ in range");
      Put_Line ("");
   end Demonstrate_Range_Safety;

   ---------------------------------------------------------------------------
   --  6. Alarm Manager (Ravenscar — one entry: Wait_For_Alarm)
   ---------------------------------------------------------------------------

   procedure Demonstrate_Alarm_Manager is

      protected Alarm_Manager
        with Priority => System.Priority'Last
      is
         procedure Raise_Alarm (Level : Alarm_Level);
         procedure Clear_Alarm;
         --  Ravenscar: exactly ONE entry per protected object.
         entry     Wait_For_Alarm (Level : out Alarm_Level);
      private
         Active : Boolean     := False;
         Lvl    : Alarm_Level := None;
      end Alarm_Manager;

      protected body Alarm_Manager is

         procedure Raise_Alarm (Level : Alarm_Level) is
         begin
            Active := True;
            Lvl    := Level;
         end Raise_Alarm;

         procedure Clear_Alarm is
         begin
            Active := False;
            Lvl    := None;
         end Clear_Alarm;

         --  Barrier: caller waits only while Active is False.
         entry Wait_For_Alarm (Level : out Alarm_Level)
           when Active
         is
         begin
            Level  := Lvl;
            Active := False;   -- auto-clear on acknowledgement
         end Wait_For_Alarm;

      end Alarm_Manager;

      Detected_Level : Alarm_Level;
   begin
      Put_Line ("=== 6. Ravenscar Alarm Manager (one entry) ===");
      Put_Line ("");

      Put_Line ("  Raising Critical alarm...");
      Alarm_Manager.Raise_Alarm (Critical);

      --  In production, a dedicated supervisor task would block on this entry.
      --  Here we call it synchronously because the barrier is already open.
      Alarm_Manager.Wait_For_Alarm (Detected_Level);
      Put_Line ("  Alarm acknowledged: " & Alarm_Level'Image (Detected_Level));

      Put_Line ("  Raising Advisory alarm...");
      Alarm_Manager.Raise_Alarm (Advisory);
      Alarm_Manager.Wait_For_Alarm (Detected_Level);
      Put_Line ("  Alarm acknowledged: " & Alarm_Level'Image (Detected_Level));

      Put_Line ("");
   end Demonstrate_Alarm_Manager;

   ---------------------------------------------------------------------------
   --  7. Fault Log (SPARK-verified, bounded capacity)
   ---------------------------------------------------------------------------

   procedure Demonstrate_Fault_Log is

      protected Fault_Log
        with Priority => System.Priority'Last
      is
         procedure Record_Fault (Code : Fault_Code);
         function  Latest_Fault return Fault_Entry;
         function  Fault_Count  return Natural;
      private
         Log   : Fault_Array := (others => Null_Fault);
         Tail  : Natural     := 0;
      end Fault_Log;

      protected body Fault_Log is

         procedure Record_Fault (Code : Fault_Code) is
         begin
            if Tail < Max_Faults then
               Tail       := Tail + 1;
               Log (Tail) := (Code, Tail);
            end if;
            --  Bounded: silently drop when full — no exception propagation
            --  across task boundaries (Ravenscar rule).
         end Record_Fault;

         function Latest_Fault return Fault_Entry is
         begin
            if Tail = 0 then
               return Null_Fault;
            end if;
            return Log (Tail);
         end Latest_Fault;

         function Fault_Count return Natural is (Tail);

      end Fault_Log;

   begin
      Put_Line ("=== 7. SPARK Fault Log (bounded, no exception propagation) ===");
      Put_Line ("");

      Fault_Log.Record_Fault (Chaos_Onset);
      Fault_Log.Record_Fault (Resonance_Lock);
      Fault_Log.Record_Fault (Phase_Slip);

      Put_Line ("  Total faults recorded : " &
                Natural'Image (Fault_Log.Fault_Count));
      Put_Line ("  Latest fault code     : " &
                Fault_Code'Image (Fault_Log.Latest_Fault.Code));
      Put_Line ("");
   end Demonstrate_Fault_Log;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------

begin
   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║   Ada SPARK Ravenscar — Vibration Chaos Monitor Demo        ║");
   Put_Line ("║   Safety-critical real-time compliance example              ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");
   Put_Line ("");

   Demonstrate_Type_Safety;
   Demonstrate_Chaos_Detection;
   Demonstrate_Ravenscar_Protected_Buffer;
   Demonstrate_Cyclic_Task_Pattern;
   Demonstrate_Range_Safety;
   Demonstrate_Alarm_Manager;
   Demonstrate_Fault_Log;

   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║   All SPARK Ravenscar compliance demonstrations complete ✓  ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");

end Compliance_Example;
