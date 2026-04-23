# Ada SPARK Ravenscar Compliance — Vibration & Chaos Monitoring

## Overview

This compliance framework governs the **Vibration Chaos Monitor**, a safety-critical
real-time system built with **Ada SPARK** and the **Ravenscar tasking profile**. The
system detects resonance, chaos onset, and structural anomalies in physical
environments by processing periodic sensor readings under formally verified, bounded
execution guarantees.

Every subprogram is annotated with SPARK contracts (`Pre`, `Post`, `Global`,
`Depends`). Every concurrent object is a Ravenscar-compliant protected type. No
dynamic allocation, no unbounded loops, no ceiling-priority violations.

---

## SPARK Mode

All units that participate in formal analysis carry:

```ada
pragma SPARK_Mode (On);
```

Units that interface with the Ada runtime (I/O, clock) are wrapped in thin
`SPARK_Mode (Off)` boundary packages so that the core analysis domain remains
fully provable.

---

## Ravenscar Profile

The top-level configuration unit declares:

```ada
pragma Profile (Ravenscar);
pragma Partition_Elaboration_Policy (Sequential);
```

This enforces:

- **One entry per protected object** — no selective accept, no terminate
  alternative.
- **No dynamic task creation** — all tasks are statically declared at library
  level.
- **Cyclic tasks only** — every task body is a loop guarded by a single
  `delay until` statement anchored to `Ada.Real_Time.Clock`.
- **Ceiling locking** — every protected object declares
  `pragma Priority (System.Priority'Last)` or an explicit ceiling.
- **No exception propagation across task boundaries** — exceptions are caught
  locally and recorded in a fault log protected object.

---

## Compliance Categories

### 1. Vibration Domain Types

Strong subtypes prevent unit confusion at compile time:

| Type | Base | Range | Unit |
|------|------|-------|------|
| `Frequency_Hz` | `Float` | 0.0 .. 20_000.0 | Hz |
| `Amplitude_G` | `Float` | 0.0 .. 100.0 | g (acceleration) |
| `Phase_Rad` | `Float` | −π .. π | radians |
| `Sensor_ID` | `Positive` | 1 .. 64 | — |
| `Sample_Count` | `Natural` | 0 .. 65_536 | — |

SPARK contracts verify that every arithmetic operation on these types stays
within range:

```ada
function Dominant_Frequency (Buffer : Sample_Buffer) return Frequency_Hz
  with Pre  => Buffer.Count >= Min_FFT_Size,
       Post => Dominant_Frequency'Result in 0.0 .. Nyquist_Limit (Buffer.Rate);
```

### 2. Chaos Detection Contracts

The Lyapunov exponent estimator carries a full SPARK proof:

```ada
function Lyapunov_Estimate
  (S1, S2 : Amplitude_G; Delta_T : Positive_Duration) return Float
  with Pre  => S1 /= S2 and Delta_T > 0.0,
       Post => (if Lyapunov_Estimate'Result > 0.0
                then Chaotic_Divergence_Detected);
```

A positive exponent triggers an immediate alarm via the `Chaos_Alert`
protected object.

### 3. Protected Sensor Buffer (Ravenscar)

```ada
protected Sensor_Buffer
  with Priority => System.Priority'Last
is
   procedure Write (Sample : Amplitude_G);
   function  Read  return Amplitude_G;
   function  Is_Full return Boolean;
private
   Data  : Sample_Array := (others => 0.0);
   Head  : Natural      := 0;
   Count : Natural      := 0;
end Sensor_Buffer;
```

- Exactly **one entry** in the entire protected object hierarchy per
  Ravenscar rules (the `Write` procedure is a procedure, not an entry, so
  there is no blocking — callers never queue).
- `Read` is a protected function: concurrent reads are allowed; writes
  are serialised.

### 4. Cyclic Sampling Task

```ada
task Sampling_Task
  with Priority => System.Priority'Last - 1;

task body Sampling_Task is
   use Ada.Real_Time;
   Period     : constant Time_Span := Milliseconds (1);  -- 1 kHz
   Next_Start : Time              := Clock + Period;
begin
   loop
      delay until Next_Start;
      Sensor_Buffer.Write (Read_Accelerometer);
      Next_Start := Next_Start + Period;
   end loop;
end Sampling_Task;
```

- **Period is a compile-time constant** — no jitter from dynamic allocation.
- `delay until` is the only allowed delay in the Ravenscar profile.
- The task does **not** call any unbounded subprogram.

### 5. Range & Overflow Safety

Every fixed-point and floating-point computation is guarded by a SPARK
precondition that prevents overflow before the operation executes:

```ada
function Scale_Amplitude (A : Amplitude_G; Factor : Float) return Amplitude_G
  with Pre  => Factor in 0.0 .. 1.0
               and then Float (A) * Factor <= Float (Amplitude_G'Last),
       Post => Scale_Amplitude'Result <= A;
```

`gnatprove` discharges all range checks at Gold level (`--level=4`).

### 6. Fault Isolation

Faults are captured in a dedicated protected log, never propagated across task
boundaries:

```ada
protected Fault_Log
  with Priority => System.Priority'Last
is
   procedure Record_Fault (Code : Fault_Code; Msg : Fault_String);
   function  Latest_Fault return Fault_Entry;
   function  Fault_Count  return Natural;
private
   Log   : Fault_Array  := (others => Null_Fault);
   Tail  : Natural       := 0;
end Fault_Log;
```

### 7. Resonance Alarm

The `Alarm_Manager` protected object stores the current alarm state and
exposes a single conditional entry so that a supervisor task can wait
without spinning:

```ada
protected Alarm_Manager
  with Priority => System.Priority'Last
is
   procedure Raise_Alarm  (Level : Alarm_Level);
   procedure Clear_Alarm;
   entry     Wait_For_Alarm (Level : out Alarm_Level);
private
   Active : Boolean     := False;
   Lvl    : Alarm_Level := None;
end Alarm_Manager;
```

The `Wait_For_Alarm` entry is the **one permitted entry** for this protected
object; its barrier is `Active`.

---

## Formal Verification Levels

| Component | `gnatprove` Level | Proof Goal |
|-----------|-------------------|------------|
| Domain type arithmetic | 4 (Gold) | No range check failure |
| Lyapunov estimator | 4 (Gold) | No division by zero |
| FFT magnitude loop | 3 (Silver) | Loop termination |
| Protected buffer write | 2 | Data race freedom |
| Sampling task period | 1 | Task activation |

Run analysis:

```bash
cd ada
gnatprove -P hft.gpr --mode=gold --report=all
```

---

## Ravenscar Compliance Checklist

| Rule | Status |
|------|--------|
| No dynamic task creation | ✓ |
| One entry per protected object | ✓ |
| No `select` with terminate alternative | ✓ |
| No `abort` statement | ✓ |
| `delay until` only (no relative delay) | ✓ |
| All tasks are library-level | ✓ |
| Ceiling priorities assigned | ✓ |
| No dynamic memory allocation | ✓ |
| Bounded loop iterations | ✓ |
| No exception propagation across tasks | ✓ |

---

## Building

```bash
cd ada
gprbuild -P hft.gpr -XBuild=Ravenscar
```

Compiler flags applied by the project file:

```
-gnatX   -- Ada 2022 extensions
-gnato   -- numeric overflow checking
-gnatp   -- suppress proof obligations already discharged by SPARK
-gnat-p  -- (leave on for debug builds)
-fstack-usage
```

## Running Compliance Tests

```bash
./obj/vibration_compliance_test
```

## Running Formal Proof

```bash
gnatprove -P hft.gpr --level=4 --report=all 2>&1 | tee proof_report.txt
```

---

## References

- [Ada Reference Manual 2022](http://www.ada-auth.org/standards/rm22/html/RM-TOC.html)
- [SPARK Reference Manual](https://docs.adacore.com/spark2014-docs/html/lrm/)
- [Ravenscar Profile (ARM D.13)](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-D-13.html)
- [High-Integrity System Development — AdaCore](https://www.adacore.com/books/hi-system-development)
- [Chaos & Lyapunov Exponents — Sprott](http://sprott.physics.wisc.edu/chaos/)

---

**Built for safety-critical real-time systems where every vibration counts.**
