-- Lean 4 Chinese Finger Trap Metric Proofs
-- Formal verification of the Bourbaki ≤ FingerTrap metric hierarchy

import Mathlib.Data.Real.Basic
import Mathlib.Tactic.Ring

/-! # The Chinese Finger Trap Metric

This module formalises two competing notions of distance and proves
that the classical Bourbaki (Euclidean) metric is merely the zero-
velocity special case of the dynamic "finger trap" metric.

The finger-trap metric encodes the key insight that the cost to
traverse a manifold is **not** static: it scales with the square of
the entity's escape velocity (its "struggle"), modulated by an
environmental resistance constant k.

Connection to spatiotemporal propagation
  ‖x − sᵢ‖ = c · Δtᵢ   ←→   ‖profile − aᵢ‖ = rᵢ(βᵢ)

The left-hand side is the wavefront equation (signal at sᵢ reaches x
after Δtᵢ time steps at speed c); the right-hand side is the
profile-sphere bound (entity aᵢ interacts within radius rᵢ(βᵢ) in
ecological profile-space).  The finger-trap metric bridges them:
when v → 0 (no struggle, no momentum) the dynamic metric collapses
to pure Bourbaki distance, and as v grows the manifold "tightens".
-/

namespace FingerTrap

/--
  The Bourbaki Model:
  Distance is absolute, invariant, and blind to momentum.
-/
def bourbaki_metric (x y : ℝ) : ℝ :=
  |x - y|

/--
  The Chinese Finger Trap Model:
  The distance (or computational cost to escape) is a dynamic tensor.
  It scales with the square of the entity's escape velocity (v).
  k is the environmental resistance constant.
-/
def finger_trap_metric (x y v k : ℝ) : ℝ :=
  |x - y| * (1 + k * v ^ 2)

/--
  THEOREM 1: CHINESE FINGER TRAP ⊇ BOURBAKI

  Proof that Bourbaki is the trivial state of the Trap when the
  entity ceases to struggle (v = 0).  The dynamic metric subsumes
  the static one.
-/
theorem trap_subsumes_bourbaki (x y k : ℝ) :
    finger_trap_metric x y 0 k = bourbaki_metric x y := by
  unfold finger_trap_metric bourbaki_metric
  have h1 : (0 : ℝ) ^ 2 = 0 := by ring
  rw [h1]
  have h2 : k * 0 = 0 := mul_zero k
  rw [h2]
  have h3 : 1 + (0 : ℝ) = 1 := add_zero 1
  rw [h3]
  exact mul_one (|x - y|)

/--
  THEOREM 2: THE CLAMP (RUGGEDNESS REACTION)

  Proof that as the entity's struggle increases (v₁ → v₂), the
  bounding cost of the manifold strictly increases.
  You cannot overpower the trap; fighting it scales its strength.
-/
theorem the_trap_tightens (x y k v1 v2 : ℝ)
    (h_dist : |x - y| > 0)
    (hk : k > 0)
    (hv_nonneg : 0 ≤ v1)
    (hv_struggle : v1 < v2) :
    finger_trap_metric x y v1 k < finger_trap_metric x y v2 k := by
  unfold finger_trap_metric
  -- 1. Since 0 ≤ v1 < v2, we know v1² < v2²
  have h_v2 : v1 ^ 2 < v2 ^ 2 := by nlinarith
  -- 2. Multiply by k (strictly positive)
  have h_kv : k * v1 ^ 2 < k * v2 ^ 2 := (mul_lt_mul_left hk).mpr h_v2
  -- 3. Add 1 to both sides
  have h_1kv : 1 + k * v1 ^ 2 < 1 + k * v2 ^ 2 := add_lt_add_left h_kv 1
  -- 4. Multiply by the base physical distance (strictly positive)
  exact (mul_lt_mul_left h_dist).mpr h_1kv

end FingerTrap
