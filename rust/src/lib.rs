// ---------------------------------------------------------
// INVARIANTES TOPOLÓGICOS / TOPOLOGICAL INVARIANTS
// ---------------------------------------------------------
//
// Dual-sphere localization identity:
//   ‖x − sᵢ‖ = c·Δtᵢ   ←→   ‖profile − aᵢ‖ = rᵢ(βᵢ)
//
// 20-node linear fitness landscape metrics (🐎 animal path):
//   Landscape σ(f)     : 3.1603
//   Ruggedness (R)     : 0.4896
//   Feet (Boundaries)  : { 1 20 }
//   Head (Optima)      : { 11 12 }
//   Bottleneck (s=5)   : 0.3158
//   Neck (Saddles)     : { 5 10 13 }
//   Wings (Symmetric)  : { 6 14 17 }
//   Body (Rugged)      : { 2 3 4 7 8 9 15 16 18 19 }
//   Branching (s=13)   : 1
// ---------------------------------------------------------

pub const SIGMA_F: f64 = 3.1603;
pub const RUGGEDNESS_R: f64 = 0.4896;
pub const BOTTLENECK_S5: f64 = 0.3158;
pub const BRANCHING_S13: f64 = 1.0;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TopologyClass {
    Boundary,   // 👣 Pies / Fronteras [1, 20]
    Optimum,    // ⛰️  Cabeza / Óptimos [11, 12]
    Saddle,     // 🔗 Cuello / Sillas [5, 10, 13]
    Symmetric,  // 🦋 Alas / Simétrico [6, 14, 17]
    Rugged,     // 🐎 Cuerpo / Rugoso [2, 3, 4, 7, 8, 9, 15, 16, 18, 19]
}

pub struct IUTLandscape20 {
    pub classes: [TopologyClass; 20],
    pub throughput_data: [f64; 20],
    pub traversal_velocities: [f64; 20],
    pub iut_teleport_active: [bool; 20],
}

impl IUTLandscape20 {
    pub fn new() -> Self {
        let mut classes = [TopologyClass::Rugged; 20];
        // 👣 Feet / Boundaries
        classes[0] = TopologyClass::Boundary;  // [01]
        classes[19] = TopologyClass::Boundary; // [20]

        // ⛰️  Head / Optima
        classes[10] = TopologyClass::Optimum;  // [11]
        classes[11] = TopologyClass::Optimum;  // [12]

        // 🔗 Neck / Saddles
        classes[4] = TopologyClass::Saddle;    // [05] Cuello_botella
        classes[9] = TopologyClass::Saddle;    // [10]
        classes[12] = TopologyClass::Saddle;   // [13] Bifurcación

        // 🦋 Wings / Symmetric
        classes[5] = TopologyClass::Symmetric; // [06]
        classes[13] = TopologyClass::Symmetric;// [14]
        classes[16] = TopologyClass::Symmetric;// [17]

        Self {
            classes,
            throughput_data: [100.0; 20], // Baseline energy / energía base
            traversal_velocities: [0.0; 20],
            iut_teleport_active: [false; 20],
        }
    }

    /// THROUGHPUT-DIVISION-DATA
    /// Calculates the deterministic lock using division over landscape constraints.
    pub fn execute_throughput_division(&mut self) {
        let mut i = 0;

        while i < 20 {
            let data = self.throughput_data[i];

            self.traversal_velocities[i] = match self.classes[i] {
                // 🐎 Cuerpo / Rugoso: Divide by (Sigma + Ruggedness)
                TopologyClass::Rugged => data / (SIGMA_F + RUGGEDNESS_R),

                // 🔗 Cuello_botella (s=5): Severe throughput division
                TopologyClass::Saddle if i == 4 => data / (SIGMA_F / BOTTLENECK_S5),

                // 🔗 Bifurcación (s=13): Standard division, triggers IUT evaluation
                TopologyClass::Saddle if i == 12 => data / BRANCHING_S13,

                // 🦋 Alas / Simétrico: Symmetrical orbit maintains perfect momentum
                TopologyClass::Symmetric => data / SIGMA_F,

                // Default boundaries and optima
                _ => data / SIGMA_F,
            };

            // 🛸 PLATILLO VOLADOR / IUT TELEPORT
            // If the velocity drops below bottleneck thresholds, trigger the IUT phase shift.
            // ‖profile − aᵢ‖ = rᵢ(βᵢ)
            if self.traversal_velocities[i] < BOTTLENECK_S5 {
                self.iut_teleport_active[i] = true;
            } else {
                self.iut_teleport_active[i] = false;
            }

            i += 1;
        }
    }
}

impl Default for IUTLandscape20 {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------
// Tests
// ---------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_topology_classes() {
        let landscape = IUTLandscape20::new();

        // Feet / Boundaries
        assert_eq!(landscape.classes[0], TopologyClass::Boundary);
        assert_eq!(landscape.classes[19], TopologyClass::Boundary);

        // Head / Optima
        assert_eq!(landscape.classes[10], TopologyClass::Optimum);
        assert_eq!(landscape.classes[11], TopologyClass::Optimum);

        // Neck / Saddles
        assert_eq!(landscape.classes[4], TopologyClass::Saddle);
        assert_eq!(landscape.classes[9], TopologyClass::Saddle);
        assert_eq!(landscape.classes[12], TopologyClass::Saddle);

        // Wings / Symmetric
        assert_eq!(landscape.classes[5], TopologyClass::Symmetric);
        assert_eq!(landscape.classes[13], TopologyClass::Symmetric);
        assert_eq!(landscape.classes[16], TopologyClass::Symmetric);

        // Body / Rugged (sample)
        for &i in &[1, 2, 3, 6, 7, 8, 14, 15, 17, 18] {
            assert_eq!(landscape.classes[i], TopologyClass::Rugged);
        }
    }

    #[test]
    fn test_constants() {
        assert!((SIGMA_F - 3.1603).abs() < 1e-10);
        assert!((RUGGEDNESS_R - 0.4896).abs() < 1e-10);
        assert!((BOTTLENECK_S5 - 0.3158).abs() < 1e-10);
        assert!((BRANCHING_S13 - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_throughput_division_rugged() {
        let mut landscape = IUTLandscape20::new();
        landscape.execute_throughput_division();
        // Rugged node [2] (index 1): 100 / (SIGMA_F + RUGGEDNESS_R)
        let expected = 100.0 / (SIGMA_F + RUGGEDNESS_R);
        assert!((landscape.traversal_velocities[1] - expected).abs() < 1e-10);
    }

    #[test]
    fn test_throughput_division_bottleneck_s5() {
        let mut landscape = IUTLandscape20::new();
        landscape.execute_throughput_division();
        // Saddle at index 4 (node 5 / s=5): 100 / (SIGMA_F / BOTTLENECK_S5)
        let expected = 100.0 / (SIGMA_F / BOTTLENECK_S5);
        assert!((landscape.traversal_velocities[4] - expected).abs() < 1e-10);
    }

    #[test]
    fn test_throughput_division_branching_s13() {
        let mut landscape = IUTLandscape20::new();
        landscape.execute_throughput_division();
        // Saddle at index 12 (node 13 / s=13): 100 / BRANCHING_S13
        let expected = 100.0 / BRANCHING_S13;
        assert!((landscape.traversal_velocities[12] - expected).abs() < 1e-10);
        // Velocity = 100 >= BOTTLENECK_S5, so IUT teleport should be false
        assert!(!landscape.iut_teleport_active[12]);
    }

    #[test]
    fn test_iut_teleport_activation() {
        let mut landscape = IUTLandscape20::new();
        landscape.execute_throughput_division();
        // Nodes with velocity < BOTTLENECK_S5 (0.3158) should have teleport active.
        // Saddle at index 9 (node 10): uses default arm → 100 / SIGMA_F ≈ 31.6 → teleport off
        assert!(!landscape.iut_teleport_active[9]);
        // Any node where velocity ends up below 0.3158 should be teleport-on.
        for i in 0..20 {
            let vel = landscape.traversal_velocities[i];
            assert_eq!(landscape.iut_teleport_active[i], vel < BOTTLENECK_S5);
        }
    }
}
