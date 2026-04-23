use iut_landscape::{IUTLandscape20, TopologyClass, BOTTLENECK_S5, BRANCHING_S13, RUGGEDNESS_R, SIGMA_F};

fn class_label(c: TopologyClass) -> &'static str {
    match c {
        TopologyClass::Boundary  => "👣 Boundary ",
        TopologyClass::Optimum   => "⛰️  Optimum  ",
        TopologyClass::Saddle    => "🔗 Saddle   ",
        TopologyClass::Symmetric => "🦋 Symmetric",
        TopologyClass::Rugged    => "🐎 Rugged   ",
    }
}

fn main() {
    println!();
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("  IUT Landscape — Topological Invariants  (🐎 animal path)");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("  ‖x − sᵢ‖ = c·Δtᵢ   ←→   ‖profile − aᵢ‖ = rᵢ(βᵢ)");
    println!();
    println!("  Landscape σ(f)     : {SIGMA_F}");
    println!("  Ruggedness (R)     : {RUGGEDNESS_R}");
    println!("  Feet (Boundaries)  : {{ 1 20 }}");
    println!("  Head (Optima)      : {{ 11 12 }}");
    println!("  Bottleneck (s=5)   : {BOTTLENECK_S5}");
    println!("  Neck (Saddles)     : {{ 5 10 13 }}");
    println!("  Wings (Symmetric)  : {{ 6 14 17 }}");
    println!("  Body (Rugged)      : {{ 2 3 4 7 8 9 15 16 18 19 }}");
    println!("  Branching (s=13)   : {BRANCHING_S13}");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!();

    let mut landscape = IUTLandscape20::new();
    landscape.execute_throughput_division();

    println!("  Node  Class          Velocity      IUT Teleport");
    println!("  ────  ─────────────  ────────────  ────────────");
    for i in 0..20 {
        let node = i + 1;
        let class = class_label(landscape.classes[i]);
        let vel = landscape.traversal_velocities[i];
        let teleport = if landscape.iut_teleport_active[i] { "🛸 ACTIVE" } else { "  –" };
        println!("  [{node:02}]  {class}  {vel:>12.6}  {teleport}");
    }

    println!();
    println!("  🐎  AÑGEL | ISLÃND — IUT landscape computed.");
    println!();
}
