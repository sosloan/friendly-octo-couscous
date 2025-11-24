package com.hft.arbot.options.nodes;

/**
 * Mathematical constants with Greek symbols
 * Provides constants used in financial mathematics and options pricing
 */
public final class GreekConstants {
    
    // Mathematical constants: א Δ Ω φ 𝓜無λφ
    public static final double ALEPH = Double.POSITIVE_INFINITY;  // א - represents infinity/cardinal
    public static final double DELTA = 0.001;     // Δ - change/difference constant
    public static final double OMEGA = Math.PI * 2;  // Ω - angular frequency (2π)
    public static final double PHI = (1.0 + Math.sqrt(5.0)) / 2.0;  // φ - golden ratio (~1.618)
    public static final double LAMBDA = 0.69314718056;  // λ - logarithmic constant (ln(2))
    public static final double MU = 0.5772156649;  // 𝓜/μ - Euler-Mascheroni constant (γ)
    public static final double MUU = 0.0;  // 無 - void/nothing (philosophical zero)
    public static final double PSI = PHI - 1;  // ψ - reciprocal of golden ratio (~0.618)
    
    // Additional mathematical constants
    public static final double E = Math.E;  // e - Euler's number (~2.718)
    public static final double PI = Math.PI;  // π - pi (~3.14159)
    public static final double TAU = Math.PI * 2;  // τ - tau (2π)
    public static final double EPSILON = 2.220446049250313e-16;  // ε - machine epsilon
    public static final double GAMMA = 0.5772156649015329;  // γ - Euler-Mascheroni constant
    public static final double ETA = 0.5;  // η - eta (often used for efficiency)
    public static final double THETA = Math.PI / 4;  // θ - theta (45 degrees)
    public static final double SIGMA = 1.0;  // σ - sigma (standard deviation constant)
    public static final double ALPHA = 0.618;  // α - alpha (related to golden ratio)
    public static final double BETA = 0.382;  // β - beta (complement of alpha)
    public static final double KAPPA = 0.577;  // κ - kappa (curvature)
    public static final double RHO = 1.618;  // ρ - rho (density, related to phi)
    
    // Private constructor to prevent instantiation
    private GreekConstants() {
        throw new AssertionError("Cannot instantiate constants class");
    }
}
