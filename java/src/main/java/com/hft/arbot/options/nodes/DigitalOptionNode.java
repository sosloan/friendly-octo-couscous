package com.hft.arbot.options.nodes;

/**
 * Digital (Binary) Option Node - ComputeNode implementation
 * 
 * A digital option pays a fixed amount if the option expires in the money,
 * zero otherwise. Ultra-low latency implementation with zero-GC design.
 * 
 * Pricing formula uses cumulative normal distribution.
 */
public final class DigitalOptionNode {
    
    private final DigitalOptionType type;
    private final double strike;
    private final double payout;
    private final double time;
    private final double rate;
    private final double volatility;
    
    // Pre-computed values to avoid GC
    private final double sqrtTime;
    private final double volSqrtTime;
    
    private DigitalOptionNode(DigitalOptionType type, double strike, double payout, 
                              double time, double rate, double volatility) {
        this.type = type;
        this.strike = strike;
        this.payout = payout;
        this.time = time;
        this.rate = rate;
        this.volatility = volatility;
        
        // Pre-compute for performance
        this.sqrtTime = Math.sqrt(time);
        this.volSqrtTime = volatility * sqrtTime;
    }
    
    /**
     * Create a digital call option
     */
    public static DigitalOptionNode createCall(double strike, double payout, 
                                               double time, double rate, double volatility) {
        return new DigitalOptionNode(DigitalOptionType.CALL, strike, payout, time, rate, volatility);
    }
    
    /**
     * Create a digital put option
     */
    public static DigitalOptionNode createPut(double strike, double payout, 
                                              double time, double rate, double volatility) {
        return new DigitalOptionNode(DigitalOptionType.PUT, strike, payout, time, rate, volatility);
    }
    
    /**
     * Compute option price given spot price
     */
    public double compute(double spot) {
        double d2 = computeD2(spot);
        double discountFactor = Math.exp(-rate * time);
        
        if (type == DigitalOptionType.CALL) {
            return payout * discountFactor * cumulativeNormal(d2);
        } else {
            return payout * discountFactor * cumulativeNormal(-d2);
        }
    }
    
    /**
     * Delta: sensitivity to spot price
     */
    public double delta(double spot) {
        double d2 = computeD2(spot);
        double discountFactor = Math.exp(-rate * time);
        double normPdf = normalPdf(d2);
        
        if (type == DigitalOptionType.CALL) {
            return payout * discountFactor * normPdf / (spot * volSqrtTime);
        } else {
            return -payout * discountFactor * normPdf / (spot * volSqrtTime);
        }
    }
    
    /**
     * Gamma: second derivative w.r.t. spot
     */
    public double gamma(double spot) {
        double d2 = computeD2(spot);
        double d1 = d2 + volSqrtTime;
        double discountFactor = Math.exp(-rate * time);
        double normPdf = normalPdf(d2);
        
        double term1 = -normPdf / (spot * spot * volSqrtTime);
        double term2 = 1.0 + d1 / volSqrtTime;
        
        if (type == DigitalOptionType.CALL) {
            return payout * discountFactor * term1 * term2;
        } else {
            return -payout * discountFactor * term1 * term2;
        }
    }
    
    /**
     * Vega: sensitivity to volatility
     */
    public double vega(double spot) {
        double d2 = computeD2(spot);
        double discountFactor = Math.exp(-rate * time);
        double normPdf = normalPdf(d2);
        
        if (type == DigitalOptionType.CALL) {
            return -payout * discountFactor * normPdf * sqrtTime;
        } else {
            return payout * discountFactor * normPdf * sqrtTime;
        }
    }
    
    /**
     * Theta: time decay
     */
    public double theta(double spot) {
        double d2 = computeD2(spot);
        double discountFactor = Math.exp(-rate * time);
        double cdfValue = type == DigitalOptionType.CALL ? 
            cumulativeNormal(d2) : cumulativeNormal(-d2);
        
        // Approximate theta calculation
        double term1 = rate * payout * discountFactor * cdfValue;
        double normPdf = normalPdf(d2);
        double term2 = payout * discountFactor * normPdf * (0.5 / sqrtTime);
        
        return -term1 - term2;
    }
    
    /**
     * Compute d2 parameter from Black-Scholes
     */
    private double computeD2(double spot) {
        double d1 = (Math.log(spot / strike) + (rate + 0.5 * volatility * volatility) * time) / volSqrtTime;
        return d1 - volSqrtTime;
    }
    
    /**
     * Cumulative normal distribution (approximation)
     */
    private double cumulativeNormal(double x) {
        // Abramowitz and Stegun approximation
        if (x < -10.0) return 0.0;
        if (x > 10.0) return 1.0;
        
        double t = 1.0 / (1.0 + 0.2316419 * Math.abs(x));
        double d = 0.3989423 * Math.exp(-x * x / 2.0);
        double p = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
        
        return x > 0.0 ? 1.0 - p : p;
    }
    
    /**
     * Normal probability density function
     */
    private double normalPdf(double x) {
        return Math.exp(-0.5 * x * x) / Math.sqrt(2.0 * Math.PI);
    }
    
    // Getters
    public DigitalOptionType getType() {
        return type;
    }
    
    public double getStrike() {
        return strike;
    }
    
    public double getPayout() {
        return payout;
    }
}
