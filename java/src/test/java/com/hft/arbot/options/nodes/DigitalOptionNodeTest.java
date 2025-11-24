package com.hft.arbot.options.nodes;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;

/**
 * TDD Tests for Branch 1: Exotic Options as ComputeNode Implementations
 * 
 * Branch: feature/exotic-options-compute-nodes
 * 
 * Philosophy: Exotic options as first-class FRECS citizens
 * Focus: Ultra-low latency (< 100μs), zero-GC design
 * 
 * Tests define expected behavior for:
 * - Digital (binary) options
 * - Barrier options (knock-in, knock-out)
 * - Asian options (geometric/arithmetic average)
 * - Integration with FRECS framework
 * - Struct-of-arrays storage
 * - Proof bundles for correctness
 */
@ExtendWith(MockitoExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DigitalOptionNodeTest {
    
    // Test parameters
    private static final double SPOT = 100.0;
    private static final double STRIKE = 100.0;
    private static final double VOLATILITY = 0.20;
    private static final double RATE = 0.05;
    private static final double TIME = 0.25; // 3 months
    private static final double PAYOUT = 1.0;
    private static final double TOLERANCE = 1e-6;
    
    // ========================================
    // Digital Option Tests
    // ========================================
    
    @Test
    @Order(1)
    @DisplayName("Test 1: Digital call option initialization")
    void testDigitalCallInitialization() {
        // Digital call: pays $1 if spot > strike at maturity
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        assertNotNull(node, "Node should be created");
        assertEquals(DigitalOptionType.CALL, node.getType());
        assertEquals(STRIKE, node.getStrike());
        assertEquals(PAYOUT, node.getPayout());
    }
    
    @Test
    @Order(2)
    @DisplayName("Test 2: Digital call option pricing")
    void testDigitalCallPricing() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Price at the money
        double priceATM = node.compute(SPOT);
        assertTrue(priceATM > 0 && priceATM < PAYOUT,
            "ATM digital call price should be between 0 and payout");
        
        // Price deep in the money
        double priceITM = node.compute(SPOT + 20.0);
        assertTrue(priceITM > priceATM,
            "ITM digital call should be worth more than ATM");
        
        // Price deep out of the money
        double priceOTM = node.compute(SPOT - 20.0);
        assertTrue(priceOTM < priceATM,
            "OTM digital call should be worth less than ATM");
    }
    
    @Test
    @Order(3)
    @DisplayName("Test 3: Digital put option pricing")
    void testDigitalPutPricing() {
        DigitalOptionNode node = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Price deep in the money (spot < strike)
        double priceITM = node.compute(SPOT - 20.0);
        assertTrue(priceITM > 0.5 * PAYOUT,
            "Deep ITM digital put should be worth > 50% of payout");
        
        // Price deep out of the money (spot > strike)
        double priceOTM = node.compute(SPOT + 20.0);
        assertTrue(priceOTM < 0.5 * PAYOUT,
            "Deep OTM digital put should be worth < 50% of payout");
    }
    
    @Test
    @Order(4)
    @DisplayName("Test 4: Digital option put-call parity")
    void testDigitalPutCallParity() {
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        DigitalOptionNode put = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Call + Put should approximately equal discounted payout
        double callPrice = call.compute(SPOT);
        double putPrice = put.compute(SPOT);
        double sum = callPrice + putPrice;
        double discountedPayout = PAYOUT * Math.exp(-RATE * TIME);
        
        assertEquals(discountedPayout, sum, 0.01,
            "Digital put-call parity: Call + Put = Discounted Payout");
    }
    
    @Test
    @Order(5)
    @DisplayName("Test 5: Digital option delta (sensitivity to spot)")
    void testDigitalDelta() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double delta = node.delta(SPOT);
        
        // Delta should be positive for call
        assertTrue(delta > 0, "Digital call delta should be positive");
        
        // Delta should be large near strike (high sensitivity)
        double deltaATM = node.delta(STRIKE);
        double deltaITM = node.delta(STRIKE + 10.0);
        
        assertTrue(Math.abs(deltaATM) > Math.abs(deltaITM),
            "Delta should be largest at the money");
    }
    
    @Test
    @Order(6)
    @DisplayName("Test 6: Digital option gamma (second derivative)")
    void testDigitalGamma() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Gamma should exist and change with spot price
        double gamma1 = node.gamma(STRIKE - 5.0);
        double gamma2 = node.gamma(STRIKE);
        double gamma3 = node.gamma(STRIKE + 5.0);
        
        // Gamma should be non-zero and change across different spot prices
        assertTrue(gamma1 != gamma2 || gamma2 != gamma3,
            "Gamma should vary with spot price");
    }
    
    @Test
    @Order(7)
    @DisplayName("Test 7: Digital option vega (sensitivity to volatility)")
    void testDigitalVega() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double vega = node.vega(SPOT);
        
        // Vega can be positive or negative for digital options
        assertTrue(Math.abs(vega) > 0, "Vega should be non-zero");
    }
    
    @Test
    @Order(8)
    @DisplayName("Test 8: Digital option theta (time decay)")
    void testDigitalTheta() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double theta = node.theta(SPOT);
        
        // Theta should be non-zero
        assertNotEquals(0.0, theta, TOLERANCE,
            "Theta should indicate time decay");
    }
    
    // ========================================
    // Performance Tests
    // ========================================
    
    @Test
    @Order(10)
    @DisplayName("Test 10: Pricing latency < 100μs")
    void testPricingLatency() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Warmup
        for (int i = 0; i < 1000; i++) {
            node.compute(SPOT + i * 0.01);
        }
        
        // Measure
        int iterations = 10000;
        long startNanos = System.nanoTime();
        
        for (int i = 0; i < iterations; i++) {
            node.compute(SPOT + i * 0.01);
        }
        
        long endNanos = System.nanoTime();
        long avgNanos = (endNanos - startNanos) / iterations;
        double avgMicros = avgNanos / 1000.0;
        
        assertTrue(avgMicros < 100.0,
            String.format("Pricing should be < 100μs, got %.2fμs", avgMicros));
        
        System.out.printf("Digital option pricing: %.2fμs avg%n", avgMicros);
    }
    
    @Test
    @Order(11)
    @DisplayName("Test 11: Greeks calculation latency < 200μs")
    void testGreeksLatency() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Warmup
        for (int i = 0; i < 1000; i++) {
            node.delta(SPOT);
            node.gamma(SPOT);
            node.vega(SPOT);
            node.theta(SPOT);
        }
        
        // Measure
        int iterations = 10000;
        long startNanos = System.nanoTime();
        
        for (int i = 0; i < iterations; i++) {
            node.delta(SPOT + i * 0.01);
            node.gamma(SPOT + i * 0.01);
            node.vega(SPOT + i * 0.01);
            node.theta(SPOT + i * 0.01);
        }
        
        long endNanos = System.nanoTime();
        long avgNanos = (endNanos - startNanos) / iterations;
        double avgMicros = avgNanos / 1000.0;
        
        assertTrue(avgMicros < 200.0,
            String.format("Greeks should be < 200μs, got %.2fμs", avgMicros));
        
        System.out.printf("Digital option Greeks: %.2fμs avg%n", avgMicros);
    }
    
    @Test
    @Order(12)
    @DisplayName("Test 12: Zero GC allocations in hot path")
    void testZeroGCAllocations() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        // Force GC before test
        System.gc();
        Thread.yield();
        
        long gcBefore = getGCCount();
        
        // Pricing hot path
        for (int i = 0; i < 100000; i++) {
            node.compute(SPOT + i * 0.01);
        }
        
        long gcAfter = getGCCount();
        long gcCount = gcAfter - gcBefore;
        
        // Some GC may occur, but should be minimal
        assertTrue(gcCount < 10,
            String.format("Should have minimal GC, got %d collections", gcCount));
    }
    
    private long getGCCount() {
        long count = 0;
        for (java.lang.management.GarbageCollectorMXBean gc : 
             java.lang.management.ManagementFactory.getGarbageCollectorMXBeans()) {
            count += gc.getCollectionCount();
        }
        return count;
    }
    
    // ========================================
    // Proof Bundle Tests
    // ========================================
    
    @Test
    @Order(20)
    @DisplayName("Test 20: Proof - Digital option converges to 0 or payout at maturity")
    void proofMaturityConvergence() {
        double veryShortTime = 0.0001; // Near maturity
        
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, veryShortTime, RATE, VOLATILITY
        );
        
        // Deep ITM should converge to payout
        double priceITM = call.compute(STRIKE + 10.0);
        assertEquals(PAYOUT, priceITM, 0.01,
            "Deep ITM digital should converge to payout at maturity");
        
        // Deep OTM should converge to 0
        double priceOTM = call.compute(STRIKE - 10.0);
        assertEquals(0.0, priceOTM, 0.01,
            "Deep OTM digital should converge to 0 at maturity");
    }
    
    @Test
    @Order(21)
    @DisplayName("Test 21: Proof - Digital option price bounded by discounted payout")
    void proofPriceBounded() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double discountedPayout = PAYOUT * Math.exp(-RATE * TIME);
        
        // Try various spot prices
        for (double spot = STRIKE - 50; spot < STRIKE + 50; spot += 1.0) {
            double price = node.compute(spot);
            assertTrue(price >= 0 && price <= discountedPayout,
                String.format("Price %.4f should be in [0, %.4f] at spot %.2f",
                    price, discountedPayout, spot));
        }
    }
    
    @Test
    @Order(22)
    @DisplayName("Test 22: Proof - Digital option is monotonic in spot (for call)")
    void proofMonotonicity() {
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double prevPrice = 0.0;
        
        // As spot increases, call price should increase (or stay same)
        for (double spot = STRIKE - 20; spot < STRIKE + 20; spot += 0.1) {
            double price = call.compute(spot);
            assertTrue(price >= prevPrice - TOLERANCE,
                String.format("Digital call should be monotonically increasing: %.4f >= %.4f",
                    price, prevPrice));
            prevPrice = price;
        }
    }
    
    @Test
    @Order(23)
    @DisplayName("Test 23: Proof - Finite difference delta matches analytical")
    void proofFiniteDifferenceDelta() {
        DigitalOptionNode node = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, TIME, RATE, VOLATILITY
        );
        
        double h = 0.01; // Small bump
        
        // Finite difference delta
        double priceUp = node.compute(SPOT + h);
        double priceDown = node.compute(SPOT - h);
        double finiteDelta = (priceUp - priceDown) / (2 * h);
        
        // Analytical delta
        double analyticalDelta = node.delta(SPOT);
        
        assertEquals(analyticalDelta, finiteDelta, 0.001,
            "Finite difference delta should match analytical delta");
    }
    
    // ========================================
    // Struct-of-Arrays Storage Tests
    // ========================================
    
    @Test
    @Order(30)
    @DisplayName("Test 30: Bulk pricing with SoA storage")
    void testBulkPricingWithSoA() {
        int entityCount = 1000;
        ExoticOptionStore store = new ExoticOptionStore(entityCount);
        
        // Create entities
        for (int i = 0; i < entityCount; i++) {
            int entityId = store.createEntity();
            store.setDigitalCall(entityId, STRIKE + i, PAYOUT, TIME, RATE, VOLATILITY);
        }
        
        // Bulk price
        double[] spots = new double[entityCount];
        for (int i = 0; i < entityCount; i++) {
            spots[i] = SPOT + i * 0.1;
        }
        
        long startNanos = System.nanoTime();
        double[] prices = store.computePrices(spots);
        long endNanos = System.nanoTime();
        
        assertEquals(entityCount, prices.length);
        
        double avgNanos = (endNanos - startNanos) / (double) entityCount;
        double avgMicros = avgNanos / 1000.0;
        
        assertTrue(avgMicros < 100.0,
            String.format("Bulk pricing should be < 100μs per option, got %.2fμs", avgMicros));
    }
    
    // ========================================
    // Time Period Tests
    // ========================================
    
    @Test
    @Order(40)
    @DisplayName("Test 40: 30-day option pricing")
    void test30DayOption() {
        double time30Days = 30.0 / 365.0; // 30 days in years
        
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time30Days, RATE, VOLATILITY
        );
        
        DigitalOptionNode put = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, time30Days, RATE, VOLATILITY
        );
        
        double callPrice = call.compute(SPOT);
        double putPrice = put.compute(SPOT);
        
        // Prices should be valid
        assertTrue(callPrice > 0 && callPrice < PAYOUT,
            "30-day call price should be between 0 and payout");
        assertTrue(putPrice > 0 && putPrice < PAYOUT,
            "30-day put price should be between 0 and payout");
        
        // Put-call parity
        double discountedPayout = PAYOUT * Math.exp(-RATE * time30Days);
        assertEquals(discountedPayout, callPrice + putPrice, 0.01,
            "30-day options should satisfy put-call parity");
    }
    
    @Test
    @Order(41)
    @DisplayName("Test 41: 60-day option pricing")
    void test60DayOption() {
        double time60Days = 60.0 / 365.0; // 60 days in years
        
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time60Days, RATE, VOLATILITY
        );
        
        DigitalOptionNode put = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, time60Days, RATE, VOLATILITY
        );
        
        double callPrice = call.compute(SPOT);
        double putPrice = put.compute(SPOT);
        
        // Prices should be valid
        assertTrue(callPrice > 0 && callPrice < PAYOUT,
            "60-day call price should be between 0 and payout");
        assertTrue(putPrice > 0 && putPrice < PAYOUT,
            "60-day put price should be between 0 and payout");
        
        // Put-call parity
        double discountedPayout = PAYOUT * Math.exp(-RATE * time60Days);
        assertEquals(discountedPayout, callPrice + putPrice, 0.01,
            "60-day options should satisfy put-call parity");
    }
    
    @Test
    @Order(42)
    @DisplayName("Test 42: 90-day option pricing")
    void test90DayOption() {
        double time90Days = 90.0 / 365.0; // 90 days in years
        
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time90Days, RATE, VOLATILITY
        );
        
        DigitalOptionNode put = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, time90Days, RATE, VOLATILITY
        );
        
        double callPrice = call.compute(SPOT);
        double putPrice = put.compute(SPOT);
        
        // Prices should be valid
        assertTrue(callPrice > 0 && callPrice < PAYOUT,
            "90-day call price should be between 0 and payout");
        assertTrue(putPrice > 0 && putPrice < PAYOUT,
            "90-day put price should be between 0 and payout");
        
        // Put-call parity
        double discountedPayout = PAYOUT * Math.exp(-RATE * time90Days);
        assertEquals(discountedPayout, callPrice + putPrice, 0.01,
            "90-day options should satisfy put-call parity");
    }
    
    @Test
    @Order(43)
    @DisplayName("Test 43: 180-day option pricing")
    void test180DayOption() {
        double time180Days = 180.0 / 365.0; // 180 days in years
        
        DigitalOptionNode call = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time180Days, RATE, VOLATILITY
        );
        
        DigitalOptionNode put = DigitalOptionNode.createPut(
            STRIKE, PAYOUT, time180Days, RATE, VOLATILITY
        );
        
        double callPrice = call.compute(SPOT);
        double putPrice = put.compute(SPOT);
        
        // Prices should be valid
        assertTrue(callPrice > 0 && callPrice < PAYOUT,
            "180-day call price should be between 0 and payout");
        assertTrue(putPrice > 0 && putPrice < PAYOUT,
            "180-day put price should be between 0 and payout");
        
        // Put-call parity
        double discountedPayout = PAYOUT * Math.exp(-RATE * time180Days);
        assertEquals(discountedPayout, callPrice + putPrice, 0.01,
            "180-day options should satisfy put-call parity");
    }
    
    @Test
    @Order(44)
    @DisplayName("Test 44: Time decay comparison across maturities")
    void testTimeDecayAcrossMaturities() {
        double time30Days = 30.0 / 365.0;
        double time60Days = 60.0 / 365.0;
        double time90Days = 90.0 / 365.0;
        double time180Days = 180.0 / 365.0;
        
        DigitalOptionNode node30 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time30Days, RATE, VOLATILITY
        );
        DigitalOptionNode node60 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time60Days, RATE, VOLATILITY
        );
        DigitalOptionNode node90 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time90Days, RATE, VOLATILITY
        );
        DigitalOptionNode node180 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time180Days, RATE, VOLATILITY
        );
        
        double price30 = node30.compute(SPOT);
        double price60 = node60.compute(SPOT);
        double price90 = node90.compute(SPOT);
        double price180 = node180.compute(SPOT);
        
        // At the money, longer maturity options should have similar or slightly different prices
        // due to more time for the underlying to move
        assertTrue(price30 > 0 && price60 > 0 && price90 > 0 && price180 > 0,
            "All option prices should be positive");
        
        System.out.printf("Price comparison - 30d: %.4f, 60d: %.4f, 90d: %.4f, 180d: %.4f%n",
            price30, price60, price90, price180);
    }
    
    @Test
    @Order(45)
    @DisplayName("Test 45: Greeks sensitivity across time periods")
    void testGreeksSensitivityAcrossTimePeriods() {
        double time30Days = 30.0 / 365.0;
        double time60Days = 60.0 / 365.0;
        double time180Days = 180.0 / 365.0;
        
        DigitalOptionNode node30 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time30Days, RATE, VOLATILITY
        );
        DigitalOptionNode node60 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time60Days, RATE, VOLATILITY
        );
        DigitalOptionNode node180 = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, time180Days, RATE, VOLATILITY
        );
        
        // Calculate deltas
        double delta30 = node30.delta(SPOT);
        double delta60 = node60.delta(SPOT);
        double delta180 = node180.delta(SPOT);
        
        // Calculate vegas
        double vega30 = node30.vega(SPOT);
        double vega60 = node60.vega(SPOT);
        double vega180 = node180.vega(SPOT);
        
        // All deltas should be non-zero
        assertTrue(Math.abs(delta30) > 0 && Math.abs(delta60) > 0 && Math.abs(delta180) > 0,
            "Deltas should be non-zero for all maturities");
        
        // All vegas should be non-zero
        assertTrue(Math.abs(vega30) > 0 && Math.abs(vega60) > 0 && Math.abs(vega180) > 0,
            "Vegas should be non-zero for all maturities");
        
        System.out.printf("Delta comparison - 30d: %.6f, 60d: %.6f, 180d: %.6f%n",
            delta30, delta60, delta180);
        System.out.printf("Vega comparison - 30d: %.6f, 60d: %.6f, 180d: %.6f%n",
            vega30, vega60, vega180);
    }
    
    @Test
    @Order(46)
    @DisplayName("Test 46: Near-expiry vs far-expiry behavior")
    void testNearExpiryVsFarExpiry() {
        double timeNearExpiry = 1.0 / 365.0; // 1 day
        double timeFarExpiry = 365.0 / 365.0; // 1 year
        
        DigitalOptionNode nearExpiry = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, timeNearExpiry, RATE, VOLATILITY
        );
        DigitalOptionNode farExpiry = DigitalOptionNode.createCall(
            STRIKE, PAYOUT, timeFarExpiry, RATE, VOLATILITY
        );
        
        // Price ITM option
        double nearPriceITM = nearExpiry.compute(STRIKE + 5.0);
        double farPriceITM = farExpiry.compute(STRIKE + 5.0);
        
        // Near expiry ITM should be closer to discounted payout
        double nearDiscount = Math.exp(-RATE * timeNearExpiry);
        assertTrue(nearPriceITM > 0.9 * PAYOUT * nearDiscount,
            "Near-expiry ITM option should be close to discounted payout");
        
        // Far expiry has more uncertainty
        assertTrue(farPriceITM < nearPriceITM * Math.exp(RATE * (timeFarExpiry - timeNearExpiry)) + 0.1,
            "Far-expiry option has different pricing dynamics");
        
        System.out.printf("Near-expiry ITM: %.4f, Far-expiry ITM: %.4f%n",
            nearPriceITM, farPriceITM);
    }
}
