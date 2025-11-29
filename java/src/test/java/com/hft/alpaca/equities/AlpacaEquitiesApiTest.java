package com.hft.alpaca.equities;

import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AlpacaEquitiesApi.
 */
class AlpacaEquitiesApiTest {

    private AlpacaConfig config;
    private AlpacaEquitiesApi api;
    
    @BeforeEach
    void setUp() {
        config = AlpacaConfig.paperTrading("test-api-key", "test-secret-key");
        api = new AlpacaEquitiesApi(config);
    }
    
    @Test
    @DisplayName("API should be created with valid config")
    void testApiCreation() {
        assertNotNull(api);
        assertEquals(config, api.getConfig());
    }
    
    @Test
    @DisplayName("API should throw exception for null config")
    void testNullConfigThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaEquitiesApi(null);
        });
    }
    
    @Test
    @DisplayName("Order request should be constructed properly")
    void testOrderConstruction() {
        // Testing the model classes
        Order order = new Order(
            "test-id",
            "client-id",
            java.time.Instant.now(),
            null, null, null, null, null,
            "asset-id",
            "AAPL",
            "us_equity",
            java.math.BigDecimal.valueOf(100),
            java.math.BigDecimal.ZERO,
            OrderType.LIMIT,
            OrderSide.BUY,
            TimeInForce.DAY,
            java.math.BigDecimal.valueOf(150.00),
            null,
            null,
            OrderStatus.NEW,
            false,
            null, null
        );
        
        assertEquals("AAPL", order.symbol());
        assertEquals(OrderType.LIMIT, order.type());
        assertEquals(OrderSide.BUY, order.side());
        assertTrue(order.isActive());
        assertFalse(order.isFilled());
    }
    
    @Test
    @DisplayName("Position should calculate profitability correctly")
    void testPositionProfitability() {
        Position profitable = new Position(
            "asset-id", "AAPL", "NASDAQ", "us_equity",
            java.math.BigDecimal.valueOf(100),
            java.math.BigDecimal.valueOf(10),
            "long",
            java.math.BigDecimal.valueOf(1100),
            java.math.BigDecimal.valueOf(1000),
            java.math.BigDecimal.valueOf(100), // Unrealized profit
            java.math.BigDecimal.valueOf(10),
            java.math.BigDecimal.valueOf(50),
            java.math.BigDecimal.valueOf(5),
            java.math.BigDecimal.valueOf(110),
            java.math.BigDecimal.valueOf(100),
            java.math.BigDecimal.valueOf(10)
        );
        
        assertTrue(profitable.isProfitable());
        assertTrue(profitable.isLong());
        assertFalse(profitable.isShort());
        
        Position losing = new Position(
            "asset-id", "AAPL", "NASDAQ", "us_equity",
            java.math.BigDecimal.valueOf(100),
            java.math.BigDecimal.valueOf(10),
            "long",
            java.math.BigDecimal.valueOf(900),
            java.math.BigDecimal.valueOf(1000),
            java.math.BigDecimal.valueOf(-100), // Unrealized loss
            java.math.BigDecimal.valueOf(-10),
            java.math.BigDecimal.valueOf(-50),
            java.math.BigDecimal.valueOf(-5),
            java.math.BigDecimal.valueOf(90),
            java.math.BigDecimal.valueOf(100),
            java.math.BigDecimal.valueOf(-10)
        );
        
        assertFalse(losing.isProfitable());
    }
    
    @Test
    @DisplayName("Account should check active status correctly")
    void testAccountStatus() {
        Account active = new Account(
            "id", "number", "ACTIVE", "USD",
            java.math.BigDecimal.valueOf(10000),
            java.math.BigDecimal.valueOf(50000),
            false, false, false, false, false,
            java.time.Instant.now(),
            true,
            java.math.BigDecimal.valueOf(40000),
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.valueOf(50000),
            java.math.BigDecimal.valueOf(50000),
            java.math.BigDecimal.valueOf(4),
            java.math.BigDecimal.valueOf(100000),
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.ZERO,
            java.math.BigDecimal.valueOf(100000),
            java.math.BigDecimal.valueOf(50000)
        );
        
        assertTrue(active.isActive());
        assertEquals(java.math.BigDecimal.valueOf(100000), active.getAvailableBuyingPower());
    }
}
