package com.hft.java;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;

/**
 * Test suite for HFT Java components
 */
class OrderTest {

    @Test
    @DisplayName("Valid order should be created successfully")
    void testValidOrderCreation() {
        Order order = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.50"),
            100L,
            OrderSide.BUY
        );
        
        assertEquals(1L, order.orderId());
        assertEquals("AAPL", order.symbol());
        assertEquals(new BigDecimal("150.50"), order.price());
        assertEquals(100L, order.quantity());
        assertEquals(OrderSide.BUY, order.side());
    }

    @Test
    @DisplayName("Order with zero price should throw exception")
    void testInvalidPriceThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> {
            new Order(
                1L,
                "AAPL",
                BigDecimal.ZERO,
                100L,
                OrderSide.BUY
            );
        });
    }

    @Test
    @DisplayName("Order with negative quantity should throw exception")
    void testInvalidQuantityThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> {
            new Order(
                1L,
                "AAPL",
                new BigDecimal("150.50"),
                -100L,
                OrderSide.BUY
            );
        });
    }

    @Test
    @DisplayName("Calculate order value correctly")
    void testOrderValueCalculation() {
        Order order = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.50"),
            100L,
            OrderSide.BUY
        );
        
        BigDecimal expectedValue = new BigDecimal("15050.00");
        assertEquals(0, expectedValue.compareTo(order.calculateValue()));
    }

    @Test
    @DisplayName("Buy order should match with lower sell order")
    void testOrderMatching() {
        Order buyOrder = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.50"),
            100L,
            OrderSide.BUY
        );
        
        Order sellOrder = new Order(
            2L,
            "AAPL",
            new BigDecimal("150.25"),
            100L,
            OrderSide.SELL
        );
        
        assertTrue(buyOrder.canMatchWith(sellOrder));
    }

    @Test
    @DisplayName("Buy order should not match with higher sell order")
    void testOrderNotMatching() {
        Order buyOrder = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.00"),
            100L,
            OrderSide.BUY
        );
        
        Order sellOrder = new Order(
            2L,
            "AAPL",
            new BigDecimal("150.75"),
            100L,
            OrderSide.SELL
        );
        
        assertFalse(buyOrder.canMatchWith(sellOrder));
    }

    @Test
    @DisplayName("Orders with different symbols should not match")
    void testDifferentSymbolsNoMatch() {
        Order buyOrder = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.50"),
            100L,
            OrderSide.BUY
        );
        
        Order sellOrder = new Order(
            2L,
            "GOOGL",
            new BigDecimal("150.25"),
            100L,
            OrderSide.SELL
        );
        
        assertFalse(buyOrder.canMatchWith(sellOrder));
    }

    @Test
    @DisplayName("Same side orders should not match")
    void testSameSideNoMatch() {
        Order buyOrder1 = new Order(
            1L,
            "AAPL",
            new BigDecimal("150.50"),
            100L,
            OrderSide.BUY
        );
        
        Order buyOrder2 = new Order(
            2L,
            "AAPL",
            new BigDecimal("150.25"),
            100L,
            OrderSide.BUY
        );
        
        assertFalse(buyOrder1.canMatchWith(buyOrder2));
    }
}
