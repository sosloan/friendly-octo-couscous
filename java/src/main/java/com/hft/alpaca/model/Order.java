package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents a trading order.
 */
public record Order(
    String id,
    String clientOrderId,
    Instant createdAt,
    Instant updatedAt,
    Instant submittedAt,
    Instant filledAt,
    Instant expiredAt,
    Instant canceledAt,
    String assetId,
    String symbol,
    String assetClass,
    BigDecimal quantity,
    BigDecimal filledQuantity,
    OrderType type,
    OrderSide side,
    TimeInForce timeInForce,
    BigDecimal limitPrice,
    BigDecimal stopPrice,
    BigDecimal filledAvgPrice,
    OrderStatus status,
    boolean extendedHours,
    BigDecimal trailPercent,
    BigDecimal trailPrice
) {
    /**
     * Check if order is completely filled.
     */
    public boolean isFilled() {
        return status == OrderStatus.FILLED;
    }
    
    /**
     * Check if order is still active.
     */
    public boolean isActive() {
        return status == OrderStatus.NEW || 
               status == OrderStatus.ACCEPTED ||
               status == OrderStatus.PARTIALLY_FILLED ||
               status == OrderStatus.PENDING_NEW;
    }
    
    /**
     * Calculate remaining quantity to fill.
     */
    public BigDecimal getRemainingQuantity() {
        if (quantity == null || filledQuantity == null) {
            return quantity;
        }
        return quantity.subtract(filledQuantity);
    }
}
