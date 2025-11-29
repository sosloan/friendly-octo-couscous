package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents a position in an asset.
 */
public record Position(
    String assetId,
    String symbol,
    String exchange,
    String assetClass,
    BigDecimal avgEntryPrice,
    BigDecimal quantity,
    String side,
    BigDecimal marketValue,
    BigDecimal costBasis,
    BigDecimal unrealizedPL,
    BigDecimal unrealizedPLPercent,
    BigDecimal unrealizedIntradayPL,
    BigDecimal unrealizedIntradayPLPercent,
    BigDecimal currentPrice,
    BigDecimal lastDayPrice,
    BigDecimal changeToday
) {
    /**
     * Check if position is profitable.
     */
    public boolean isProfitable() {
        return unrealizedPL != null && unrealizedPL.compareTo(BigDecimal.ZERO) > 0;
    }
    
    /**
     * Check if this is a long position.
     */
    public boolean isLong() {
        return "long".equalsIgnoreCase(side);
    }
    
    /**
     * Check if this is a short position.
     */
    public boolean isShort() {
        return "short".equalsIgnoreCase(side);
    }
}
