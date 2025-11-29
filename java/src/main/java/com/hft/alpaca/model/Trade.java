package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents a trade execution.
 */
public record Trade(
    String symbol,
    Instant timestamp,
    BigDecimal price,
    long size,
    String exchange,
    long tradeId,
    String[] conditions
) {
    /**
     * Calculate trade value.
     */
    public BigDecimal getValue() {
        if (price == null) {
            return null;
        }
        return price.multiply(BigDecimal.valueOf(size));
    }
}
