package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents an OHLCV bar (candlestick) for price data.
 */
public record Bar(
    String symbol,
    Instant timestamp,
    BigDecimal open,
    BigDecimal high,
    BigDecimal low,
    BigDecimal close,
    long volume,
    long tradeCount,
    BigDecimal vwap
) {
    public Bar {
        if (open != null && open.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Open price cannot be negative");
        }
        if (high != null && high.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("High price cannot be negative");
        }
        if (low != null && low.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Low price cannot be negative");
        }
        if (close != null && close.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Close price cannot be negative");
        }
        if (volume < 0) {
            throw new IllegalArgumentException("Volume cannot be negative");
        }
    }
}
