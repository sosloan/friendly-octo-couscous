package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents a quote with bid/ask prices.
 */
public record Quote(
    String symbol,
    Instant timestamp,
    BigDecimal bidPrice,
    long bidSize,
    String bidExchange,
    BigDecimal askPrice,
    long askSize,
    String askExchange
) {
    /**
     * Calculate bid-ask spread.
     */
    public BigDecimal getSpread() {
        if (askPrice == null || bidPrice == null) {
            return null;
        }
        return askPrice.subtract(bidPrice);
    }
    
    /**
     * Calculate mid price.
     */
    public BigDecimal getMidPrice() {
        if (askPrice == null || bidPrice == null) {
            return null;
        }
        return askPrice.add(bidPrice).divide(BigDecimal.valueOf(2));
    }
}
