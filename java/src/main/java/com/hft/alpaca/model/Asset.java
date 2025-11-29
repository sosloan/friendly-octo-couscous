package com.hft.alpaca.model;

/**
 * Represents an asset available for trading.
 */
public record Asset(
    String id,
    String assetClass,
    String exchange,
    String symbol,
    String name,
    String status,
    boolean tradable,
    boolean marginable,
    boolean shortable,
    boolean easyToBorrow,
    boolean fractionable,
    double minOrderSize,
    double minTradeIncrement,
    double priceIncrement
) {
    /**
     * Check if asset is active and tradable.
     */
    public boolean isAvailable() {
        return "active".equalsIgnoreCase(status) && tradable;
    }
}
