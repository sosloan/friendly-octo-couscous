package com.hft.java;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * Order record using Java modern features
 * Immutable value object for HFT orders
 */
public record Order(
    long orderId,
    String symbol,
    BigDecimal price,
    long quantity,
    OrderSide side
) {
    public Order {
        Objects.requireNonNull(symbol, "Symbol cannot be null");
        Objects.requireNonNull(price, "Price cannot be null");
        Objects.requireNonNull(side, "Side cannot be null");
        
        if (price.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Price must be positive");
        }
        if (quantity <= 0) {
            throw new IllegalArgumentException("Quantity must be positive");
        }
    }
    
    public BigDecimal calculateValue() {
        return price.multiply(BigDecimal.valueOf(quantity));
    }
    
    public boolean canMatchWith(Order other) {
        if (this.side == other.side) return false;
        if (!this.symbol.equals(other.symbol)) return false;
        
        if (this.side == OrderSide.BUY) {
            return this.price.compareTo(other.price) >= 0;
        } else {
            return other.price.compareTo(this.price) >= 0;
        }
    }
}
