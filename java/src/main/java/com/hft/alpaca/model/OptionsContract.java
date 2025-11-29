package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Represents an options contract.
 */
public record OptionsContract(
    String id,
    String symbol,
    String name,
    String status,
    boolean tradable,
    String underlyingSymbol,
    String underlyingAssetId,
    OptionType type,
    String style,
    BigDecimal strikePrice,
    BigDecimal multiplier,
    BigDecimal size,
    LocalDate expirationDate,
    String rootSymbol,
    BigDecimal openInterest,
    BigDecimal openInterestDate,
    BigDecimal closePrice,
    LocalDate closePriceDate
) {
    /**
     * Check if option is a call.
     */
    public boolean isCall() {
        return type == OptionType.CALL;
    }
    
    /**
     * Check if option is a put.
     */
    public boolean isPut() {
        return type == OptionType.PUT;
    }
    
    /**
     * Check if option is in the money given the current underlying price.
     */
    public boolean isInTheMoney(BigDecimal underlyingPrice) {
        if (underlyingPrice == null || strikePrice == null) {
            return false;
        }
        if (isCall()) {
            return underlyingPrice.compareTo(strikePrice) > 0;
        } else {
            return underlyingPrice.compareTo(strikePrice) < 0;
        }
    }
    
    /**
     * Calculate intrinsic value given the current underlying price.
     */
    public BigDecimal getIntrinsicValue(BigDecimal underlyingPrice) {
        if (underlyingPrice == null || strikePrice == null) {
            return BigDecimal.ZERO;
        }
        BigDecimal diff;
        if (isCall()) {
            diff = underlyingPrice.subtract(strikePrice);
        } else {
            diff = strikePrice.subtract(underlyingPrice);
        }
        return diff.compareTo(BigDecimal.ZERO) > 0 ? diff : BigDecimal.ZERO;
    }
}
