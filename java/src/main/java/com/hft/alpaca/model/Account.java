package com.hft.alpaca.model;

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Represents an account information.
 */
public record Account(
    String id,
    String accountNumber,
    String status,
    String currency,
    BigDecimal cash,
    BigDecimal portfolioValue,
    boolean patternDayTrader,
    boolean tradeSuspendedByUser,
    boolean tradingBlocked,
    boolean transfersBlocked,
    boolean accountBlocked,
    Instant createdAt,
    boolean shortingEnabled,
    BigDecimal longMarketValue,
    BigDecimal shortMarketValue,
    BigDecimal equity,
    BigDecimal lastEquity,
    BigDecimal multiplier,
    BigDecimal buyingPower,
    BigDecimal initialMargin,
    BigDecimal maintenanceMargin,
    BigDecimal sma,
    BigDecimal daytradeCount,
    BigDecimal lastMaintenanceMargin,
    BigDecimal daytradesBuyingPower,
    BigDecimal regtBuyingPower
) {
    /**
     * Check if account is active and can trade.
     */
    public boolean isActive() {
        return "ACTIVE".equalsIgnoreCase(status) && 
               !tradingBlocked && 
               !accountBlocked;
    }
    
    /**
     * Get available buying power for day trades.
     */
    public BigDecimal getAvailableBuyingPower() {
        return buyingPower != null ? buyingPower : BigDecimal.ZERO;
    }
}
