package com.hft.alpaca.client;

import java.util.Objects;

/**
 * Configuration for Alpaca API client.
 * Contains API keys and endpoint URLs.
 */
public record AlpacaConfig(
    String apiKey,
    String secretKey,
    String baseUrl,
    String dataUrl
) {
    // Default paper trading URLs
    public static final String PAPER_TRADING_URL = "https://paper-api.alpaca.markets";
    public static final String LIVE_TRADING_URL = "https://api.alpaca.markets";
    public static final String DATA_URL = "https://data.alpaca.markets";
    
    public AlpacaConfig {
        Objects.requireNonNull(apiKey, "API key cannot be null");
        Objects.requireNonNull(secretKey, "Secret key cannot be null");
        if (baseUrl == null || baseUrl.isBlank()) {
            baseUrl = PAPER_TRADING_URL;
        }
        if (dataUrl == null || dataUrl.isBlank()) {
            dataUrl = DATA_URL;
        }
    }
    
    /**
     * Create config for paper trading.
     */
    public static AlpacaConfig paperTrading(String apiKey, String secretKey) {
        return new AlpacaConfig(apiKey, secretKey, PAPER_TRADING_URL, DATA_URL);
    }
    
    /**
     * Create config for live trading.
     */
    public static AlpacaConfig liveTrading(String apiKey, String secretKey) {
        return new AlpacaConfig(apiKey, secretKey, LIVE_TRADING_URL, DATA_URL);
    }
}
