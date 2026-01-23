package com.hft.alpaca.client;

import java.util.Objects;

/**
 * Configuration for Alpaca API client.
 * Contains API keys and endpoint URLs.
 * 
 * Security Note: Credentials should be loaded from environment variables
 * or a secure secrets management service, never hardcoded.
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
    
    /**
     * Create config from environment variables.
     * Reads ALPACA_API_KEY, ALPACA_SECRET_KEY, and ALPACA_MODE.
     * 
     * @return AlpacaConfig loaded from environment
     * @throws IllegalStateException if required environment variables are not set
     */
    public static AlpacaConfig fromEnvironment() {
        String apiKey = System.getenv("ALPACA_API_KEY");
        String secretKey = System.getenv("ALPACA_SECRET_KEY");
        String mode = System.getenv("ALPACA_MODE");
        
        if (apiKey == null || apiKey.isBlank()) {
            throw new IllegalStateException("ALPACA_API_KEY environment variable is not set");
        }
        if (secretKey == null || secretKey.isBlank()) {
            throw new IllegalStateException("ALPACA_SECRET_KEY environment variable is not set");
        }
        
        // Default to paper trading if mode is not specified
        boolean isPaper = mode == null || mode.equalsIgnoreCase("paper");
        
        String baseUrl = System.getenv("ALPACA_BASE_URL");
        String dataUrl = System.getenv("ALPACA_DATA_URL");
        
        if (baseUrl == null || baseUrl.isBlank()) {
            baseUrl = isPaper ? PAPER_TRADING_URL : LIVE_TRADING_URL;
        }
        if (dataUrl == null || dataUrl.isBlank()) {
            dataUrl = DATA_URL;
        }
        
        return new AlpacaConfig(apiKey, secretKey, baseUrl, dataUrl);
    }
    
    /**
     * Override toString to avoid accidentally logging sensitive credentials.
     */
    @Override
    public String toString() {
        return "AlpacaConfig{" +
            "apiKey=***REDACTED***, " +
            "secretKey=***REDACTED***, " +
            "baseUrl='" + baseUrl + "', " +
            "dataUrl='" + dataUrl + "'" +
            "}";
    }
}
