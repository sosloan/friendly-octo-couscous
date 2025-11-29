package com.hft.alpaca.client;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AlpacaConfig.
 */
class AlpacaConfigTest {

    @Test
    @DisplayName("Config should be created with valid parameters")
    void testValidConfig() {
        AlpacaConfig config = new AlpacaConfig(
            "test-api-key",
            "test-secret-key",
            "https://paper-api.alpaca.markets",
            "https://data.alpaca.markets"
        );
        
        assertEquals("test-api-key", config.apiKey());
        assertEquals("test-secret-key", config.secretKey());
        assertEquals("https://paper-api.alpaca.markets", config.baseUrl());
        assertEquals("https://data.alpaca.markets", config.dataUrl());
    }
    
    @Test
    @DisplayName("Config should use default URLs when null")
    void testDefaultUrls() {
        AlpacaConfig config = new AlpacaConfig(
            "test-api-key",
            "test-secret-key",
            null,
            null
        );
        
        assertEquals(AlpacaConfig.PAPER_TRADING_URL, config.baseUrl());
        assertEquals(AlpacaConfig.DATA_URL, config.dataUrl());
    }
    
    @Test
    @DisplayName("Config should throw exception for null API key")
    void testNullApiKeyThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaConfig(null, "secret", null, null);
        });
    }
    
    @Test
    @DisplayName("Config should throw exception for null secret key")
    void testNullSecretKeyThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaConfig("api-key", null, null, null);
        });
    }
    
    @Test
    @DisplayName("Paper trading factory should use correct URL")
    void testPaperTradingFactory() {
        AlpacaConfig config = AlpacaConfig.paperTrading("api-key", "secret-key");
        
        assertEquals(AlpacaConfig.PAPER_TRADING_URL, config.baseUrl());
        assertEquals(AlpacaConfig.DATA_URL, config.dataUrl());
    }
    
    @Test
    @DisplayName("Live trading factory should use correct URL")
    void testLiveTradingFactory() {
        AlpacaConfig config = AlpacaConfig.liveTrading("api-key", "secret-key");
        
        assertEquals(AlpacaConfig.LIVE_TRADING_URL, config.baseUrl());
        assertEquals(AlpacaConfig.DATA_URL, config.dataUrl());
    }
}
