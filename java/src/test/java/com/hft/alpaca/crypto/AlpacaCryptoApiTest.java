package com.hft.alpaca.crypto;

import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;

/**
 * Tests for AlpacaCryptoApi.
 */
class AlpacaCryptoApiTest {

    private AlpacaConfig config;
    private AlpacaCryptoApi api;
    
    @BeforeEach
    void setUp() {
        config = AlpacaConfig.paperTrading("test-api-key", "test-secret-key");
        api = new AlpacaCryptoApi(config);
    }
    
    @Test
    @DisplayName("Crypto API should be created with valid config")
    void testApiCreation() {
        assertNotNull(api);
        assertEquals(config, api.getConfig());
    }
    
    @Test
    @DisplayName("Crypto API should throw exception for null config")
    void testNullConfigThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaCryptoApi(null);
        });
    }
    
    @Test
    @DisplayName("Crypto asset model should work correctly")
    void testCryptoAssetModel() {
        Asset asset = new Asset(
            "crypto-btc-usd",
            "crypto",
            "CRYPTO",
            "BTC/USD",
            "Bitcoin",
            "active",
            true,
            false,
            false,
            false,
            true,
            0.0001,
            0.0001,
            0.01
        );
        
        assertEquals("BTC/USD", asset.symbol());
        assertEquals("crypto", asset.assetClass());
        assertTrue(asset.isAvailable());
        assertTrue(asset.fractionable());
    }
    
    @Test
    @DisplayName("Trade model should calculate value correctly")
    void testTradeValue() {
        Trade trade = new Trade(
            "BTC/USD",
            java.time.Instant.now(),
            BigDecimal.valueOf(50000),
            2,
            "CRYPTO",
            12345L,
            new String[]{"@"}
        );
        
        assertEquals(BigDecimal.valueOf(100000), trade.getValue());
    }
    
    @Test
    @DisplayName("Quote model should calculate spread and mid price")
    void testQuoteCalculations() {
        Quote quote = new Quote(
            "BTC/USD",
            java.time.Instant.now(),
            BigDecimal.valueOf(49900),  // bid
            10,
            "CRYPTO",
            BigDecimal.valueOf(50100),  // ask
            10,
            "CRYPTO"
        );
        
        assertEquals(BigDecimal.valueOf(200), quote.getSpread());
        assertEquals(BigDecimal.valueOf(50000), quote.getMidPrice());
    }
    
    @Test
    @DisplayName("Bar model should validate prices")
    void testBarValidation() {
        // Valid bar
        Bar validBar = new Bar(
            "BTC/USD",
            java.time.Instant.now(),
            BigDecimal.valueOf(50000),
            BigDecimal.valueOf(51000),
            BigDecimal.valueOf(49000),
            BigDecimal.valueOf(50500),
            1000,
            100,
            BigDecimal.valueOf(50250)
        );
        
        assertNotNull(validBar);
        
        // Invalid bar with negative price should throw
        assertThrows(IllegalArgumentException.class, () -> {
            new Bar(
                "BTC/USD",
                java.time.Instant.now(),
                BigDecimal.valueOf(-50000), // negative price
                BigDecimal.valueOf(51000),
                BigDecimal.valueOf(49000),
                BigDecimal.valueOf(50500),
                1000,
                100,
                BigDecimal.valueOf(50250)
            );
        });
        
        // Invalid bar with negative volume should throw
        assertThrows(IllegalArgumentException.class, () -> {
            new Bar(
                "BTC/USD",
                java.time.Instant.now(),
                BigDecimal.valueOf(50000),
                BigDecimal.valueOf(51000),
                BigDecimal.valueOf(49000),
                BigDecimal.valueOf(50500),
                -1000, // negative volume
                100,
                BigDecimal.valueOf(50250)
            );
        });
    }
}
