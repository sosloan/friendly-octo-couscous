package com.hft.alpaca.client;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AlpacaClient base class.
 */
class AlpacaClientTest {

    private AlpacaConfig config;
    private AlpacaClient client;
    
    @BeforeEach
    void setUp() {
        config = AlpacaConfig.paperTrading("test-api-key", "test-secret-key");
        client = new AlpacaClient(config);
    }
    
    @Test
    @DisplayName("Client should be created with valid config")
    void testClientCreation() {
        assertNotNull(client);
        assertEquals(config, client.getConfig());
    }
    
    @Test
    @DisplayName("Client should throw exception for null config")
    void testNullConfigThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaClient(null);
        });
    }
    
    @Test
    @DisplayName("Config should be accessible via getter")
    void testGetConfig() {
        AlpacaConfig retrievedConfig = client.getConfig();
        
        assertEquals("test-api-key", retrievedConfig.apiKey());
        assertEquals("test-secret-key", retrievedConfig.secretKey());
    }
}
