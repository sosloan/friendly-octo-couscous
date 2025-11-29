package com.hft.alpaca.client;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for AlpacaApiException.
 */
class AlpacaApiExceptionTest {

    @Test
    @DisplayName("Exception should contain status code and body")
    void testExceptionProperties() {
        AlpacaApiException exception = new AlpacaApiException(401, "Unauthorized");
        
        assertEquals(401, exception.getStatusCode());
        assertEquals("Unauthorized", exception.getResponseBody());
        assertTrue(exception.getMessage().contains("401"));
        assertTrue(exception.getMessage().contains("Unauthorized"));
    }
    
    @Test
    @DisplayName("Exception message should be descriptive")
    void testExceptionMessage() {
        AlpacaApiException exception = new AlpacaApiException(404, "Not Found");
        
        String message = exception.getMessage();
        assertNotNull(message);
        assertTrue(message.contains("Alpaca API error"));
        assertTrue(message.contains("404"));
    }
}
