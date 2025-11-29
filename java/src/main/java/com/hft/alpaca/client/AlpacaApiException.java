package com.hft.alpaca.client;

/**
 * Exception thrown when Alpaca API returns an error.
 */
public class AlpacaApiException extends RuntimeException {
    
    private final int statusCode;
    private final String responseBody;
    
    public AlpacaApiException(int statusCode, String responseBody) {
        super("Alpaca API error: HTTP " + statusCode + " - " + responseBody);
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }
    
    public int getStatusCode() {
        return statusCode;
    }
    
    public String getResponseBody() {
        return responseBody;
    }
}
