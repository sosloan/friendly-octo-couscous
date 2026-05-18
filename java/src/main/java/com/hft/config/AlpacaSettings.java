package com.hft.config;

/**
 * Alpaca API connection settings loaded from PKL configuration.
 * Sensitive credentials (apiKey, secretKey) are intentionally excluded and
 * must be supplied via environment variables at runtime.
 */
public record AlpacaSettings(
    String baseUrl,
    String dataUrl,
    int timeoutSeconds,
    String mode
) {}
