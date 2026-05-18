package com.hft.config;

/**
 * Top-level application configuration loaded from PKL via pkl-config-java.
 */
public record AppConfig(
    AlpacaSettings alpaca,
    ServerSettings server
) {}
