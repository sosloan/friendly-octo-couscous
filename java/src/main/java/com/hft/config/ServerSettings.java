package com.hft.config;

/**
 * Netty HFT server settings loaded from PKL configuration.
 */
public record ServerSettings(
    int port,
    int backlog,
    int workerThreads
) {}
