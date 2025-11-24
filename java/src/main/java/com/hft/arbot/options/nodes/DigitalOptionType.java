package com.hft.arbot.options.nodes;

/**
 * Type of digital (binary) option
 */
public enum DigitalOptionType {
    CALL,  // Pays out if spot > strike
    PUT    // Pays out if spot < strike
}
