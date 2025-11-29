package com.hft.alpaca.model;

/**
 * Time in force enumeration.
 */
public enum TimeInForce {
    DAY,        // Day order
    GTC,        // Good till canceled
    OPG,        // Market on open
    CLS,        // Market on close
    IOC,        // Immediate or cancel
    FOK         // Fill or kill
}
