package com.hft.alpaca.model;

/**
 * Order status enumeration.
 */
public enum OrderStatus {
    NEW,
    ACCEPTED,
    PENDING_NEW,
    ACCEPTED_FOR_BIDDING,
    STOPPED,
    REJECTED,
    SUSPENDED,
    CALCULATED,
    PARTIALLY_FILLED,
    FILLED,
    DONE_FOR_DAY,
    CANCELED,
    EXPIRED,
    REPLACED,
    PENDING_CANCEL,
    PENDING_REPLACE
}
