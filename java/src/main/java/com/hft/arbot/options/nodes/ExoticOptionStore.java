package com.hft.arbot.options.nodes;

/**
 * Exotic Option Store using Struct-of-Arrays (SoA) design
 * 
 * Provides cache-friendly storage for bulk option pricing operations.
 * Zero-GC design for hot path operations.
 */
public final class ExoticOptionStore {
    
    // Struct-of-Arrays storage
    private final DigitalOptionNode[] nodes;
    private final int capacity;
    private int size;
    
    public ExoticOptionStore(int capacity) {
        this.capacity = capacity;
        this.nodes = new DigitalOptionNode[capacity];
        this.size = 0;
    }
    
    /**
     * Create a new entity and return its ID
     */
    public int createEntity() {
        if (size >= capacity) {
            throw new IllegalStateException("Store is full");
        }
        return size++;
    }
    
    /**
     * Set digital call option for an entity
     */
    public void setDigitalCall(int entityId, double strike, double payout, 
                               double time, double rate, double volatility) {
        validateEntityId(entityId);
        nodes[entityId] = DigitalOptionNode.createCall(strike, payout, time, rate, volatility);
    }
    
    /**
     * Set digital put option for an entity
     */
    public void setDigitalPut(int entityId, double strike, double payout, 
                              double time, double rate, double volatility) {
        validateEntityId(entityId);
        nodes[entityId] = DigitalOptionNode.createPut(strike, payout, time, rate, volatility);
    }
    
    /**
     * Compute prices for all entities given spot prices
     * Zero-GC hot path
     */
    public double[] computePrices(double[] spots) {
        if (spots.length != size) {
            throw new IllegalArgumentException("Spot array size must match entity count");
        }
        
        double[] prices = new double[size];
        for (int i = 0; i < size; i++) {
            if (nodes[i] != null) {
                prices[i] = nodes[i].compute(spots[i]);
            }
        }
        return prices;
    }
    
    /**
     * Get node for entity
     */
    public DigitalOptionNode getNode(int entityId) {
        validateEntityId(entityId);
        return nodes[entityId];
    }
    
    private void validateEntityId(int entityId) {
        if (entityId < 0 || entityId >= size) {
            throw new IllegalArgumentException("Invalid entity ID: " + entityId);
        }
    }
    
    public int size() {
        return size;
    }
    
    public int capacity() {
        return capacity;
    }
}
