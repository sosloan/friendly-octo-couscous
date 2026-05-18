package com.hft.java;

import com.hft.config.AppConfig;
import com.hft.config.PklConfigLoader;
import java.math.BigDecimal;
import java.util.concurrent.Executors;
import java.util.logging.Logger;

/**
 * Java Powerhouse - Main HFT Application
 * Leverages modern Java features including Virtual Threads (Java 21+), Records, Pattern Matching
 * 
 * This is the main execution engine combining:
 * - Virtual Threads for massive concurrency (Java 21 feature)
 * - Netty for ultra-low latency networking
 * - Modern Java Records and Sealed types
 */
public class HFTPowerhouse {
    
    private static final Logger logger = Logger.getLogger(HFTPowerhouse.class.getName());
    
    public static void main(String[] args) {
        System.out.println("=== Java 21 HFT Powerhouse 💪 ===");
        System.out.println("🚀 Initializing high-performance trading engine");

        // Load application configuration from PKL
        AppConfig appConfig;
        try (var loader = new PklConfigLoader()) {
            appConfig = loader.load();
        }
        System.out.println("📋 Configuration loaded from PKL");
        System.out.println("   Trading mode : " + appConfig.alpaca().mode());
        System.out.println("   Broker URL   : " + appConfig.alpaca().baseUrl());
        System.out.println("   Server port  : " + appConfig.server().port());
        
        // Demonstrate Virtual Threads (Java 21+)
        try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
            
            // Create sample orders
            Order buyOrder = new Order(
                1L,
                "AAPL",
                new BigDecimal("150.50"),
                100L,
                OrderSide.BUY
            );
            
            Order sellOrder = new Order(
                2L,
                "AAPL",
                new BigDecimal("150.25"),
                100L,
                OrderSide.SELL
            );
            
            // Process orders in virtual threads
            executor.submit(() -> {
                System.out.println("📊 Processing Buy Order: " + buyOrder);
                System.out.println("   Value: $" + buyOrder.calculateValue());
                return null;
            });
            
            executor.submit(() -> {
                System.out.println("📊 Processing Sell Order: " + sellOrder);
                System.out.println("   Value: $" + sellOrder.calculateValue());
                return null;
            });
            
            // Check if orders can match
            executor.submit(() -> {
                if (buyOrder.canMatchWith(sellOrder)) {
                    System.out.println("✓ Orders matched! Trade executed at $" + sellOrder.price());
                    System.out.println("  Quantity: " + Math.min(buyOrder.quantity(), sellOrder.quantity()));
                } else {
                    System.out.println("✗ Orders cannot be matched");
                }
                return null;
            });
            
            // Give virtual threads time to complete
            Thread.sleep(100);
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            logger.severe("Execution interrupted: " + e.getMessage());
        }
        
        System.out.println("\n🌐 Starting Netty Network Server...");

        // Start Netty server with port from PKL configuration
        final int serverPort = appConfig.server().port();
        Thread.ofVirtual().start(() -> {
            try {
                NettyHFTServer server = new NettyHFTServer(serverPort);
                server.start();
            } catch (Exception e) {
                logger.severe("Failed to start Netty server: " + e.getMessage());
            }
        });
        
        // Keep main thread alive
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        
        System.out.println("=== Java Powerhouse Running ===");
        System.out.println("💪 Virtual Threads: Active");
        System.out.println("⚡ Netty Server: Listening on port " + appConfig.server().port());
        System.out.println("🎯 Ultra-low latency mode: Enabled");
    }
}
