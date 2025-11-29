package com.hft.alpaca.equities;

import com.hft.alpaca.client.AlpacaClient;
import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Alpaca Equities (Stocks & ETFs) API client.
 * Provides access to trading and market data for US equities.
 */
public class AlpacaEquitiesApi extends AlpacaClient {
    
    public AlpacaEquitiesApi(AlpacaConfig config) {
        super(config);
    }
    
    // ==================== Account API ====================
    
    /**
     * Get current account information.
     */
    public Account getAccount() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/account"), Account.class);
    }
    
    /**
     * Get account information asynchronously.
     */
    public CompletableFuture<Account> getAccountAsync() {
        return getAsync(buildTradingRequest("/v2/account"), Account.class);
    }
    
    // ==================== Assets API ====================
    
    /**
     * Get list of all assets.
     */
    public Asset[] getAssets() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/assets"), Asset[].class);
    }
    
    /**
     * Get list of assets asynchronously.
     */
    public CompletableFuture<Asset[]> getAssetsAsync() {
        return getAsync(buildTradingRequest("/v2/assets"), Asset[].class);
    }
    
    /**
     * Get a specific asset by symbol.
     */
    public Asset getAsset(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/assets/" + encoded), Asset.class);
    }
    
    /**
     * Get asset by symbol asynchronously.
     */
    public CompletableFuture<Asset> getAssetAsync(String symbol) {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return getAsync(buildTradingRequest("/v2/assets/" + encoded), Asset.class);
    }
    
    // ==================== Orders API ====================
    
    /**
     * Get all orders.
     */
    public Order[] getOrders() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/orders"), Order[].class);
    }
    
    /**
     * Get orders asynchronously.
     */
    public CompletableFuture<Order[]> getOrdersAsync() {
        return getAsync(buildTradingRequest("/v2/orders"), Order[].class);
    }
    
    /**
     * Get a specific order by ID.
     */
    public Order getOrder(String orderId) throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/orders/" + orderId), Order.class);
    }
    
    /**
     * Submit a market order.
     */
    public Order submitMarketOrder(String symbol, BigDecimal quantity, OrderSide side) 
            throws IOException, InterruptedException {
        OrderRequest request = new OrderRequest(
            symbol, 
            quantity.toPlainString(), 
            side.name().toLowerCase(), 
            "market", 
            "day",
            null, null, false, null, null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a limit order.
     */
    public Order submitLimitOrder(String symbol, BigDecimal quantity, OrderSide side, 
            BigDecimal limitPrice, TimeInForce timeInForce) throws IOException, InterruptedException {
        OrderRequest request = new OrderRequest(
            symbol,
            quantity.toPlainString(),
            side.name().toLowerCase(),
            "limit",
            timeInForce.name().toLowerCase(),
            limitPrice.toPlainString(),
            null, false, null, null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a stop order.
     */
    public Order submitStopOrder(String symbol, BigDecimal quantity, OrderSide side, 
            BigDecimal stopPrice, TimeInForce timeInForce) throws IOException, InterruptedException {
        OrderRequest request = new OrderRequest(
            symbol,
            quantity.toPlainString(),
            side.name().toLowerCase(),
            "stop",
            timeInForce.name().toLowerCase(),
            null,
            stopPrice.toPlainString(),
            false, null, null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a stop limit order.
     */
    public Order submitStopLimitOrder(String symbol, BigDecimal quantity, OrderSide side, 
            BigDecimal limitPrice, BigDecimal stopPrice, TimeInForce timeInForce) 
            throws IOException, InterruptedException {
        OrderRequest request = new OrderRequest(
            symbol,
            quantity.toPlainString(),
            side.name().toLowerCase(),
            "stop_limit",
            timeInForce.name().toLowerCase(),
            limitPrice.toPlainString(),
            stopPrice.toPlainString(),
            false, null, null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Cancel an order by ID.
     */
    public void cancelOrder(String orderId) throws IOException, InterruptedException {
        delete(buildTradingRequest("/v2/orders/" + orderId), Void.class);
    }
    
    /**
     * Cancel all open orders.
     */
    public void cancelAllOrders() throws IOException, InterruptedException {
        delete(buildTradingRequest("/v2/orders"), Void.class);
    }
    
    // ==================== Positions API ====================
    
    /**
     * Get all open positions.
     */
    public Position[] getPositions() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/positions"), Position[].class);
    }
    
    /**
     * Get positions asynchronously.
     */
    public CompletableFuture<Position[]> getPositionsAsync() {
        return getAsync(buildTradingRequest("/v2/positions"), Position[].class);
    }
    
    /**
     * Get position for a specific symbol.
     */
    public Position getPosition(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/positions/" + encoded), Position.class);
    }
    
    /**
     * Close a position by symbol.
     */
    public Order closePosition(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return delete(buildTradingRequest("/v2/positions/" + encoded), Order.class);
    }
    
    /**
     * Close all positions.
     */
    public void closeAllPositions() throws IOException, InterruptedException {
        delete(buildTradingRequest("/v2/positions"), Void.class);
    }
    
    // ==================== Market Data API ====================
    
    /**
     * Get historical bars for a symbol.
     */
    public Bar[] getBars(String symbol, String timeframe, Instant start, Instant end, int limit) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        StringBuilder url = new StringBuilder("/v2/stocks/")
            .append(encoded)
            .append("/bars?timeframe=").append(timeframe);
        
        if (start != null) {
            url.append("&start=").append(start.toString());
        }
        if (end != null) {
            url.append("&end=").append(end.toString());
        }
        if (limit > 0) {
            url.append("&limit=").append(limit);
        }
        
        return get(buildDataRequest(url.toString()), Bar[].class);
    }
    
    /**
     * Get latest quote for a symbol.
     */
    public Quote getLatestQuote(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildDataRequest("/v2/stocks/" + encoded + "/quotes/latest"), Quote.class);
    }
    
    /**
     * Get latest trade for a symbol.
     */
    public Trade getLatestTrade(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildDataRequest("/v2/stocks/" + encoded + "/trades/latest"), Trade.class);
    }
    
    /**
     * Get snapshot for a symbol (latest bar, quote, and trade).
     */
    public String getSnapshot(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return getRaw(buildDataRequest("/v2/stocks/" + encoded + "/snapshot"));
    }
    
    // ==================== Request Records ====================
    
    private record OrderRequest(
        String symbol,
        String qty,
        String side,
        String type,
        String time_in_force,
        String limit_price,
        String stop_price,
        boolean extended_hours,
        String trail_percent,
        String trail_price
    ) {}
}
