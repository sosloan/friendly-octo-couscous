package com.hft.alpaca.crypto;

import com.hft.alpaca.client.AlpacaClient;
import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;

/**
 * Alpaca Crypto API client.
 * Provides access to cryptocurrency trading and market data.
 */
public class AlpacaCryptoApi extends AlpacaClient {
    
    private static final String CRYPTO_DATA_PREFIX = "/v1beta3/crypto/us";
    
    public AlpacaCryptoApi(AlpacaConfig config) {
        super(config);
    }
    
    // ==================== Assets API ====================
    
    /**
     * Get list of all crypto assets.
     */
    public Asset[] getCryptoAssets() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/assets?asset_class=crypto"), Asset[].class);
    }
    
    /**
     * Get crypto assets asynchronously.
     */
    public CompletableFuture<Asset[]> getCryptoAssetsAsync() {
        return getAsync(buildTradingRequest("/v2/assets?asset_class=crypto"), Asset[].class);
    }
    
    /**
     * Get a specific crypto asset by symbol.
     */
    public Asset getCryptoAsset(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/assets/" + encoded), Asset.class);
    }
    
    // ==================== Orders API ====================
    
    /**
     * Get all crypto orders.
     */
    public Order[] getCryptoOrders() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/orders?asset_class=crypto"), Order[].class);
    }
    
    /**
     * Submit a crypto market order.
     */
    public Order submitCryptoMarketOrder(String symbol, BigDecimal quantity, OrderSide side) 
            throws IOException, InterruptedException {
        CryptoOrderRequest request = new CryptoOrderRequest(
            symbol,
            quantity.toPlainString(),
            null,
            side.name().toLowerCase(),
            "market",
            "gtc"
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a crypto limit order.
     */
    public Order submitCryptoLimitOrder(String symbol, BigDecimal quantity, OrderSide side, 
            BigDecimal limitPrice) throws IOException, InterruptedException {
        CryptoOrderRequest request = new CryptoOrderRequest(
            symbol,
            quantity.toPlainString(),
            limitPrice.toPlainString(),
            side.name().toLowerCase(),
            "limit",
            "gtc"
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a crypto order with notional amount (dollar amount).
     */
    public Order submitCryptoNotionalOrder(String symbol, BigDecimal notional, OrderSide side) 
            throws IOException, InterruptedException {
        CryptoNotionalOrderRequest request = new CryptoNotionalOrderRequest(
            symbol,
            notional.toPlainString(),
            side.name().toLowerCase(),
            "market",
            "gtc"
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Cancel a crypto order.
     */
    public void cancelCryptoOrder(String orderId) throws IOException, InterruptedException {
        delete(buildTradingRequest("/v2/orders/" + orderId), Void.class);
    }
    
    // ==================== Positions API ====================
    
    /**
     * Get all crypto positions.
     */
    public Position[] getCryptoPositions() throws IOException, InterruptedException {
        Position[] allPositions = get(buildTradingRequest("/v2/positions"), Position[].class);
        return java.util.Arrays.stream(allPositions)
            .filter(p -> "crypto".equalsIgnoreCase(p.assetClass()))
            .toArray(Position[]::new);
    }
    
    /**
     * Get position for a specific crypto symbol.
     */
    public Position getCryptoPosition(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/positions/" + encoded), Position.class);
    }
    
    /**
     * Close a crypto position.
     */
    public Order closeCryptoPosition(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return delete(buildTradingRequest("/v2/positions/" + encoded), Order.class);
    }
    
    // ==================== Market Data API ====================
    
    /**
     * Get historical bars for a crypto pair.
     */
    public Bar[] getCryptoBars(String symbol, String timeframe, Instant start, Instant end, int limit) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        StringBuilder url = new StringBuilder(CRYPTO_DATA_PREFIX)
            .append("/bars?symbols=").append(encoded)
            .append("&timeframe=").append(timeframe);
        
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
     * Get latest quote for a crypto pair.
     */
    public Quote getCryptoLatestQuote(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildDataRequest(CRYPTO_DATA_PREFIX + "/latest/quotes?symbols=" + encoded), Quote.class);
    }
    
    /**
     * Get latest trade for a crypto pair.
     */
    public Trade getCryptoLatestTrade(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return get(buildDataRequest(CRYPTO_DATA_PREFIX + "/latest/trades?symbols=" + encoded), Trade.class);
    }
    
    /**
     * Get historical trades for a crypto pair.
     */
    public Trade[] getCryptoTrades(String symbol, Instant start, Instant end, int limit) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        StringBuilder url = new StringBuilder(CRYPTO_DATA_PREFIX)
            .append("/trades?symbols=").append(encoded);
        
        if (start != null) {
            url.append("&start=").append(start.toString());
        }
        if (end != null) {
            url.append("&end=").append(end.toString());
        }
        if (limit > 0) {
            url.append("&limit=").append(limit);
        }
        
        return get(buildDataRequest(url.toString()), Trade[].class);
    }
    
    /**
     * Get snapshot for a crypto pair.
     */
    public String getCryptoSnapshot(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return getRaw(buildDataRequest(CRYPTO_DATA_PREFIX + "/snapshots?symbols=" + encoded));
    }
    
    /**
     * Get orderbook for a crypto pair.
     */
    public String getCryptoOrderbook(String symbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbol, StandardCharsets.UTF_8);
        return getRaw(buildDataRequest(CRYPTO_DATA_PREFIX + "/orderbooks/latest?symbols=" + encoded));
    }
    
    // ==================== Request Records ====================
    
    private record CryptoOrderRequest(
        String symbol,
        String qty,
        String limit_price,
        String side,
        String type,
        String time_in_force
    ) {}
    
    private record CryptoNotionalOrderRequest(
        String symbol,
        String notional,
        String side,
        String type,
        String time_in_force
    ) {}
}
