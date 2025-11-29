package com.hft.alpaca.options;

import com.hft.alpaca.client.AlpacaClient;
import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.util.concurrent.CompletableFuture;

/**
 * Alpaca Options API client.
 * Provides access to options trading and market data.
 */
public class AlpacaOptionsApi extends AlpacaClient {
    
    public AlpacaOptionsApi(AlpacaConfig config) {
        super(config);
    }
    
    // ==================== Contracts API ====================
    
    /**
     * Get all options contracts for a symbol.
     */
    public OptionsContract[] getContracts(String underlyingSymbol) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/options/contracts?underlying_symbols=" + encoded), 
            OptionsContract[].class);
    }
    
    /**
     * Get contracts asynchronously.
     */
    public CompletableFuture<OptionsContract[]> getContractsAsync(String underlyingSymbol) {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        return getAsync(buildTradingRequest("/v2/options/contracts?underlying_symbols=" + encoded), 
            OptionsContract[].class);
    }
    
    /**
     * Get options contracts filtered by expiration date.
     */
    public OptionsContract[] getContractsByExpiration(String underlyingSymbol, LocalDate expirationDate) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        String url = "/v2/options/contracts?underlying_symbols=" + encoded +
            "&expiration_date=" + expirationDate.toString();
        return get(buildTradingRequest(url), OptionsContract[].class);
    }
    
    /**
     * Get options contracts filtered by type (call/put).
     */
    public OptionsContract[] getContractsByType(String underlyingSymbol, OptionType type) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        String url = "/v2/options/contracts?underlying_symbols=" + encoded +
            "&type=" + type.name().toLowerCase();
        return get(buildTradingRequest(url), OptionsContract[].class);
    }
    
    /**
     * Get options contracts filtered by strike price range.
     */
    public OptionsContract[] getContractsByStrikeRange(String underlyingSymbol, 
            BigDecimal minStrike, BigDecimal maxStrike) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        StringBuilder url = new StringBuilder("/v2/options/contracts?underlying_symbols=")
            .append(encoded);
        
        if (minStrike != null) {
            url.append("&strike_price_gte=").append(minStrike.toPlainString());
        }
        if (maxStrike != null) {
            url.append("&strike_price_lte=").append(maxStrike.toPlainString());
        }
        
        return get(buildTradingRequest(url.toString()), OptionsContract[].class);
    }
    
    /**
     * Get a specific options contract by symbol or ID.
     */
    public OptionsContract getContract(String symbolOrId) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(symbolOrId, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/options/contracts/" + encoded), OptionsContract.class);
    }
    
    // ==================== Orders API ====================
    
    /**
     * Get all options orders.
     */
    public Order[] getOptionsOrders() throws IOException, InterruptedException {
        return get(buildTradingRequest("/v2/orders?asset_class=options"), Order[].class);
    }
    
    /**
     * Submit an options market order.
     */
    public Order submitOptionsMarketOrder(String contractSymbol, BigDecimal quantity, OrderSide side) 
            throws IOException, InterruptedException {
        OptionsOrderRequest request = new OptionsOrderRequest(
            contractSymbol,
            quantity.toPlainString(),
            side.name().toLowerCase(),
            "market",
            "day",
            null, null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit an options limit order.
     */
    public Order submitOptionsLimitOrder(String contractSymbol, BigDecimal quantity, 
            OrderSide side, BigDecimal limitPrice) throws IOException, InterruptedException {
        OptionsOrderRequest request = new OptionsOrderRequest(
            contractSymbol,
            quantity.toPlainString(),
            side.name().toLowerCase(),
            "limit",
            "day",
            limitPrice.toPlainString(),
            null
        );
        return post(buildTradingRequest("/v2/orders"), request, Order.class);
    }
    
    /**
     * Submit a buy-to-open order (open long position).
     */
    public Order buyToOpen(String contractSymbol, BigDecimal quantity, BigDecimal limitPrice) 
            throws IOException, InterruptedException {
        return submitOptionsLimitOrder(contractSymbol, quantity, OrderSide.BUY, limitPrice);
    }
    
    /**
     * Submit a sell-to-close order (close long position).
     */
    public Order sellToClose(String contractSymbol, BigDecimal quantity, BigDecimal limitPrice) 
            throws IOException, InterruptedException {
        return submitOptionsLimitOrder(contractSymbol, quantity, OrderSide.SELL, limitPrice);
    }
    
    /**
     * Submit a sell-to-open order (open short position).
     */
    public Order sellToOpen(String contractSymbol, BigDecimal quantity, BigDecimal limitPrice) 
            throws IOException, InterruptedException {
        return submitOptionsLimitOrder(contractSymbol, quantity, OrderSide.SELL, limitPrice);
    }
    
    /**
     * Submit a buy-to-close order (close short position).
     */
    public Order buyToClose(String contractSymbol, BigDecimal quantity, BigDecimal limitPrice) 
            throws IOException, InterruptedException {
        return submitOptionsLimitOrder(contractSymbol, quantity, OrderSide.BUY, limitPrice);
    }
    
    /**
     * Cancel an options order.
     */
    public void cancelOptionsOrder(String orderId) throws IOException, InterruptedException {
        delete(buildTradingRequest("/v2/orders/" + orderId), Void.class);
    }
    
    // ==================== Positions API ====================
    
    /**
     * Get all options positions.
     */
    public Position[] getOptionsPositions() throws IOException, InterruptedException {
        Position[] allPositions = get(buildTradingRequest("/v2/positions"), Position[].class);
        return java.util.Arrays.stream(allPositions)
            .filter(p -> "option".equalsIgnoreCase(p.assetClass()) || 
                        "us_option".equalsIgnoreCase(p.assetClass()))
            .toArray(Position[]::new);
    }
    
    /**
     * Get position for a specific options contract.
     */
    public Position getOptionsPosition(String contractSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        return get(buildTradingRequest("/v2/positions/" + encoded), Position.class);
    }
    
    /**
     * Close an options position.
     */
    public Order closeOptionsPosition(String contractSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        return delete(buildTradingRequest("/v2/positions/" + encoded), Order.class);
    }
    
    // ==================== Market Data API ====================
    
    /**
     * Get historical bars for an options contract.
     */
    public Bar[] getOptionsBars(String contractSymbol, String timeframe, Instant start, Instant end, int limit) 
            throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        StringBuilder url = new StringBuilder("/v1beta1/options/bars?symbols=")
            .append(encoded)
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
     * Get latest quote for an options contract.
     */
    public Quote getOptionsLatestQuote(String contractSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        return get(buildDataRequest("/v1beta1/options/quotes/latest?symbols=" + encoded), Quote.class);
    }
    
    /**
     * Get latest trade for an options contract.
     */
    public Trade getOptionsLatestTrade(String contractSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        return get(buildDataRequest("/v1beta1/options/trades/latest?symbols=" + encoded), Trade.class);
    }
    
    /**
     * Get snapshot for an options contract.
     */
    public String getOptionsSnapshot(String contractSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(contractSymbol, StandardCharsets.UTF_8);
        return getRaw(buildDataRequest("/v1beta1/options/snapshots?symbols=" + encoded));
    }
    
    /**
     * Get option chain for an underlying symbol.
     */
    public String getOptionChain(String underlyingSymbol) throws IOException, InterruptedException {
        String encoded = URLEncoder.encode(underlyingSymbol, StandardCharsets.UTF_8);
        return getRaw(buildDataRequest("/v1beta1/options/snapshots/" + encoded));
    }
    
    // ==================== Request Records ====================
    
    private record OptionsOrderRequest(
        String symbol,
        String qty,
        String side,
        String type,
        String time_in_force,
        String limit_price,
        String stop_price
    ) {}
}
