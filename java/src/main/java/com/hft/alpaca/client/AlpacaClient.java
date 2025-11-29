package com.hft.alpaca.client;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Logger;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * Base Alpaca API client with common HTTP functionality.
 * Uses Java 11+ HttpClient for async/sync HTTP requests.
 */
public class AlpacaClient {
    
    private static final Logger logger = Logger.getLogger(AlpacaClient.class.getName());
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);
    
    protected final AlpacaConfig config;
    protected final HttpClient httpClient;
    protected final Gson gson;
    
    public AlpacaClient(AlpacaConfig config) {
        this.config = Objects.requireNonNull(config, "Config cannot be null");
        this.httpClient = HttpClient.newBuilder()
            .connectTimeout(DEFAULT_TIMEOUT)
            .build();
        this.gson = new GsonBuilder()
            .setDateFormat("yyyy-MM-dd'T'HH:mm:ssX")
            .create();
    }
    
    /**
     * Build authenticated request to trading API.
     */
    protected HttpRequest.Builder buildTradingRequest(String endpoint) {
        return HttpRequest.newBuilder()
            .uri(URI.create(config.baseUrl() + endpoint))
            .header("APCA-API-KEY-ID", config.apiKey())
            .header("APCA-API-SECRET-KEY", config.secretKey())
            .header("Content-Type", "application/json")
            .timeout(DEFAULT_TIMEOUT);
    }
    
    /**
     * Build authenticated request to data API.
     */
    protected HttpRequest.Builder buildDataRequest(String endpoint) {
        return HttpRequest.newBuilder()
            .uri(URI.create(config.dataUrl() + endpoint))
            .header("APCA-API-KEY-ID", config.apiKey())
            .header("APCA-API-SECRET-KEY", config.secretKey())
            .header("Content-Type", "application/json")
            .timeout(DEFAULT_TIMEOUT);
    }
    
    /**
     * Execute synchronous GET request.
     */
    protected <T> T get(HttpRequest.Builder requestBuilder, Class<T> responseType) 
            throws IOException, InterruptedException {
        HttpRequest request = requestBuilder.GET().build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        
        if (response.statusCode() >= 400) {
            throw new AlpacaApiException(response.statusCode(), response.body());
        }
        
        return gson.fromJson(response.body(), responseType);
    }
    
    /**
     * Execute asynchronous GET request.
     */
    protected <T> CompletableFuture<T> getAsync(HttpRequest.Builder requestBuilder, Class<T> responseType) {
        HttpRequest request = requestBuilder.GET().build();
        return httpClient.sendAsync(request, HttpResponse.BodyHandlers.ofString())
            .thenApply(response -> {
                if (response.statusCode() >= 400) {
                    throw new AlpacaApiException(response.statusCode(), response.body());
                }
                return gson.fromJson(response.body(), responseType);
            });
    }
    
    /**
     * Execute synchronous POST request.
     */
    protected <T> T post(HttpRequest.Builder requestBuilder, Object body, Class<T> responseType) 
            throws IOException, InterruptedException {
        String jsonBody = gson.toJson(body);
        HttpRequest request = requestBuilder
            .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
            .build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        
        if (response.statusCode() >= 400) {
            throw new AlpacaApiException(response.statusCode(), response.body());
        }
        
        return gson.fromJson(response.body(), responseType);
    }
    
    /**
     * Execute asynchronous POST request.
     */
    protected <T> CompletableFuture<T> postAsync(HttpRequest.Builder requestBuilder, Object body, Class<T> responseType) {
        String jsonBody = gson.toJson(body);
        HttpRequest request = requestBuilder
            .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
            .build();
        return httpClient.sendAsync(request, HttpResponse.BodyHandlers.ofString())
            .thenApply(response -> {
                if (response.statusCode() >= 400) {
                    throw new AlpacaApiException(response.statusCode(), response.body());
                }
                return gson.fromJson(response.body(), responseType);
            });
    }
    
    /**
     * Execute synchronous DELETE request.
     */
    protected <T> T delete(HttpRequest.Builder requestBuilder, Class<T> responseType) 
            throws IOException, InterruptedException {
        HttpRequest request = requestBuilder.DELETE().build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        
        if (response.statusCode() >= 400) {
            throw new AlpacaApiException(response.statusCode(), response.body());
        }
        
        return response.body().isBlank() ? null : gson.fromJson(response.body(), responseType);
    }
    
    /**
     * Get raw response as string.
     */
    protected String getRaw(HttpRequest.Builder requestBuilder) 
            throws IOException, InterruptedException {
        HttpRequest request = requestBuilder.GET().build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        
        if (response.statusCode() >= 400) {
            throw new AlpacaApiException(response.statusCode(), response.body());
        }
        
        return response.body();
    }
    
    /**
     * Get the configuration.
     */
    public AlpacaConfig getConfig() {
        return config;
    }
}
