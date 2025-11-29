package com.hft.alpaca.news;

import com.hft.alpaca.client.AlpacaClient;
import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.NewsArticle;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;

/**
 * Alpaca News API client.
 * Provides access to market news and financial information.
 */
public class AlpacaNewsApi extends AlpacaClient {
    
    private static final String NEWS_ENDPOINT = "/v1beta1/news";
    
    public AlpacaNewsApi(AlpacaConfig config) {
        super(config);
    }
    
    // ==================== News API ====================
    
    /**
     * Get latest news articles.
     */
    public NewsArticle[] getNews(int limit) throws IOException, InterruptedException {
        StringBuilder url = new StringBuilder(NEWS_ENDPOINT);
        if (limit > 0) {
            url.append("?limit=").append(Math.min(limit, 50));
        }
        return get(buildDataRequest(url.toString()), NewsArticle[].class);
    }
    
    /**
     * Get news articles asynchronously.
     */
    public CompletableFuture<NewsArticle[]> getNewsAsync(int limit) {
        StringBuilder url = new StringBuilder(NEWS_ENDPOINT);
        if (limit > 0) {
            url.append("?limit=").append(Math.min(limit, 50));
        }
        return getAsync(buildDataRequest(url.toString()), NewsArticle[].class);
    }
    
    /**
     * Get news for specific symbols.
     */
    public NewsArticle[] getNewsForSymbols(String... symbols) throws IOException, InterruptedException {
        if (symbols == null || symbols.length == 0) {
            return getNews(50);
        }
        
        String symbolList = String.join(",", symbols);
        String encoded = URLEncoder.encode(symbolList, StandardCharsets.UTF_8);
        return get(buildDataRequest(NEWS_ENDPOINT + "?symbols=" + encoded), NewsArticle[].class);
    }
    
    /**
     * Get news for symbols asynchronously.
     */
    public CompletableFuture<NewsArticle[]> getNewsForSymbolsAsync(String... symbols) {
        if (symbols == null || symbols.length == 0) {
            return getNewsAsync(50);
        }
        
        String symbolList = String.join(",", symbols);
        String encoded = URLEncoder.encode(symbolList, StandardCharsets.UTF_8);
        return getAsync(buildDataRequest(NEWS_ENDPOINT + "?symbols=" + encoded), NewsArticle[].class);
    }
    
    /**
     * Get news within a time range.
     */
    public NewsArticle[] getNewsByTimeRange(Instant start, Instant end, int limit) 
            throws IOException, InterruptedException {
        StringBuilder url = new StringBuilder(NEWS_ENDPOINT).append("?");
        
        boolean hasParam = false;
        if (start != null) {
            url.append("start=").append(start.toString());
            hasParam = true;
        }
        if (end != null) {
            if (hasParam) url.append("&");
            url.append("end=").append(end.toString());
            hasParam = true;
        }
        if (limit > 0) {
            if (hasParam) url.append("&");
            url.append("limit=").append(Math.min(limit, 50));
        }
        
        return get(buildDataRequest(url.toString()), NewsArticle[].class);
    }
    
    /**
     * Get news with full filtering options.
     */
    public NewsArticle[] getNewsFiltered(NewsQuery query) throws IOException, InterruptedException {
        StringBuilder url = new StringBuilder(NEWS_ENDPOINT).append("?");
        
        boolean hasParam = false;
        
        if (query.symbols() != null && query.symbols().length > 0) {
            String symbolList = String.join(",", query.symbols());
            url.append("symbols=").append(URLEncoder.encode(symbolList, StandardCharsets.UTF_8));
            hasParam = true;
        }
        
        if (query.start() != null) {
            if (hasParam) url.append("&");
            url.append("start=").append(query.start().toString());
            hasParam = true;
        }
        
        if (query.end() != null) {
            if (hasParam) url.append("&");
            url.append("end=").append(query.end().toString());
            hasParam = true;
        }
        
        if (query.limit() > 0) {
            if (hasParam) url.append("&");
            url.append("limit=").append(Math.min(query.limit(), 50));
            hasParam = true;
        }
        
        if (query.sort() != null) {
            if (hasParam) url.append("&");
            url.append("sort=").append(query.sort().name().toLowerCase());
            hasParam = true;
        }
        
        if (query.includeContent()) {
            if (hasParam) url.append("&");
            url.append("include_content=true");
        }
        
        return get(buildDataRequest(url.toString()), NewsArticle[].class);
    }
    
    /**
     * Get a single news article by ID.
     */
    public NewsArticle getNewsById(long articleId) throws IOException, InterruptedException {
        return get(buildDataRequest(NEWS_ENDPOINT + "/" + articleId), NewsArticle.class);
    }
    
    /**
     * Search news by keyword (in headline/summary).
     */
    public NewsArticle[] searchNews(String keyword, int limit) throws IOException, InterruptedException {
        // Note: Alpaca doesn't have a direct keyword search, but we filter client-side
        // In a production environment, you might want to use a different approach
        NewsArticle[] articles = getNews(Math.max(limit * 3, 50));
        String lowercaseKeyword = keyword.toLowerCase();
        
        return java.util.Arrays.stream(articles)
            .filter(article -> 
                (article.headline() != null && article.headline().toLowerCase().contains(lowercaseKeyword)) ||
                (article.summary() != null && article.summary().toLowerCase().contains(lowercaseKeyword)))
            .limit(limit)
            .toArray(NewsArticle[]::new);
    }
    
    // ==================== Query Builder ====================
    
    /**
     * Create a news query builder.
     */
    public static NewsQueryBuilder query() {
        return new NewsQueryBuilder();
    }
    
    /**
     * News query record for filtering.
     */
    public record NewsQuery(
        String[] symbols,
        Instant start,
        Instant end,
        int limit,
        SortOrder sort,
        boolean includeContent
    ) {}
    
    /**
     * Sort order enumeration.
     */
    public enum SortOrder {
        ASC,
        DESC
    }
    
    /**
     * Builder class for constructing news queries.
     */
    public static class NewsQueryBuilder {
        private String[] symbols;
        private Instant start;
        private Instant end;
        private int limit = 50;
        private SortOrder sort = SortOrder.DESC;
        private boolean includeContent = false;
        
        public NewsQueryBuilder symbols(String... symbols) {
            this.symbols = symbols;
            return this;
        }
        
        public NewsQueryBuilder start(Instant start) {
            this.start = start;
            return this;
        }
        
        public NewsQueryBuilder end(Instant end) {
            this.end = end;
            return this;
        }
        
        public NewsQueryBuilder limit(int limit) {
            this.limit = limit;
            return this;
        }
        
        public NewsQueryBuilder sort(SortOrder sort) {
            this.sort = sort;
            return this;
        }
        
        public NewsQueryBuilder includeContent(boolean includeContent) {
            this.includeContent = includeContent;
            return this;
        }
        
        public NewsQuery build() {
            return new NewsQuery(symbols, start, end, limit, sort, includeContent);
        }
    }
}
