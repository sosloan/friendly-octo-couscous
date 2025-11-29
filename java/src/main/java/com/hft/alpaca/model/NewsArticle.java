package com.hft.alpaca.model;

import java.time.Instant;
import java.util.List;

/**
 * Represents a news article.
 */
public record NewsArticle(
    long id,
    String headline,
    String author,
    Instant createdAt,
    Instant updatedAt,
    String summary,
    String content,
    String url,
    List<String> images,
    List<String> symbols,
    String source
) {
    /**
     * Check if article is related to a specific symbol.
     */
    public boolean relatesToSymbol(String symbol) {
        if (symbols == null || symbol == null) {
            return false;
        }
        return symbols.stream()
            .anyMatch(s -> s.equalsIgnoreCase(symbol));
    }
    
    /**
     * Check if article has images.
     */
    public boolean hasImages() {
        return images != null && !images.isEmpty();
    }
}
