package com.hft.alpaca.news;

import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.NewsArticle;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

import java.time.Instant;
import java.util.List;

/**
 * Tests for AlpacaNewsApi.
 */
class AlpacaNewsApiTest {

    private AlpacaConfig config;
    private AlpacaNewsApi api;
    
    @BeforeEach
    void setUp() {
        config = AlpacaConfig.paperTrading("test-api-key", "test-secret-key");
        api = new AlpacaNewsApi(config);
    }
    
    @Test
    @DisplayName("News API should be created with valid config")
    void testApiCreation() {
        assertNotNull(api);
        assertEquals(config, api.getConfig());
    }
    
    @Test
    @DisplayName("News API should throw exception for null config")
    void testNullConfigThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaNewsApi(null);
        });
    }
    
    @Test
    @DisplayName("NewsArticle should check symbol relevance correctly")
    void testNewsArticleSymbolRelevance() {
        NewsArticle article = new NewsArticle(
            12345L,
            "Apple reports record earnings",
            "John Doe",
            Instant.now(),
            Instant.now(),
            "Apple Inc. reported record quarterly earnings...",
            "Full article content here...",
            "https://example.com/news/1",
            List.of("https://example.com/image.jpg"),
            List.of("AAPL", "MSFT", "GOOGL"),
            "Reuters"
        );
        
        assertTrue(article.relatesToSymbol("AAPL"));
        assertTrue(article.relatesToSymbol("aapl")); // case insensitive
        assertTrue(article.relatesToSymbol("MSFT"));
        assertFalse(article.relatesToSymbol("TSLA"));
    }
    
    @Test
    @DisplayName("NewsArticle should check for images")
    void testNewsArticleImages() {
        NewsArticle withImages = new NewsArticle(
            1L, "Title", "Author", Instant.now(), Instant.now(),
            "Summary", "Content", "url",
            List.of("image1.jpg", "image2.jpg"),
            List.of("AAPL"),
            "Source"
        );
        
        assertTrue(withImages.hasImages());
        
        NewsArticle withoutImages = new NewsArticle(
            2L, "Title", "Author", Instant.now(), Instant.now(),
            "Summary", "Content", "url",
            List.of(),
            List.of("AAPL"),
            "Source"
        );
        
        assertFalse(withoutImages.hasImages());
        
        NewsArticle nullImages = new NewsArticle(
            3L, "Title", "Author", Instant.now(), Instant.now(),
            "Summary", "Content", "url",
            null,
            List.of("AAPL"),
            "Source"
        );
        
        assertFalse(nullImages.hasImages());
    }
    
    @Test
    @DisplayName("NewsQuery builder should work correctly")
    void testNewsQueryBuilder() {
        Instant now = Instant.now();
        Instant yesterday = now.minusSeconds(86400);
        
        AlpacaNewsApi.NewsQuery query = AlpacaNewsApi.query()
            .symbols("AAPL", "MSFT")
            .start(yesterday)
            .end(now)
            .limit(25)
            .sort(AlpacaNewsApi.SortOrder.DESC)
            .includeContent(true)
            .build();
        
        assertNotNull(query.symbols());
        assertEquals(2, query.symbols().length);
        assertEquals("AAPL", query.symbols()[0]);
        assertEquals("MSFT", query.symbols()[1]);
        assertEquals(yesterday, query.start());
        assertEquals(now, query.end());
        assertEquals(25, query.limit());
        assertEquals(AlpacaNewsApi.SortOrder.DESC, query.sort());
        assertTrue(query.includeContent());
    }
    
    @Test
    @DisplayName("NewsQuery builder should use defaults")
    void testNewsQueryBuilderDefaults() {
        AlpacaNewsApi.NewsQuery query = AlpacaNewsApi.query().build();
        
        assertNull(query.symbols());
        assertNull(query.start());
        assertNull(query.end());
        assertEquals(50, query.limit());
        assertEquals(AlpacaNewsApi.SortOrder.DESC, query.sort());
        assertFalse(query.includeContent());
    }
}
