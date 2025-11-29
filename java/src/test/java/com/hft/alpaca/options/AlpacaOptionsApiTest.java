package com.hft.alpaca.options;

import com.hft.alpaca.client.AlpacaConfig;
import com.hft.alpaca.model.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Tests for AlpacaOptionsApi.
 */
class AlpacaOptionsApiTest {

    private AlpacaConfig config;
    private AlpacaOptionsApi api;
    
    @BeforeEach
    void setUp() {
        config = AlpacaConfig.paperTrading("test-api-key", "test-secret-key");
        api = new AlpacaOptionsApi(config);
    }
    
    @Test
    @DisplayName("Options API should be created with valid config")
    void testApiCreation() {
        assertNotNull(api);
        assertEquals(config, api.getConfig());
    }
    
    @Test
    @DisplayName("Options API should throw exception for null config")
    void testNullConfigThrows() {
        assertThrows(NullPointerException.class, () -> {
            new AlpacaOptionsApi(null);
        });
    }
    
    @Test
    @DisplayName("OptionsContract should identify call/put correctly")
    void testOptionsContractType() {
        OptionsContract call = new OptionsContract(
            "id-1",
            "AAPL230120C00150000",
            "AAPL 01/20/2023 150.00 Call",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.CALL,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            BigDecimal.valueOf(1000),
            BigDecimal.valueOf(1000),
            BigDecimal.valueOf(5.50),
            LocalDate.now()
        );
        
        assertTrue(call.isCall());
        assertFalse(call.isPut());
        
        OptionsContract put = new OptionsContract(
            "id-2",
            "AAPL230120P00150000",
            "AAPL 01/20/2023 150.00 Put",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.PUT,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            BigDecimal.valueOf(500),
            BigDecimal.valueOf(500),
            BigDecimal.valueOf(3.25),
            LocalDate.now()
        );
        
        assertFalse(put.isCall());
        assertTrue(put.isPut());
    }
    
    @Test
    @DisplayName("OptionsContract should check ITM status correctly")
    void testInTheMoneyStatus() {
        OptionsContract call = new OptionsContract(
            "id-1",
            "AAPL230120C00150000",
            "AAPL Call",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.CALL,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            null, null, null, null
        );
        
        // Call is ITM when underlying > strike
        assertTrue(call.isInTheMoney(BigDecimal.valueOf(160)));
        assertFalse(call.isInTheMoney(BigDecimal.valueOf(140)));
        
        OptionsContract put = new OptionsContract(
            "id-2",
            "AAPL230120P00150000",
            "AAPL Put",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.PUT,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            null, null, null, null
        );
        
        // Put is ITM when underlying < strike
        assertTrue(put.isInTheMoney(BigDecimal.valueOf(140)));
        assertFalse(put.isInTheMoney(BigDecimal.valueOf(160)));
    }
    
    @Test
    @DisplayName("OptionsContract should calculate intrinsic value correctly")
    void testIntrinsicValue() {
        OptionsContract call = new OptionsContract(
            "id-1",
            "AAPL230120C00150000",
            "AAPL Call",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.CALL,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            null, null, null, null
        );
        
        // Call intrinsic value = max(0, underlying - strike)
        assertEquals(BigDecimal.valueOf(10), call.getIntrinsicValue(BigDecimal.valueOf(160)));
        assertEquals(BigDecimal.ZERO, call.getIntrinsicValue(BigDecimal.valueOf(140)));
        
        OptionsContract put = new OptionsContract(
            "id-2",
            "AAPL230120P00150000",
            "AAPL Put",
            "active",
            true,
            "AAPL",
            "aapl-asset-id",
            OptionType.PUT,
            "american",
            BigDecimal.valueOf(150),
            BigDecimal.valueOf(100),
            BigDecimal.valueOf(1),
            LocalDate.of(2023, 1, 20),
            "AAPL",
            null, null, null, null
        );
        
        // Put intrinsic value = max(0, strike - underlying)
        assertEquals(BigDecimal.valueOf(10), put.getIntrinsicValue(BigDecimal.valueOf(140)));
        assertEquals(BigDecimal.ZERO, put.getIntrinsicValue(BigDecimal.valueOf(160)));
    }
}
