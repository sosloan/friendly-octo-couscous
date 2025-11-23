import XCTest
@testable import HFTSwift

final class HFTSwiftTests: XCTestCase {
    
    // MARK: - Order Tests
    
    func testOrderCreation() throws {
        let order = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .buy
        )
        
        XCTAssertEqual(order.id, 1)
        XCTAssertEqual(order.symbol, "AAPL")
        XCTAssertEqual(order.price, 150.50)
        XCTAssertEqual(order.quantity, 100)
        XCTAssertEqual(order.side, .buy)
    }
    
    func testOrderValue() throws {
        let order = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .buy
        )
        
        XCTAssertEqual(order.value, 15050.00)
    }
    
    func testOrderMatching() throws {
        let buyOrder = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .buy
        )
        
        let sellOrder = Order(
            id: 2,
            symbol: "AAPL",
            price: 150.25,
            quantity: 100,
            side: .sell
        )
        
        XCTAssertTrue(buyOrder.canMatch(with: sellOrder))
        XCTAssertTrue(sellOrder.canMatch(with: buyOrder))
    }
    
    func testOrderMatchingDifferentSymbol() throws {
        let buyOrder = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .buy
        )
        
        let sellOrder = Order(
            id: 2,
            symbol: "GOOGL",
            price: 2800.00,
            quantity: 50,
            side: .sell
        )
        
        XCTAssertFalse(buyOrder.canMatch(with: sellOrder))
    }
    
    func testOrderMatchingPriceNotCrossed() throws {
        let buyOrder = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.00,
            quantity: 100,
            side: .buy
        )
        
        let sellOrder = Order(
            id: 2,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .sell
        )
        
        XCTAssertFalse(buyOrder.canMatch(with: sellOrder))
    }
    
    // MARK: - Trade Tests
    
    func testTradeCreation() throws {
        let buyOrder = Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
        let sellOrder = Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell)
        
        let trade = Trade(
            id: 1,
            buyOrder: buyOrder,
            sellOrder: sellOrder,
            executionPrice: 150.25,
            quantity: 100
        )
        
        XCTAssertEqual(trade.id, 1)
        XCTAssertEqual(trade.executionPrice, 150.25)
        XCTAssertEqual(trade.quantity, 100)
        XCTAssertEqual(trade.value, 15025.00)
    }
    
    // MARK: - Order Book Tests
    
    func testOrderBookCreation() throws {
        let orderBook = OrderBook(
            buyOrders: [
                Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
                Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 200, side: .buy)
            ],
            sellOrders: [
                Order(id: 3, symbol: "AAPL", price: 150.75, quantity: 100, side: .sell),
                Order(id: 4, symbol: "AAPL", price: 151.00, quantity: 150, side: .sell)
            ]
        )
        
        XCTAssertEqual(orderBook.buyOrders.count, 2)
        XCTAssertEqual(orderBook.sellOrders.count, 2)
    }
    
    func testOrderBookBestBidAsk() throws {
        let orderBook = OrderBook(
            buyOrders: [
                Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
                Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 200, side: .buy)
            ],
            sellOrders: [
                Order(id: 3, symbol: "AAPL", price: 150.75, quantity: 100, side: .sell),
                Order(id: 4, symbol: "AAPL", price: 151.00, quantity: 150, side: .sell)
            ]
        )
        
        XCTAssertEqual(orderBook.bestBid, 150.50)
        XCTAssertEqual(orderBook.bestAsk, 150.75)
        XCTAssertEqual(orderBook.spread, 0.25)
    }
    
    func testOrderBookSorting() throws {
        let orderBook = OrderBook(
            buyOrders: [
                Order(id: 1, symbol: "AAPL", price: 150.25, quantity: 100, side: .buy),
                Order(id: 2, symbol: "AAPL", price: 150.50, quantity: 200, side: .buy),
                Order(id: 3, symbol: "AAPL", price: 150.00, quantity: 150, side: .buy)
            ],
            sellOrders: [
                Order(id: 4, symbol: "AAPL", price: 151.00, quantity: 100, side: .sell),
                Order(id: 5, symbol: "AAPL", price: 150.75, quantity: 150, side: .sell),
                Order(id: 6, symbol: "AAPL", price: 151.25, quantity: 200, side: .sell)
            ]
        )
        
        // Buy orders should be sorted descending (highest first)
        XCTAssertEqual(orderBook.buyOrders.first?.price, 150.50)
        XCTAssertEqual(orderBook.buyOrders.last?.price, 150.00)
        
        // Sell orders should be sorted ascending (lowest first)
        XCTAssertEqual(orderBook.sellOrders.first?.price, 150.75)
        XCTAssertEqual(orderBook.sellOrders.last?.price, 151.25)
    }
    
    // MARK: - Reactive Engine Tests (Apple platforms only)
    
    #if canImport(Combine)
    func testReactiveEngineInitialization() throws {
        let engine = ReactiveTradeEngine()
        
        XCTAssertEqual(engine.orderBook.buyOrders.count, 0)
        XCTAssertEqual(engine.orderBook.sellOrders.count, 0)
        XCTAssertEqual(engine.recentTrades.count, 0)
        XCTAssertEqual(engine.marketData.count, 0)
    }
    
    func testReactiveEngineConnection() throws {
        let engine = ReactiveTradeEngine()
        
        let expectation = XCTestExpectation(description: "Engine connects")
        
        engine.connect()
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
            XCTAssertTrue(engine.connectionStatus.isConnected)
            expectation.fulfill()
        }
        
        wait(for: [expectation], timeout: 2.0)
    }
    
    func testReactiveEngineOrderSubmission() throws {
        let engine = ReactiveTradeEngine()
        
        let expectation = XCTestExpectation(description: "Order is submitted")
        
        let order = Order(
            id: 1,
            symbol: "AAPL",
            price: 150.50,
            quantity: 100,
            side: .buy
        )
        
        engine.submitOrder(order)
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            XCTAssertEqual(engine.orderBook.buyOrders.count, 1)
            XCTAssertEqual(engine.orderBook.buyOrders.first?.id, 1)
            expectation.fulfill()
        }
        
        wait(for: [expectation], timeout: 1.0)
    }
    
    func testReactiveEngineOrderMatching() throws {
        let engine = ReactiveTradeEngine()
        
        let expectation = XCTestExpectation(description: "Orders match and trade executes")
        
        let buyOrder = Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
        let sellOrder = Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell)
        
        engine.submitOrder(buyOrder)
        engine.submitOrder(sellOrder)
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            XCTAssertEqual(engine.recentTrades.count, 1)
            XCTAssertEqual(engine.orderBook.buyOrders.count, 0)
            XCTAssertEqual(engine.orderBook.sellOrders.count, 0)
            expectation.fulfill()
        }
        
        wait(for: [expectation], timeout: 1.0)
    }
    #endif
}
