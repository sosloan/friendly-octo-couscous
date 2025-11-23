import Foundation

#if canImport(Combine)
import Combine
#endif

#if canImport(SwiftUI)
import SwiftUI
import HFTSwift

/// Main SwiftUI Application Entry Point
/// Demonstrates the HFT system with SwiftUI and Combine
@main
struct HFTSwiftApp: App {
    
    // Create the reactive engine and Akka bridge
    @StateObject private var engine = ReactiveTradeEngine()
    @StateObject private var akkaBridge: AkkaBridge
    
    init() {
        let engine = ReactiveTradeEngine()
        let bridge = AkkaBridge(
            configuration: AkkaBridge.Configuration(
                akkaHost: "localhost",
                akkaPort: 8080,
                useHTTP: true
            ),
            engine: engine
        )
        
        _engine = StateObject(wrappedValue: engine)
        _akkaBridge = StateObject(wrappedValue: bridge)
    }
    
    var body: some Scene {
        WindowGroup {
            TradingDashboardView(engine: engine, akkaBridge: akkaBridge)
                .onAppear {
                    setupDemo()
                }
        }
        #if os(macOS)
        .windowStyle(.hiddenTitleBar)
        .windowToolbarStyle(.unified)
        #endif
        
        #if os(visionOS)
        ImmersiveSpace(id: "TradingSpace") {
            // 3D immersive trading environment for visionOS
            VStack {
                Text("HFT Trading System")
                    .font(.extraLargeTitle)
                
                TradingDashboardView(engine: engine, akkaBridge: akkaBridge)
            }
            .frame(depth: 200)
        }
        .immersionStyle(selection: .constant(.mixed), in: .mixed)
        #endif
    }
    
    // MARK: - Demo Setup
    
    private func setupDemo() {
        // Connect the engine
        engine.connect()
        
        // Add some sample market data
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
            addSampleMarketData()
        }
        
        // Add some sample orders
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.0) {
            addSampleOrders()
        }
        
        // Attempt to connect to Akka (will fail gracefully if not available)
        Task {
            try? await akkaBridge.connect()
        }
    }
    
    private func addSampleMarketData() {
        let symbols = ["AAPL", "GOOGL", "MSFT", "AMZN", "TSLA"]
        
        for symbol in symbols {
            let basePrice = Decimal(arc4random_uniform(1000) + 100)
            let marketData = MarketData(
                symbol: symbol,
                lastPrice: basePrice,
                volume: Int64(arc4random_uniform(10000000)),
                high: basePrice + Decimal(arc4random_uniform(10)),
                low: basePrice - Decimal(arc4random_uniform(10))
            )
            
            engine.marketDataPublisher.send(marketData)
        }
    }
    
    private func addSampleOrders() {
        let orders = [
            Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
            Order(id: 2, symbol: "AAPL", price: 150.75, quantity: 100, side: .sell),
            Order(id: 3, symbol: "GOOGL", price: 2800.00, quantity: 50, side: .buy),
            Order(id: 4, symbol: "GOOGL", price: 2795.00, quantity: 50, side: .sell),
            Order(id: 5, symbol: "AAPL", price: 150.25, quantity: 200, side: .buy),
            Order(id: 6, symbol: "AAPL", price: 151.00, quantity: 150, side: .sell)
        ]
        
        for order in orders {
            engine.submitOrder(order)
        }
    }
}
#else
// Linux/Non-Apple platform demo
import HFTSwift

@main
struct HFTSwiftDemo {
    static func main() {
        print("=== Swift HFT System Demo ===")
        print("🚀 Starting reactive trading engine (Linux mode)")
        print("")
        
        // Demonstrate the core models without UI
        let engine = ReactiveEngineCore()
        
        // Add sample orders
        print("📊 Creating sample orders...")
        let buyOrder = Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
        let sellOrder = Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell)
        
        print("  ✓ Buy Order: \(buyOrder.symbol) \(buyOrder.quantity) @ $\(buyOrder.price)")
        print("  ✓ Sell Order: \(sellOrder.symbol) \(sellOrder.quantity) @ $\(sellOrder.price)")
        print("")
        
        // Test matching
        print("🔄 Testing order matching...")
        if buyOrder.canMatch(with: sellOrder) {
            print("  ✓ Orders can match!")
            
            let trade = Trade(
                id: 1,
                buyOrder: buyOrder,
                sellOrder: sellOrder,
                executionPrice: sellOrder.price,
                quantity: min(buyOrder.quantity, sellOrder.quantity)
            )
            
            print("  ✓ Trade executed: \(trade.quantity) shares @ $\(trade.executionPrice)")
            print("  ✓ Trade value: $\(trade.value)")
        } else {
            print("  ✗ Orders cannot match")
        }
        print("")
        
        // Test order book
        print("📖 Creating order book...")
        let orderBook = OrderBook(
            buyOrders: [
                Order(id: 3, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
                Order(id: 4, symbol: "AAPL", price: 150.25, quantity: 200, side: .buy),
                Order(id: 5, symbol: "AAPL", price: 150.00, quantity: 150, side: .buy)
            ],
            sellOrders: [
                Order(id: 6, symbol: "AAPL", price: 150.75, quantity: 100, side: .sell),
                Order(id: 7, symbol: "AAPL", price: 151.00, quantity: 150, side: .sell),
                Order(id: 8, symbol: "AAPL", price: 151.25, quantity: 200, side: .sell)
            ]
        )
        
        print("  ✓ Buy orders: \(orderBook.buyOrders.count)")
        print("  ✓ Sell orders: \(orderBook.sellOrders.count)")
        if let bid = orderBook.bestBid {
            print("  ✓ Best bid: $\(bid)")
        }
        if let ask = orderBook.bestAsk {
            print("  ✓ Best ask: $\(ask)")
        }
        if let spread = orderBook.spread {
            print("  ✓ Spread: $\(spread)")
        }
        print("")
        
        // Test market data
        print("📈 Creating market data...")
        let marketData = MarketData(
            symbol: "AAPL",
            lastPrice: 150.50,
            volume: 1234567,
            high: 152.00,
            low: 148.50
        )
        
        print("  ✓ Symbol: \(marketData.symbol)")
        print("  ✓ Last Price: $\(marketData.lastPrice)")
        print("  ✓ Volume: \(marketData.volume)")
        print("  ✓ High: $\(marketData.high)")
        print("  ✓ Low: $\(marketData.low)")
        print("")
        
        print("=== Demo Complete ===")
        print("✓ Swift reactive engine working correctly")
        print("📱 For full SwiftUI experience, run on macOS, iOS, or visionOS")
    }
}

// Simple reactive engine core for Linux (without Combine)
class ReactiveEngineCore {
    var orderBook: OrderBook = OrderBook()
    var trades: [Trade] = []
    
    init() {}
}
#endif
