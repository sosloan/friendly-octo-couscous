import Foundation

#if canImport(Combine)
import Combine

/// Swift Combine-based Reactive Trading Engine
/// This provides the reactive data layer using Combine Publishers and Subscribers
public class ReactiveTradeEngine: ObservableObject {
    
    // MARK: - Published Properties
    
    @Published public private(set) var orderBook: OrderBook = OrderBook()
    @Published public private(set) var recentTrades: [Trade] = []
    @Published public private(set) var marketData: [String: MarketData] = [:]
    @Published public private(set) var connectionStatus: ConnectionStatus = .disconnected
    
    // MARK: - Publishers
    
    public let orderSubmittedPublisher = PassthroughSubject<Order, Never>()
    public let tradeExecutedPublisher = PassthroughSubject<Trade, Never>()
    public let marketDataPublisher = PassthroughSubject<MarketData, Never>()
    public let errorPublisher = PassthroughSubject<TradingError, Never>()
    
    // MARK: - Private Properties
    
    private var cancellables = Set<AnyCancellable>()
    private var tradeIdCounter: Int64 = 1
    private let queue = DispatchQueue(label: "com.hft.reactive.engine", qos: .userInitiated)
    
    // MARK: - Initialization
    
    public init() {
        setupSubscriptions()
    }
    
    // MARK: - Setup
    
    private func setupSubscriptions() {
        // Subscribe to order submissions
        orderSubmittedPublisher
            .receive(on: queue)
            .sink { [weak self] order in
                self?.processOrder(order)
            }
            .store(in: &cancellables)
        
        // Subscribe to market data updates
        marketDataPublisher
            .receive(on: queue)
            .sink { [weak self] data in
                self?.updateMarketData(data)
            }
            .store(in: &cancellables)
        
        // Subscribe to trade executions for recent trades tracking
        tradeExecutedPublisher
            .receive(on: DispatchQueue.main)
            .sink { [weak self] trade in
                self?.addRecentTrade(trade)
            }
            .store(in: &cancellables)
    }
    
    // MARK: - Public Methods
    
    /// Submit a new order to the engine
    public func submitOrder(_ order: Order) {
        orderSubmittedPublisher.send(order)
    }
    
    /// Connect to the trading system
    public func connect() {
        DispatchQueue.main.async { [weak self] in
            self?.connectionStatus = .connecting
        }
        
        // Simulate connection delay using asyncAfter
        queue.asyncAfter(deadline: .now() + 0.5) { [weak self] in
            DispatchQueue.main.async {
                self?.connectionStatus = .connected
            }
        }
    }
    
    /// Disconnect from the trading system
    public func disconnect() {
        DispatchQueue.main.async { [weak self] in
            self?.connectionStatus = .disconnected
        }
    }
    
    // MARK: - Private Methods
    
    private func processOrder(_ order: Order) {
        // Validate order
        guard validateOrder(order) else {
            errorPublisher.send(.invalidOrder(reason: "Invalid order parameters"))
            return
        }
        
        // Add to order book
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            
            switch order.side {
            case .buy:
                self.orderBook.buyOrders.append(order)
                self.orderBook.buyOrders.sort { $0.price > $1.price }
            case .sell:
                self.orderBook.sellOrders.append(order)
                self.orderBook.sellOrders.sort { $0.price < $1.price }
            }
            
            // Try to match orders
            self.attemptMatching()
        }
    }
    
    private func validateOrder(_ order: Order) -> Bool {
        return order.price > 0 && order.quantity > 0 && !order.symbol.isEmpty
    }
    
    private func attemptMatching() {
        guard let buyOrder = orderBook.buyOrders.first,
              let sellOrder = orderBook.sellOrders.first else {
            return
        }
        
        // Check if orders can match
        if buyOrder.symbol == sellOrder.symbol && buyOrder.price >= sellOrder.price {
            // Execute trade
            let executionPrice = sellOrder.price
            let executionQuantity = min(buyOrder.quantity, sellOrder.quantity)
            
            let trade = Trade(
                id: tradeIdCounter,
                buyOrder: buyOrder,
                sellOrder: sellOrder,
                executionPrice: executionPrice,
                quantity: executionQuantity
            )
            
            tradeIdCounter += 1
            
            // Remove matched orders
            orderBook.buyOrders.removeFirst()
            orderBook.sellOrders.removeFirst()
            
            // Publish trade execution
            tradeExecutedPublisher.send(trade)
            
            // Continue matching if more orders exist
            if !orderBook.buyOrders.isEmpty && !orderBook.sellOrders.isEmpty {
                attemptMatching()
            }
        }
    }
    
    private func updateMarketData(_ data: MarketData) {
        DispatchQueue.main.async { [weak self] in
            self?.marketData[data.symbol] = data
        }
    }
    
    private func addRecentTrade(_ trade: Trade) {
        recentTrades.insert(trade, at: 0)
        
        // Keep only last 100 trades
        if recentTrades.count > 100 {
            recentTrades = Array(recentTrades.prefix(100))
        }
    }
}

// MARK: - Supporting Types

public enum ConnectionStatus: Sendable {
    case disconnected
    case connecting
    case connected
    case error(String)
    
    public var isConnected: Bool {
        if case .connected = self {
            return true
        }
        return false
    }
}

public enum TradingError: Error, Sendable {
    case invalidOrder(reason: String)
    case connectionFailed(reason: String)
    case matchingFailed(reason: String)
    case akkaIntegrationError(reason: String)
}
#endif
