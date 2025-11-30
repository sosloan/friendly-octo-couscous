import Foundation
import Distributed
import DistributedCluster

/// Swift 6.2+ Distributed Actors for High-Frequency Trading
/// Enables truly distributed, scalable trading operations across multiple nodes

// MARK: - Distributed Trading Types

/// Distributed order message for cross-node communication
public struct DistributedOrder: Codable, Sendable {
    public let id: Int64
    public let symbol: String
    public let price: Decimal
    public let quantity: Int64
    public let side: OrderSide
    public let timestamp: Date
    public let sourceNode: String
    
    public init(from order: Order, sourceNode: String) {
        self.id = order.id
        self.symbol = order.symbol
        self.price = order.price
        self.quantity = order.quantity
        self.side = order.side
        self.timestamp = order.timestamp
        self.sourceNode = sourceNode
    }
    
    public func toOrder() -> Order {
        Order(id: id, symbol: symbol, price: price, quantity: quantity, side: side, timestamp: timestamp)
    }
}

/// Distributed trade result for cross-node execution
public struct DistributedTradeResult: Codable, Sendable {
    public let tradeId: Int64
    public let buyOrderId: Int64
    public let sellOrderId: Int64
    public let executionPrice: Decimal
    public let quantity: Int64
    public let executorNode: String
    public let timestamp: Date
    
    public init(trade: Trade, executorNode: String) {
        self.tradeId = trade.id
        self.buyOrderId = trade.buyOrder.id
        self.sellOrderId = trade.sellOrder.id
        self.executionPrice = trade.executionPrice
        self.quantity = trade.quantity
        self.executorNode = executorNode
        self.timestamp = trade.timestamp
    }
}

/// Market data broadcast for distributed nodes
public struct DistributedMarketData: Codable, Sendable {
    public let symbol: String
    public let lastPrice: Decimal
    public let volume: Int64
    public let high: Decimal
    public let low: Decimal
    public let timestamp: Date
    public let broadcastNode: String
    
    public init(from marketData: MarketData, broadcastNode: String) {
        self.symbol = marketData.symbol
        self.lastPrice = marketData.lastPrice
        self.volume = marketData.volume
        self.high = marketData.high
        self.low = marketData.low
        self.timestamp = marketData.timestamp
        self.broadcastNode = broadcastNode
    }
}

// MARK: - Distributed Order Router Actor

/// Distributed actor for routing orders across trading nodes
/// This actor handles order submission, routing, and load balancing
public distributed actor DistributedOrderRouter {
    public typealias ActorSystem = ClusterSystem
    
    private var orderBook: OrderBook = OrderBook()
    private var pendingOrders: [Int64: DistributedOrder] = [:]
    private var tradeIdCounter: Int64 = 1
    private let nodeName: String
    
    public init(actorSystem: ActorSystem, nodeName: String) async {
        self.actorSystem = actorSystem
        self.nodeName = nodeName
    }
    
    /// Submit an order to the distributed trading system
    public distributed func submitOrder(_ order: DistributedOrder) async throws -> Bool {
        // Validate order before processing
        guard isValidOrder(order) else {
            throw DistributedTradingError.invalidOrder(reason: "Order validation failed")
        }
        
        // Store pending order
        pendingOrders[order.id] = order
        
        // Add to local order book
        let localOrder = order.toOrder()
        switch order.side {
        case .buy:
            orderBook.buyOrders.append(localOrder)
            orderBook.buyOrders.sort { $0.price > $1.price }
        case .sell:
            orderBook.sellOrders.append(localOrder)
            orderBook.sellOrders.sort { $0.price < $1.price }
        }
        
        return true
    }
    
    /// Attempt to match orders across the distributed system
    public distributed func matchOrders() async throws -> [DistributedTradeResult] {
        var executedTrades: [DistributedTradeResult] = []
        
        while let buyOrder = orderBook.buyOrders.first,
              let sellOrder = orderBook.sellOrders.first,
              buyOrder.symbol == sellOrder.symbol && buyOrder.price >= sellOrder.price {
            
            let executionPrice = sellOrder.price
            let executionQuantity = min(buyOrder.quantity, sellOrder.quantity)
            
            let trade = Trade(
                id: tradeIdCounter,
                buyOrder: buyOrder,
                sellOrder: sellOrder,
                executionPrice: executionPrice,
                quantity: executionQuantity
            )
            
            let distributedResult = DistributedTradeResult(trade: trade, executorNode: nodeName)
            executedTrades.append(distributedResult)
            
            tradeIdCounter += 1
            
            // Remove matched orders
            orderBook.buyOrders.removeFirst()
            orderBook.sellOrders.removeFirst()
            
            // Remove from pending
            pendingOrders.removeValue(forKey: buyOrder.id)
            pendingOrders.removeValue(forKey: sellOrder.id)
        }
        
        return executedTrades
    }
    
    /// Get the current order book state
    public distributed func getOrderBook() async -> OrderBook {
        return orderBook
    }
    
    /// Get pending orders count
    public distributed func getPendingOrdersCount() async -> Int {
        return pendingOrders.count
    }
    
    /// Cancel an order
    public distributed func cancelOrder(orderId: Int64) async throws -> Bool {
        guard pendingOrders[orderId] != nil else {
            throw DistributedTradingError.orderNotFound(id: orderId)
        }
        
        pendingOrders.removeValue(forKey: orderId)
        orderBook.buyOrders.removeAll { $0.id == orderId }
        orderBook.sellOrders.removeAll { $0.id == orderId }
        
        return true
    }
    
    private func isValidOrder(_ order: DistributedOrder) -> Bool {
        return order.price > 0 && order.quantity > 0 && !order.symbol.isEmpty
    }
}

// MARK: - Distributed Market Data Actor

/// Distributed actor for broadcasting and receiving market data
public distributed actor DistributedMarketDataProvider {
    public typealias ActorSystem = ClusterSystem
    
    private var marketData: [String: DistributedMarketData] = [:]
    private var subscribers: [String: Set<ClusterSystem.ActorID>] = [:]
    private let nodeName: String
    
    public init(actorSystem: ActorSystem, nodeName: String) async {
        self.actorSystem = actorSystem
        self.nodeName = nodeName
    }
    
    /// Update market data for a symbol
    public distributed func updateMarketData(_ data: DistributedMarketData) async {
        marketData[data.symbol] = data
    }
    
    /// Get market data for a specific symbol
    public distributed func getMarketData(for symbol: String) async -> DistributedMarketData? {
        return marketData[symbol]
    }
    
    /// Get all market data
    public distributed func getAllMarketData() async -> [String: DistributedMarketData] {
        return marketData
    }
    
    /// Subscribe to market data updates for a symbol
    public distributed func subscribe(actorId: ClusterSystem.ActorID, to symbol: String) async {
        if subscribers[symbol] == nil {
            subscribers[symbol] = []
        }
        subscribers[symbol]?.insert(actorId)
    }
    
    /// Unsubscribe from market data updates
    public distributed func unsubscribe(actorId: ClusterSystem.ActorID, from symbol: String) async {
        subscribers[symbol]?.remove(actorId)
    }
}

// MARK: - Distributed Compliance Actor

/// Distributed actor for compliance checking across trading nodes
public distributed actor DistributedComplianceChecker {
    public typealias ActorSystem = ClusterSystem
    
    private let nilChecker: NILComplianceChecker
    private let auditLogger: DefaultAuditLogger
    private let nodeName: String
    private var violationsCount: Int = 0
    
    public init(actorSystem: ActorSystem, nodeName: String) async {
        self.actorSystem = actorSystem
        self.nodeName = nodeName
        self.nilChecker = NILComplianceChecker()
        self.auditLogger = DefaultAuditLogger()
    }
    
    /// Check compliance for a distributed order
    public distributed func checkCompliance(for order: DistributedOrder) async throws -> ComplianceCheckResult {
        // Check NIL compliance
        let nilResult = nilChecker.checkCompliance(symbol: order.symbol)
        
        var isCompliant = true
        var violations: [String] = []
        
        switch nilResult {
        case .approved:
            break
        case .restricted(let reasons):
            violations.append(contentsOf: reasons)
        case .prohibited(let reason):
            isCompliant = false
            violations.append(reason)
            violationsCount += 1
        case .unknown:
            violations.append("Unknown instrument: \(order.symbol)")
        }
        
        // Log audit event
        if !isCompliant {
            auditLogger.log(AuditEvent(
                eventType: .complianceViolation,
                severity: .error,
                details: [
                    "orderId": String(order.id),
                    "symbol": order.symbol,
                    "violations": violations.joined(separator: ", "),
                    "checkerNode": nodeName
                ]
            ))
        }
        
        return ComplianceCheckResult(
            isCompliant: isCompliant,
            violations: violations,
            checkedAt: Date(),
            checkerNode: nodeName
        )
    }
    
    /// Get total violations count
    public distributed func getViolationsCount() async -> Int {
        return violationsCount
    }
    
    /// Reset violations counter
    public distributed func resetViolationsCount() async {
        violationsCount = 0
    }
}

/// Result of a compliance check
public struct ComplianceCheckResult: Codable, Sendable {
    public let isCompliant: Bool
    public let violations: [String]
    public let checkedAt: Date
    public let checkerNode: String
}

// MARK: - Distributed Trading Coordinator

/// Coordinator for managing distributed trading actors
public distributed actor DistributedTradingCoordinator {
    public typealias ActorSystem = ClusterSystem
    
    private var orderRouters: [String: DistributedOrderRouter] = [:]
    private var marketDataProviders: [String: DistributedMarketDataProvider] = [:]
    private var complianceCheckers: [String: DistributedComplianceChecker] = [:]
    private let nodeName: String
    private var totalOrdersProcessed: Int64 = 0
    private var totalTradesExecuted: Int64 = 0
    
    public init(actorSystem: ActorSystem, nodeName: String) async {
        self.actorSystem = actorSystem
        self.nodeName = nodeName
    }
    
    /// Register an order router with the coordinator
    public distributed func registerOrderRouter(_ router: DistributedOrderRouter, nodeName: String) async {
        orderRouters[nodeName] = router
    }
    
    /// Register a market data provider
    public distributed func registerMarketDataProvider(_ provider: DistributedMarketDataProvider, nodeName: String) async {
        marketDataProviders[nodeName] = provider
    }
    
    /// Register a compliance checker
    public distributed func registerComplianceChecker(_ checker: DistributedComplianceChecker, nodeName: String) async {
        complianceCheckers[nodeName] = checker
    }
    
    /// Submit order with automatic load balancing
    public distributed func submitOrder(_ order: Order) async throws -> OrderSubmissionResult {
        let distributedOrder = DistributedOrder(from: order, sourceNode: nodeName)
        
        // First, check compliance
        guard let checker = complianceCheckers.values.first else {
            throw DistributedTradingError.noComplianceChecker
        }
        
        let complianceResult = try await checker.checkCompliance(for: distributedOrder)
        guard complianceResult.isCompliant else {
            throw DistributedTradingError.complianceViolation(violations: complianceResult.violations)
        }
        
        // Route to appropriate order router (simple round-robin for now)
        guard let router = orderRouters.values.first else {
            throw DistributedTradingError.noOrderRouter
        }
        
        let success = try await router.submitOrder(distributedOrder)
        
        if success {
            totalOrdersProcessed += 1
        }
        
        return OrderSubmissionResult(
            orderId: order.id,
            accepted: success,
            routedToNode: nodeName,
            timestamp: Date()
        )
    }
    
    /// Execute matching across all nodes
    public distributed func executeMatching() async throws -> [DistributedTradeResult] {
        var allTrades: [DistributedTradeResult] = []
        
        for router in orderRouters.values {
            let trades = try await router.matchOrders()
            allTrades.append(contentsOf: trades)
        }
        
        totalTradesExecuted += Int64(allTrades.count)
        
        return allTrades
    }
    
    /// Get cluster statistics
    public distributed func getClusterStatistics() async -> ClusterStatistics {
        return ClusterStatistics(
            totalNodes: orderRouters.count,
            totalOrdersProcessed: totalOrdersProcessed,
            totalTradesExecuted: totalTradesExecuted,
            coordinatorNode: nodeName,
            timestamp: Date()
        )
    }
}

/// Result of order submission
public struct OrderSubmissionResult: Codable, Sendable {
    public let orderId: Int64
    public let accepted: Bool
    public let routedToNode: String
    public let timestamp: Date
}

/// Cluster statistics
public struct ClusterStatistics: Codable, Sendable {
    public let totalNodes: Int
    public let totalOrdersProcessed: Int64
    public let totalTradesExecuted: Int64
    public let coordinatorNode: String
    public let timestamp: Date
}

// MARK: - Distributed Trading Errors

/// Errors that can occur in distributed trading operations
public enum DistributedTradingError: Error, Sendable {
    case invalidOrder(reason: String)
    case orderNotFound(id: Int64)
    case complianceViolation(violations: [String])
    case noOrderRouter
    case noComplianceChecker
    case clusterNotAvailable
    case nodeNotReachable(node: String)
}

// MARK: - Cluster Configuration

/// Configuration for the distributed trading cluster
public struct DistributedTradingClusterConfig: Sendable {
    public let clusterName: String
    public let nodeName: String
    public let host: String
    public let port: Int
    public let seedNodes: [String]
    
    public init(
        clusterName: String = "HFTTradingCluster",
        nodeName: String = "node-1",
        host: String = "127.0.0.1",
        port: Int = 7337,
        seedNodes: [String] = []
    ) {
        self.clusterName = clusterName
        self.nodeName = nodeName
        self.host = host
        self.port = port
        self.seedNodes = seedNodes
    }
}

// MARK: - Distributed Trading System Factory

/// Factory for creating distributed trading system components
public struct DistributedTradingSystemFactory {
    
    /// Create a new cluster system for trading
    public static func createClusterSystem(config: DistributedTradingClusterConfig) async throws -> ClusterSystem {
        let system = await ClusterSystem(config.nodeName) { settings in
            settings.bindHost = config.host
            settings.bindPort = config.port
        }
        
        return system
    }
    
    /// Create a full distributed trading node with all components
    public static func createTradingNode(
        clusterSystem: ClusterSystem,
        nodeName: String
    ) async throws -> DistributedTradingNode {
        // Create all distributed actors
        let orderRouter = await DistributedOrderRouter(
            actorSystem: clusterSystem,
            nodeName: nodeName
        )
        
        let marketDataProvider = await DistributedMarketDataProvider(
            actorSystem: clusterSystem,
            nodeName: nodeName
        )
        
        let complianceChecker = await DistributedComplianceChecker(
            actorSystem: clusterSystem,
            nodeName: nodeName
        )
        
        let coordinator = await DistributedTradingCoordinator(
            actorSystem: clusterSystem,
            nodeName: nodeName
        )
        
        // Register components with coordinator
        try await coordinator.registerOrderRouter(orderRouter, nodeName: nodeName)
        try await coordinator.registerMarketDataProvider(marketDataProvider, nodeName: nodeName)
        try await coordinator.registerComplianceChecker(complianceChecker, nodeName: nodeName)
        
        return DistributedTradingNode(
            nodeName: nodeName,
            clusterSystem: clusterSystem,
            orderRouter: orderRouter,
            marketDataProvider: marketDataProvider,
            complianceChecker: complianceChecker,
            coordinator: coordinator
        )
    }
}

/// A complete distributed trading node
public struct DistributedTradingNode: Sendable {
    public let nodeName: String
    public let clusterSystem: ClusterSystem
    public let orderRouter: DistributedOrderRouter
    public let marketDataProvider: DistributedMarketDataProvider
    public let complianceChecker: DistributedComplianceChecker
    public let coordinator: DistributedTradingCoordinator
    
    /// Submit an order through the distributed system
    public func submitOrder(_ order: Order) async throws -> OrderSubmissionResult {
        return try await coordinator.submitOrder(order)
    }
    
    /// Execute order matching
    public func executeMatching() async throws -> [DistributedTradeResult] {
        return try await coordinator.executeMatching()
    }
    
    /// Get cluster statistics
    public func getStatistics() async throws -> ClusterStatistics {
        return try await coordinator.getClusterStatistics()
    }
    
    /// Update market data
    public func updateMarketData(_ marketData: MarketData) async throws {
        let distributedData = DistributedMarketData(from: marketData, broadcastNode: nodeName)
        try await marketDataProvider.updateMarketData(distributedData)
    }
    
    /// Get market data for a symbol
    public func getMarketData(for symbol: String) async throws -> DistributedMarketData? {
        return try await marketDataProvider.getMarketData(for: symbol)
    }
}
