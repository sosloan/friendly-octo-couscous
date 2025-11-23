import Foundation

// MARK: - Order Domain Model

/// Order side enumeration matching the Akka/Java models
public enum OrderSide: String, Codable {
    case buy = "BUY"
    case sell = "SELL"
}

/// Order structure representing a trading order
public struct Order: Codable, Identifiable {
    public let id: Int64
    public let symbol: String
    public let price: Decimal
    public let quantity: Int64
    public let side: OrderSide
    public let timestamp: Date
    
    public init(id: Int64, symbol: String, price: Decimal, quantity: Int64, side: OrderSide, timestamp: Date = Date()) {
        self.id = id
        self.symbol = symbol
        self.price = price
        self.quantity = quantity
        self.side = side
        self.timestamp = timestamp
    }
    
    /// Calculate the total value of the order
    public var value: Decimal {
        price * Decimal(quantity)
    }
    
    /// Check if this order can match with another order
    public func canMatch(with other: Order) -> Bool {
        guard symbol == other.symbol else { return false }
        guard side != other.side else { return false }
        
        switch side {
        case .buy:
            return price >= other.price
        case .sell:
            return price <= other.price
        }
    }
}

/// Trade structure representing an executed trade
public struct Trade: Codable, Identifiable {
    public let id: Int64
    public let buyOrder: Order
    public let sellOrder: Order
    public let executionPrice: Decimal
    public let quantity: Int64
    public let timestamp: Date
    
    public init(id: Int64, buyOrder: Order, sellOrder: Order, executionPrice: Decimal, quantity: Int64, timestamp: Date = Date()) {
        self.id = id
        self.buyOrder = buyOrder
        self.sellOrder = sellOrder
        self.executionPrice = executionPrice
        self.quantity = quantity
        self.timestamp = timestamp
    }
    
    /// Calculate the total value of the trade
    public var value: Decimal {
        executionPrice * Decimal(quantity)
    }
}

/// Order book state
public struct OrderBook: Codable {
    public var buyOrders: [Order]
    public var sellOrders: [Order]
    
    public init(buyOrders: [Order] = [], sellOrders: [Order] = []) {
        self.buyOrders = buyOrders.sorted { $0.price > $1.price }
        self.sellOrders = sellOrders.sorted { $0.price < $1.price }
    }
    
    /// Get the best bid price
    public var bestBid: Decimal? {
        buyOrders.first?.price
    }
    
    /// Get the best ask price
    public var bestAsk: Decimal? {
        sellOrders.first?.price
    }
    
    /// Get the spread between bid and ask
    public var spread: Decimal? {
        guard let bid = bestBid, let ask = bestAsk else { return nil }
        return ask - bid
    }
}

/// Market data for a symbol
public struct MarketData: Codable, Identifiable {
    public let symbol: String
    public let lastPrice: Decimal
    public let volume: Int64
    public let high: Decimal
    public let low: Decimal
    public let timestamp: Date
    
    public var id: String { symbol }
    
    public init(symbol: String, lastPrice: Decimal, volume: Int64, high: Decimal, low: Decimal, timestamp: Date = Date()) {
        self.symbol = symbol
        self.lastPrice = lastPrice
        self.volume = volume
        self.high = high
        self.low = low
        self.timestamp = timestamp
    }
}
