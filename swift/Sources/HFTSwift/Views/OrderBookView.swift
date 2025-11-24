#if canImport(SwiftUI)
import SwiftUI

/// Order Book View displaying buy and sell orders
/// Follows HIG with proper spacing, colors, and accessibility
public struct OrderBookView: View {
    let orderBook: OrderBook
    
    public init(orderBook: OrderBook) {
        self.orderBook = orderBook
    }
    
    public var body: some View {
        ScrollView {
            VStack(spacing: 20) {
                // Market Summary
                marketSummary
                
                Divider()
                
                // Orders Grid
                HStack(alignment: .top, spacing: 16) {
                    // Buy Orders
                    VStack(alignment: .leading, spacing: 8) {
                        Text("BUY ORDERS")
                            .font(.caption.weight(.semibold))
                            .foregroundColor(.secondary)
                        
                        if orderBook.buyOrders.isEmpty {
                            emptyState(message: "No buy orders")
                        } else {
                            ForEach(orderBook.buyOrders.prefix(10)) { order in
                                OrderRow(order: order, isBuy: true)
                            }
                        }
                    }
                    .frame(maxWidth: .infinity)
                    
                    Divider()
                    
                    // Sell Orders
                    VStack(alignment: .leading, spacing: 8) {
                        Text("SELL ORDERS")
                            .font(.caption.weight(.semibold))
                            .foregroundColor(.secondary)
                        
                        if orderBook.sellOrders.isEmpty {
                            emptyState(message: "No sell orders")
                        } else {
                            ForEach(orderBook.sellOrders.prefix(10)) { order in
                                OrderRow(order: order, isBuy: false)
                            }
                        }
                    }
                    .frame(maxWidth: .infinity)
                }
            }
            .padding()
        }
    }
    
    // MARK: - Market Summary
    
    private var marketSummary: some View {
        VStack(spacing: 12) {
            Text("MARKET DEPTH")
                .font(.headline)
            
            HStack(spacing: 32) {
                // Best Bid
                VStack(spacing: 4) {
                    Text("Best Bid")
                        .font(.caption)
                        .foregroundColor(.secondary)
                    
                    if let bid = orderBook.bestBid {
                        Text(formatPrice(bid))
                            .font(.title2.weight(.bold))
                            .foregroundColor(.green)
                    } else {
                        Text("--")
                            .font(.title2.weight(.bold))
                            .foregroundColor(.secondary)
                    }
                }
                
                // Spread
                VStack(spacing: 4) {
                    Text("Spread")
                        .font(.caption)
                        .foregroundColor(.secondary)
                    
                    if let spread = orderBook.spread {
                        Text(formatPrice(spread))
                            .font(.title3.weight(.semibold))
                            .foregroundColor(.orange)
                    } else {
                        Text("--")
                            .font(.title3.weight(.semibold))
                            .foregroundColor(.secondary)
                    }
                }
                
                // Best Ask
                VStack(spacing: 4) {
                    Text("Best Ask")
                        .font(.caption)
                        .foregroundColor(.secondary)
                    
                    if let ask = orderBook.bestAsk {
                        Text(formatPrice(ask))
                            .font(.title2.weight(.bold))
                            .foregroundColor(.red)
                    } else {
                        Text("--")
                            .font(.title2.weight(.bold))
                            .foregroundColor(.secondary)
                    }
                }
            }
        }
        .padding()
        .background(
            RoundedRectangle(cornerRadius: 12)
                .fill(Color(.secondarySystemBackground))
        )
    }
    
    // MARK: - Empty State
    
    private func emptyState(message: String) -> some View {
        Text(message)
            .font(.caption)
            .foregroundColor(.secondary)
            .italic()
            .frame(maxWidth: .infinity)
            .padding(.vertical, 8)
    }
    
    // MARK: - Formatting
    
    private func formatPrice(_ price: Decimal) -> String {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        formatter.currencySymbol = "$"
        formatter.minimumFractionDigits = 2
        formatter.maximumFractionDigits = 2
        return formatter.string(from: price as NSDecimalNumber) ?? "$0.00"
    }
}

// MARK: - Order Row

struct OrderRow: View {
    let order: Order
    let isBuy: Bool
    
    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 4) {
                // Price
                Text(formatPrice(order.price))
                    .font(.system(.body, design: .monospaced).weight(.semibold))
                    .foregroundColor(isBuy ? .green : .red)
                
                // Quantity
                Text("\(order.quantity) shares")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
            
            // Order ID badge
            Text("#\(order.id)")
                .font(.caption2.weight(.medium))
                .padding(.horizontal, 8)
                .padding(.vertical, 4)
                .background(
                    Capsule()
                        .fill(Color(.tertiarySystemBackground))
                )
        }
        .padding(.vertical, 8)
        .padding(.horizontal, 12)
        .background(
            RoundedRectangle(cornerRadius: 8)
                .fill(isBuy ? Color.green.opacity(0.05) : Color.red.opacity(0.05))
        )
    }
    
    private func formatPrice(_ price: Decimal) -> String {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        formatter.currencySymbol = "$"
        formatter.minimumFractionDigits = 2
        formatter.maximumFractionDigits = 2
        return formatter.string(from: price as NSDecimalNumber) ?? "$0.00"
    }
}

// MARK: - Preview

#Preview {
    let sampleOrderBook = OrderBook(
        buyOrders: [
            Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
            Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 200, side: .buy),
            Order(id: 3, symbol: "AAPL", price: 150.00, quantity: 150, side: .buy)
        ],
        sellOrders: [
            Order(id: 4, symbol: "AAPL", price: 150.75, quantity: 100, side: .sell),
            Order(id: 5, symbol: "AAPL", price: 151.00, quantity: 300, side: .sell),
            Order(id: 6, symbol: "AAPL", price: 151.25, quantity: 250, side: .sell)
        ]
    )
    
    return OrderBookView(orderBook: sampleOrderBook)
}
#endif
