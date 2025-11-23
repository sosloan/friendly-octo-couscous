#if canImport(SwiftUI)
import SwiftUI

/// Recent Trades View displaying executed trades
/// Follows HIG with animations and proper data visualization
public struct RecentTradesView: View {
    let trades: [Trade]
    
    public init(trades: [Trade]) {
        self.trades = trades
    }
    
    public var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                // Header
                header
                
                if trades.isEmpty {
                    emptyState
                } else {
                    // Trades List
                    LazyVStack(spacing: 12) {
                        ForEach(trades) { trade in
                            TradeRow(trade: trade)
                                .transition(.asymmetric(
                                    insertion: .move(edge: .top).combined(with: .opacity),
                                    removal: .opacity
                                ))
                        }
                    }
                    .animation(.easeInOut, value: trades.count)
                }
            }
            .padding()
        }
    }
    
    // MARK: - Header
    
    private var header: some View {
        HStack {
            VStack(alignment: .leading, spacing: 4) {
                Text("RECENT TRADES")
                    .font(.headline)
                
                Text("\(trades.count) executed")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
        }
    }
    
    // MARK: - Empty State
    
    private var emptyState: some View {
        VStack(spacing: 16) {
            Image(systemName: "chart.line.uptrend.xyaxis.circle")
                .font(.system(size: 60))
                .foregroundColor(.secondary)
            
            Text("No Trades Yet")
                .font(.title3.weight(.medium))
            
            Text("Executed trades will appear here")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .padding(.top, 60)
    }
}

// MARK: - Trade Row

struct TradeRow: View {
    let trade: Trade
    
    var body: some View {
        VStack(spacing: 12) {
            // Trade header
            HStack {
                // Trade ID
                Label("#\(trade.id)", systemImage: "checkmark.circle.fill")
                    .font(.caption.weight(.semibold))
                    .foregroundColor(.green)
                
                Spacer()
                
                // Timestamp
                Text(formatTimestamp(trade.timestamp))
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Divider()
            
            // Trade details
            HStack(alignment: .top, spacing: 16) {
                // Buy side
                VStack(alignment: .leading, spacing: 4) {
                    Text("BUY")
                        .font(.caption2.weight(.bold))
                        .foregroundColor(.green)
                    
                    Text("Order #\(trade.buyOrder.id)")
                        .font(.caption2)
                        .foregroundColor(.secondary)
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                
                // Execution details
                VStack(spacing: 4) {
                    Text(trade.buyOrder.symbol)
                        .font(.headline.weight(.bold))
                    
                    Text(formatPrice(trade.executionPrice))
                        .font(.title3.weight(.semibold))
                        .foregroundColor(.primary)
                    
                    Text("\(trade.quantity) @ \(formatPrice(trade.executionPrice))")
                        .font(.caption)
                        .foregroundColor(.secondary)
                    
                    Text("Value: \(formatPrice(trade.value))")
                        .font(.caption.weight(.medium))
                        .foregroundColor(.orange)
                }
                .frame(maxWidth: .infinity)
                
                // Sell side
                VStack(alignment: .trailing, spacing: 4) {
                    Text("SELL")
                        .font(.caption2.weight(.bold))
                        .foregroundColor(.red)
                    
                    Text("Order #\(trade.sellOrder.id)")
                        .font(.caption2)
                        .foregroundColor(.secondary)
                }
                .frame(maxWidth: .infinity, alignment: .trailing)
            }
        }
        .padding()
        .background(
            RoundedRectangle(cornerRadius: 12)
                .fill(Color(.secondarySystemBackground))
        )
        .overlay(
            RoundedRectangle(cornerRadius: 12)
                .strokeBorder(Color.green.opacity(0.3), lineWidth: 1)
        )
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
    
    private func formatTimestamp(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.timeStyle = .medium
        formatter.dateStyle = .none
        return formatter.string(from: date)
    }
}

// MARK: - Preview

#Preview {
    let sampleTrades = [
        Trade(
            id: 1,
            buyOrder: Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy),
            sellOrder: Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell),
            executionPrice: 150.25,
            quantity: 100,
            timestamp: Date()
        ),
        Trade(
            id: 2,
            buyOrder: Order(id: 3, symbol: "GOOGL", price: 2800.00, quantity: 50, side: .buy),
            sellOrder: Order(id: 4, symbol: "GOOGL", price: 2795.00, quantity: 50, side: .sell),
            executionPrice: 2795.00,
            quantity: 50,
            timestamp: Date().addingTimeInterval(-60)
        )
    ]
    
    return RecentTradesView(trades: sampleTrades)
}
#endif
