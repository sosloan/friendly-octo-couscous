#if canImport(SwiftUI)
import SwiftUI

/// Market Data View displaying real-time market information
/// Follows HIG with cards and data visualization
public struct MarketDataView: View {
    let marketData: [String: MarketData]
    
    public init(marketData: [String: MarketData]) {
        self.marketData = marketData
    }
    
    public var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                // Header
                header
                
                if marketData.isEmpty {
                    emptyState
                } else {
                    // Market data grid
                    LazyVGrid(columns: columns, spacing: 16) {
                        ForEach(Array(marketData.values).sorted(by: { $0.symbol < $1.symbol })) { data in
                            MarketDataCard(data: data)
                        }
                    }
                }
            }
            .padding()
        }
    }
    
    private var columns: [GridItem] {
        [
            GridItem(.adaptive(minimum: 300, maximum: 400), spacing: 16)
        ]
    }
    
    // MARK: - Header
    
    private var header: some View {
        HStack {
            VStack(alignment: .leading, spacing: 4) {
                Text("MARKET DATA")
                    .font(.headline)
                
                Text("\(marketData.count) symbols")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
        }
    }
    
    // MARK: - Empty State
    
    private var emptyState: some View {
        VStack(spacing: 16) {
            Image(systemName: "chart.bar.fill")
                .font(.system(size: 60))
                .foregroundColor(.secondary)
            
            Text("No Market Data")
                .font(.title3.weight(.medium))
            
            Text("Market data will appear here when available")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .padding(.top, 60)
    }
}

// MARK: - Market Data Card

struct MarketDataCard: View {
    let data: MarketData
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            // Header
            HStack {
                Text(data.symbol)
                    .font(.title2.weight(.bold))
                
                Spacer()
                
                Text(formatTimestamp(data.timestamp))
                    .font(.caption2)
                    .foregroundColor(.secondary)
            }
            
            // Price
            Text(formatPrice(data.lastPrice))
                .font(.system(.title, design: .rounded).weight(.semibold))
                .foregroundColor(.primary)
            
            Divider()
            
            // Statistics
            VStack(spacing: 8) {
                StatRow(label: "Volume", value: formatNumber(data.volume))
                StatRow(label: "High", value: formatPrice(data.high))
                StatRow(label: "Low", value: formatPrice(data.low))
            }
        }
        .padding()
        .background(
            RoundedRectangle(cornerRadius: 16)
                .fill(Color(.secondarySystemBackground))
        )
        .overlay(
            RoundedRectangle(cornerRadius: 16)
                .strokeBorder(Color(.separator), lineWidth: 1)
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
    
    private func formatNumber(_ number: Int64) -> String {
        let formatter = NumberFormatter()
        formatter.numberStyle = .decimal
        return formatter.string(from: NSNumber(value: number)) ?? "0"
    }
    
    private func formatTimestamp(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.timeStyle = .short
        formatter.dateStyle = .none
        return formatter.string(from: date)
    }
}

// MARK: - Stat Row

struct StatRow: View {
    let label: String
    let value: String
    
    var body: some View {
        HStack {
            Text(label)
                .font(.caption)
                .foregroundColor(.secondary)
            
            Spacer()
            
            Text(value)
                .font(.caption.weight(.medium))
        }
    }
}

// MARK: - Preview

#Preview {
    let sampleData: [String: MarketData] = [
        "AAPL": MarketData(
            symbol: "AAPL",
            lastPrice: 150.50,
            volume: 1234567,
            high: 152.00,
            low: 148.50
        ),
        "GOOGL": MarketData(
            symbol: "GOOGL",
            lastPrice: 2800.00,
            volume: 987654,
            high: 2850.00,
            low: 2750.00
        )
    ]
    
    return MarketDataView(marketData: sampleData)
}
#endif
