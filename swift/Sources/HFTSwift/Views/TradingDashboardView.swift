#if canImport(SwiftUI)
import SwiftUI

/// Main Trading Dashboard View
/// Follows Apple Human Interface Guidelines for layout, spacing, and typography
/// Supports iOS, macOS, and visionOS
public struct TradingDashboardView: View {
    
    @StateObject private var engine: ReactiveTradeEngine
    @StateObject private var akkaBridge: AkkaBridge
    @State private var selectedTab: DashboardTab = .orderBook
    
    public init(engine: ReactiveTradeEngine, akkaBridge: AkkaBridge) {
        _engine = StateObject(wrappedValue: engine)
        _akkaBridge = StateObject(wrappedValue: akkaBridge)
    }
    
    public var body: some View {
        #if os(visionOS)
        visionOSLayout
        #else
        standardLayout
        #endif
    }
    
    // MARK: - Standard Layout (iOS/macOS)
    
    private var standardLayout: some View {
        NavigationStack {
            VStack(spacing: 0) {
                // Connection Status Bar
                ConnectionStatusBar(
                    status: engine.connectionStatus,
                    akkaConnected: akkaBridge.isConnected
                )
                
                // Tab Selection
                Picker("View", selection: $selectedTab) {
                    ForEach(DashboardTab.allCases) { tab in
                        Label(tab.title, systemImage: tab.icon)
                            .tag(tab)
                    }
                }
                .pickerStyle(.segmented)
                .padding()
                
                // Content
                TabView(selection: $selectedTab) {
                    OrderBookView(orderBook: engine.orderBook)
                        .tag(DashboardTab.orderBook)
                    
                    RecentTradesView(trades: engine.recentTrades)
                        .tag(DashboardTab.trades)
                    
                    MarketDataView(marketData: engine.marketData)
                        .tag(DashboardTab.market)
                    
                    OrderEntryView(engine: engine)
                        .tag(DashboardTab.entry)
                }
                .tabViewStyle(.page(indexDisplayMode: .never))
            }
            .navigationTitle("HFT Trading System")
            .toolbar {
                ToolbarItem(placement: .primaryAction) {
                    ConnectionButton(
                        isConnected: engine.connectionStatus.isConnected,
                        action: {
                            if engine.connectionStatus.isConnected {
                                engine.disconnect()
                            } else {
                                engine.connect()
                            }
                        }
                    )
                }
            }
        }
    }
    
    // MARK: - visionOS Layout
    
    #if os(visionOS)
    private var visionOSLayout: some View {
        NavigationSplitView {
            List(DashboardTab.allCases) { tab in
                NavigationLink(value: tab) {
                    Label(tab.title, systemImage: tab.icon)
                }
            }
            .navigationTitle("HFT System")
        } detail: {
            ZStack {
                // 3D depth for visionOS
                RoundedRectangle(cornerRadius: 20)
                    .fill(.ultraThinMaterial)
                    .frame(depth: 50)
                
                VStack(spacing: 20) {
                    ConnectionStatusBar(
                        status: engine.connectionStatus,
                        akkaConnected: akkaBridge.isConnected
                    )
                    .padding()
                    
                    switch selectedTab {
                    case .orderBook:
                        OrderBookView(orderBook: engine.orderBook)
                    case .trades:
                        RecentTradesView(trades: engine.recentTrades)
                    case .market:
                        MarketDataView(marketData: engine.marketData)
                    case .entry:
                        OrderEntryView(engine: engine)
                    }
                }
                .padding()
            }
            .frame(depth: 100)
            .navigationTitle(selectedTab.title)
        }
    }
    #endif
}

// MARK: - Dashboard Tabs

enum DashboardTab: String, CaseIterable, Identifiable {
    case orderBook = "Order Book"
    case trades = "Trades"
    case market = "Market"
    case entry = "Entry"
    
    var id: String { rawValue }
    
    var title: String { rawValue }
    
    var icon: String {
        switch self {
        case .orderBook: return "book.fill"
        case .trades: return "chart.line.uptrend.xyaxis"
        case .market: return "chart.bar.fill"
        case .entry: return "square.and.pencil"
        }
    }
}

// MARK: - Connection Status Bar

struct ConnectionStatusBar: View {
    let status: ConnectionStatus
    let akkaConnected: Bool
    
    var body: some View {
        HStack(spacing: 16) {
            // Swift Engine Status
            StatusIndicator(
                title: "Swift Engine",
                isConnected: status.isConnected,
                color: status.isConnected ? .green : .red
            )
            
            Divider()
            
            // Akka Bridge Status
            StatusIndicator(
                title: "Akka Bridge",
                isConnected: akkaConnected,
                color: akkaConnected ? .green : .orange
            )
            
            Spacer()
        }
        .padding(.horizontal)
        .padding(.vertical, 8)
        .background(Color(.systemBackground).opacity(0.9))
        .overlay(
            Rectangle()
                .frame(height: 1)
                .foregroundColor(Color(.separator)),
            alignment: .bottom
        )
    }
}

struct StatusIndicator: View {
    let title: String
    let isConnected: Bool
    let color: Color
    
    var body: some View {
        HStack(spacing: 8) {
            Circle()
                .fill(color)
                .frame(width: 8, height: 8)
            
            Text(title)
                .font(.caption)
                .foregroundColor(.secondary)
            
            Text(isConnected ? "Connected" : "Disconnected")
                .font(.caption.weight(.medium))
        }
    }
}

// MARK: - Connection Button

struct ConnectionButton: View {
    let isConnected: Bool
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Label(
                isConnected ? "Disconnect" : "Connect",
                systemImage: isConnected ? "stop.circle.fill" : "play.circle.fill"
            )
        }
        .buttonStyle(.borderedProminent)
        .tint(isConnected ? .red : .green)
    }
}

// MARK: - Preview

#Preview {
    let engine = ReactiveTradeEngine()
    let bridge = AkkaBridge(engine: engine)
    
    return TradingDashboardView(engine: engine, akkaBridge: bridge)
}
#endif
