# Swift Integration Guide

## Overview

The Swift component of the HFT system provides a modern, reactive user interface using **SwiftUI** and **Swift Combine** for reactive programming. This implementation follows Apple's Human Interface Guidelines (HIG) and supports iOS, macOS, iPadOS, and visionOS platforms.

**✅ FULLY AUDITED & CERTIFIED**: This implementation includes comprehensive audit compliance with 77 audit tests covering 20 audit categories, NIL compliance validation, Vision-inspired benchmarks, and Merkle tree cryptographic verification.

## Architecture

### Core Components

1. **Swift Combine Reactive Engine** (`ReactiveEngine.swift`)
   - Reactive data layer using Combine Publishers and Subscribers
   - Observable object for real-time UI updates
   - Order processing and matching logic
   - Trade execution tracking

2. **Audit Compliance Framework** (`AuditCompliance.swift`)
   - Comprehensive audit logging with immutable event tracking
   - 20 compliance rule categories
   - Real-time compliance validation
   - Regulatory reporting (SEC, FINRA, MiFID II)

3. **Merkle Tree Compliance** (`MerkleTreeCompliance.swift`)
   - Cryptographic verification with Merkle trees
   - Proof generation and verification
   - Audit trail integrity with tamper detection
   - Order/trade verification

4. **NIL Compliance** (`NILCompliance.swift`)
   - National Instrument List validation
   - Multi-jurisdiction support
   - Real-time compliance checking
   - Status tracking (Approved/Restricted/Prohibited)

5. **Akka Bridge** (`AkkaBridge.swift`)
   - Integration with the Scala/Akka reactive system
   - HTTP/TCP communication layer
   - Asynchronous message passing
   - Error handling and connection management

6. **SwiftUI Views** (`Views/`)
   - `TradingDashboardView`: Main dashboard with tab navigation
   - `OrderBookView`: Real-time order book visualization
   - `RecentTradesView`: Trade execution history
   - `MarketDataView`: Market data cards
   - `OrderEntryView`: Order submission form

7. **Domain Models** (`Models.swift`)
   - `Order`: Trading order representation
   - `Trade`: Executed trade
   - `OrderBook`: Buy/sell order collections
   - `MarketData`: Market statistics

## Merkle Tree Compliance

### Cryptographic Verification

The Swift implementation includes comprehensive Merkle tree support for data integrity:

```swift
// Create Merkle tree from orders
let orderTree = OrderMerkleTree()
let rootHash = orderTree.buildFromOrders(orders)

// Generate proof for specific order
if let proof = orderTree.generateProof(for: index) {
    let isValid = orderTree.verify(proof: proof)
}

// Compliance tree for audit events
let complianceTree = ComplianceMerkleTree()
let merkleRoot = complianceTree.addAuditBatch(auditEvents)

// Export with verification
let (events, root) = logger.exportWithMerkleVerification(
    from: startDate, 
    to: endDate
)
```

**Features:**
- SHA256/SHA512 hash algorithms
- Proof generation and verification
- Tamper-evident audit logs
- Order/trade verification
- Compliance report generation

**Performance Benchmarking:**
```swift
let benchmarker = MerkleTreeBenchmarker()
let results = benchmarker.runAllBenchmarks()
let report = benchmarker.generateReport()
```

See [MERKLE_TREE_SUMMARY.md](MERKLE_TREE_SUMMARY.md) for detailed documentation.

## Audit Compliance

### Comprehensive Audit Framework

The Swift implementation includes a complete audit compliance system with **77 audit tests** across **20 audit categories**:

#### Audit Categories

1. **Order Validation Chain** - Order integrity, overflow detection, validation
2. **Portfolio Risk Validation** - Portfolio state, margin consistency
3. **Multi-Asset Exposure Limits** - Delta, gamma, vega limits
4. **Position Limit Enforcement** - Position increase/decrease validation
5. **Margin Requirement Cascade** - Margin sufficiency checks
6. **Greeks Boundary Integration** - Risk profile boundaries
7. **Intraday Risk Limits** - Drawdown, volatility, correlation monitoring
8. **End-of-Day Reconciliation** - Position, P&L, margin reconciliation
9. **Cross-Exchange Validation** - Multi-exchange sync, settlement
10. **Algorithm Compliance** - Rate limiting, quote stuffing prevention
11. **Real-Time Monitoring** - Tick-by-tick, latency tracking
12. **Stress Scenario Suite** - Leverage, volatility, illiquidity testing
13. **Recovery from Failure** - Checkpoint rollback, transaction replay
14. **Concurrent Trade Execution** - Thread safety, atomicity
15. **Settlement Validation** - T+0 settlement, DVP matching
16. **Fee Calculation Audit** - Commission, exchange fees, rebates
17. **Tax Lot Tracking** - FIFO, cost basis, wash sale detection
18. **Regulatory Reporting** - SEC, FINRA, MiFID II compliance
19. **Audit Trail Integrity** - Immutable logging, hash verification
20. **System Failover** - Hot standby, heartbeat monitoring

### Running Audit Tests

```bash
# Run all audit tests
make audit-swift

# Run all tests including audit
cd swift && swift test

# Generate audit report
cd swift && swift audit-report.swift
```

### Audit Event Logging

```swift
// Create audit logger
let auditLogger = DefaultAuditLogger()

// Log audit event
auditLogger.log(AuditEvent(
    eventType: .orderSubmitted,
    severity: .info,
    userId: "user123",
    details: ["orderId": "12345", "symbol": "AAPL"]
))

// Query audit trail
let events = auditLogger.query(
    from: Date().addingTimeInterval(-3600),
    to: Date(),
    eventType: .orderSubmitted
)

// Export audit log
let auditData = auditLogger.exportAuditLog(
    from: startDate,
    to: endDate
)
```

### Compliance Validation

```swift
// Create compliance checker
let complianceChecker = ComplianceChecker(auditLogger: auditLogger)

// Validate order
let results = complianceChecker.validateOrder(order)

// Check for violations
if complianceChecker.hasViolations(results) {
    // Handle compliance violation
}

// Submit order with audit
engine.submitOrderWithAudit(
    order,
    auditLogger: auditLogger,
    complianceChecker: complianceChecker,
    userId: "trader123",
    sessionId: "session456"
)
```

## Features

### Reactive Programming with Combine

The system uses Swift Combine for reactive data flow:

```swift
// Publishers for event streams
public let orderSubmittedPublisher = PassthroughSubject<Order, Never>()
public let tradeExecutedPublisher = PassthroughSubject<Trade, Never>()
public let marketDataPublisher = PassthroughSubject<MarketData, Never>()

// Published properties for UI binding
@Published public private(set) var orderBook: OrderBook
@Published public private(set) var recentTrades: [Trade]
@Published public private(set) var marketData: [String: MarketData]
```

### SwiftUI Interface

Following HIG principles:

- **Layout**: Proper spacing using system spacing guidelines
- **Typography**: SF Pro font with semantic text styles
- **Colors**: Semantic colors that adapt to light/dark mode
- **Accessibility**: VoiceOver support with proper labels
- **Animations**: Smooth, natural animations using SwiftUI

### visionOS Support

Special features for Apple Vision Pro:

- 3D depth with `.frame(depth:)` modifier
- Spatial layout using NavigationSplitView
- Ultra-thin materials for glass morphism
- Immersive space for full 3D trading environment

## Integration with Akka

The Swift layer communicates with the Akka reactive system via:

1. **HTTP REST API**:
   - POST `/api/orders` - Submit new orders
   - POST `/api/trades` - Report executed trades
   - GET `/api/orderbook/{symbol}` - Fetch order book
   - GET `/health` - Health check

2. **Data Format**: JSON with ISO 8601 timestamps

3. **Connection Management**:
   ```swift
   let bridge = AkkaBridge(
       configuration: AkkaBridge.Configuration(
           akkaHost: "localhost",
           akkaPort: 8080,
           useHTTP: true
       ),
       engine: engine
   )
   
   try await bridge.connect()
   ```

## Building and Running

### Prerequisites

- macOS 14+ (Sonoma or later)
- Xcode 15+
- Swift 5.9+
- iOS 17+ / visionOS 1.0+ (for deployment)

### Build Commands

```bash
# Navigate to swift directory
cd swift

# Build the package
swift build

# Run tests
swift test

# Run the application (macOS)
swift run HFTSwiftApp

# Build for release
swift build -c release
```

### Xcode Integration

```bash
# Generate Xcode project
swift package generate-xcodeproj

# Open in Xcode
open HFTSwift.xcodeproj
```

## Testing

The package includes comprehensive unit tests:

```bash
# Run all tests
swift test

# Run tests with verbose output
swift test --verbose

# Run specific test categories
swift test --filter HFTSwiftTests      # Core model tests
swift test --filter AuditCompliance    # Audit compliance tests
swift test --filter NILCompliance      # NIL compliance tests (requires Apple platforms)
swift test --filter Vision             # Vision benchmarks (requires Apple platforms)
```

Test coverage includes:
- Order creation and validation
- Order matching logic
- Trade execution
- Order book management
- Reactive engine behavior
- **77 audit compliance tests**
- **NIL (National Instrument List) compliance** (Apple platforms)
- **Vision-inspired performance benchmarks** (Apple platforms)

**Note**: NIL compliance and Vision benchmark tests use advanced features that require Apple platforms (iOS, macOS, visionOS) for full execution. Core functionality tests run on all platforms including Linux.

## NIL Compliance

### National Instrument List Validation

The Swift implementation includes a comprehensive NIL (National Instrument List) compliance framework:

```swift
// Create NIL checker
let nilChecker = NILComplianceChecker()

// Check compliance for a symbol
let result = nilChecker.checkCompliance(symbol: "AAPL")

switch result {
case .approved:
    print("Symbol approved for trading")
case .restricted(let reasons):
    print("Symbol restricted: \(reasons)")
case .prohibited(let reason):
    print("Symbol prohibited: \(reason)")
case .unknown(let symbol):
    print("Unknown symbol: \(symbol)")
}

// Validate order with NIL compliance
let (isValid, reason) = nilChecker.validateOrder(order)

// Submit order with NIL validation
engine.submitOrderWithNILCompliance(
    order,
    nilChecker: nilChecker,
    auditLogger: auditLogger
)
```

**Supported Jurisdictions:**
- US (United States)
- EU (European Union)
- UK (United Kingdom)
- CA (Canada)
- JP (Japan)
- GLOBAL (Cross-jurisdictional)

**Status Types:**
- APPROVED - Cleared for trading
- RESTRICTED - Trading allowed with conditions
- PROHIBITED - Trading not permitted
- SUSPENDED - Temporarily halted
- PENDING - Awaiting regulatory approval

## Vision-Inspired Benchmarks

### Performance Metrics

The Swift layer includes comprehensive performance benchmarks inspired by Apple Vision's real-time requirements:

```bash
# Run benchmarks
cd swift && swift test --filter Vision
```

**Benchmark Categories:**

1. **Latency Benchmarks**
   - Order processing latency (target: <100μs)
   - Order matching latency (target: <50μs)
   - Audit logging latency (target: <200μs)

2. **Throughput Benchmarks**
   - Order processing throughput (target: >100K ops/sec)
   - Trade execution throughput (target: >50K trades/sec)
   - Concurrent order processing

3. **Memory Benchmarks**
   - Order book memory efficiency
   - Memory usage per 1000 orders

4. **Vision-Specific Benchmarks**
   - Spatial computing readiness (visionOS)
   - UI update latency (60 FPS target)
   - 3D rendering performance

**Performance Targets:**
- Sub-microsecond order processing
- 60 FPS UI updates for SwiftUI
- Low memory footprint for order books
- Thread-safe concurrent execution
- visionOS spatial computing ready

## Human Interface Guidelines Compliance

### Design Principles

1. **Clarity**: Clear visual hierarchy and legible text
2. **Deference**: Interface defers to content
3. **Depth**: Layers and motion convey hierarchy

### Implementation

- **Spacing**: 8pt grid system (8, 16, 24, 32px)
- **Typography**: 
  - Large Title: 34pt
  - Title: 28pt
  - Headline: 17pt semibold
  - Body: 17pt regular
  - Caption: 11pt regular

- **Colors**:
  - System colors for semantic meaning
  - Green for buy orders
  - Red for sell orders
  - Orange for trade values
  - Adaptive colors for dark mode

- **Accessibility**:
  - VoiceOver labels on all interactive elements
  - Dynamic Type support
  - Sufficient contrast ratios (WCAG AA)
  - Keyboard navigation support

### visionOS Specific Guidelines

1. **Spatial Design**: Content arranged in 3D space
2. **Depth**: Use of `.frame(depth:)` for layering
3. **Materials**: Glass material with `.ultraThinMaterial`
4. **Focus**: Clear focus indicators for eye tracking
5. **Ergonomics**: Content positioned within comfortable viewing angles

## Cross-Platform Support

### iOS/iPadOS
- Optimized for touch input
- Portrait and landscape orientations
- Multitasking support (iPad)
- Keyboard shortcuts

### macOS
- Mouse and keyboard input
- Window management
- Menu bar integration
- Toolbar support

### visionOS
- Eye tracking and hand gestures
- Spatial computing
- 3D depth and layering
- Immersive spaces

## Performance

- **Reactive Updates**: Sub-millisecond UI updates using Combine
- **Memory**: Efficient with @StateObject and @ObservedObject
- **Threading**: Background processing on dedicated queues
- **Rendering**: Hardware-accelerated SwiftUI rendering

## Integration Points

The Swift layer integrates with:

1. **Akka Reactive Bridge**: Message passing for orders and trades
2. **Java Powerhouse**: Can communicate via JNI or HTTP
3. **Erlang Supervisor**: Status monitoring and health checks
4. **Ada Engine**: Via REST API or native bindings

## Example Usage

```swift
import SwiftUI
import HFTSwift

@main
struct MyTradingApp: App {
    @StateObject private var engine = ReactiveTradeEngine()
    @StateObject private var bridge: AkkaBridge
    
    init() {
        let engine = ReactiveTradeEngine()
        _bridge = StateObject(wrappedValue: AkkaBridge(engine: engine))
    }
    
    var body: some Scene {
        WindowGroup {
            TradingDashboardView(engine: engine, akkaBridge: bridge)
        }
    }
}
```

## Contributing

When contributing to the Swift codebase:

1. Follow Swift API Design Guidelines
2. Use SwiftLint for code style
3. Add tests for new features
4. Update documentation
5. Follow HIG for UI changes
6. Test on all supported platforms

## Resources

- [Swift Documentation](https://swift.org/documentation/)
- [SwiftUI Documentation](https://developer.apple.com/documentation/swiftui/)
- [Combine Framework](https://developer.apple.com/documentation/combine)
- [Human Interface Guidelines](https://developer.apple.com/design/human-interface-guidelines/)
- [visionOS Design](https://developer.apple.com/design/human-interface-guidelines/designing-for-visionos)

## License

MIT License - See LICENSE file in repository root
