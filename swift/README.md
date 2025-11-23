# Swift Integration Guide

## Overview

The Swift component of the HFT system provides a modern, reactive user interface using **SwiftUI** and **Swift Combine** for reactive programming. This implementation follows Apple's Human Interface Guidelines (HIG) and supports iOS, macOS, iPadOS, and visionOS platforms.

## Architecture

### Core Components

1. **Swift Combine Reactive Engine** (`ReactiveEngine.swift`)
   - Reactive data layer using Combine Publishers and Subscribers
   - Observable object for real-time UI updates
   - Order processing and matching logic
   - Trade execution tracking

2. **Akka Bridge** (`AkkaBridge.swift`)
   - Integration with the Scala/Akka reactive system
   - HTTP/TCP communication layer
   - Asynchronous message passing
   - Error handling and connection management

3. **SwiftUI Views** (`Views/`)
   - `TradingDashboardView`: Main dashboard with tab navigation
   - `OrderBookView`: Real-time order book visualization
   - `RecentTradesView`: Trade execution history
   - `MarketDataView`: Market data cards
   - `OrderEntryView`: Order submission form

4. **Domain Models** (`Models.swift`)
   - `Order`: Trading order representation
   - `Trade`: Executed trade
   - `OrderBook`: Buy/sell order collections
   - `MarketData`: Market statistics

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

# Run specific test
swift test --filter HFTSwiftTests.testOrderMatching
```

Test coverage includes:
- Order creation and validation
- Order matching logic
- Trade execution
- Order book management
- Reactive engine behavior

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
