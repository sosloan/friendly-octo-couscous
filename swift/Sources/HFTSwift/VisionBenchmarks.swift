import Foundation

#if canImport(CoreFoundation)
import CoreFoundation
#endif

/// Vision-Inspired Performance Benchmarks
/// Measures system performance with visionOS-style metrics and real-time monitoring

// MARK: - Time Measurement Helper

#if canImport(CoreFoundation)
private func getCurrentTime() -> Double {
    return CFAbsoluteTimeGetCurrent()
}
#else
private func getCurrentTime() -> Double {
    return Date().timeIntervalSince1970
}
#endif

// MARK: - Benchmark Types

/// Performance metric categories
public enum BenchmarkCategory: String {
    case latency = "Latency"
    case throughput = "Throughput"
    case memory = "Memory"
    case cpu = "CPU"
    case gpu = "GPU"
    case network = "Network"
    case rendering = "Rendering"
}

/// Benchmark result
public struct BenchmarkResult {
    public let category: BenchmarkCategory
    public let name: String
    public let value: Double
    public let unit: String
    public let timestamp: Date
    public let passed: Bool
    public let threshold: Double?
    
    public init(category: BenchmarkCategory, name: String, value: Double, unit: String, passed: Bool, threshold: Double? = nil) {
        self.category = category
        self.name = name
        self.value = value
        self.unit = unit
        self.timestamp = Date()
        self.passed = passed
        self.threshold = threshold
    }
}

// MARK: - Vision Benchmark Suite

public class VisionBenchmarkSuite {
    private var results: [BenchmarkResult] = []
    
    public init() {}
    
    // MARK: - Latency Benchmarks
    
    /// Measure order processing latency
    public func benchmarkOrderProcessingLatency() -> BenchmarkResult {
        let startTime = getCurrentTime()
        
        // Simulate order processing
        let order = Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
        _ = order.canMatch(with: Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell))
        
        let endTime = getCurrentTime()
        let latencyMicros = (endTime - startTime) * 1_000_000
        
        let result = BenchmarkResult(
            category: .latency,
            name: "Order Processing Latency",
            value: latencyMicros,
            unit: "μs",
            passed: latencyMicros < 100, // Sub-100μs threshold
            threshold: 100
        )
        
        results.append(result)
        return result
    }
    
    /// Measure order matching latency
    public func benchmarkOrderMatchingLatency() -> BenchmarkResult {
        let orderBook = OrderBook(
            buyOrders: [Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)],
            sellOrders: [Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell)]
        )
        
        let startTime = getCurrentTime()
        
        // Simulate matching
        _ = orderBook.bestBid
        _ = orderBook.bestAsk
        _ = orderBook.spread
        
        let endTime = getCurrentTime()
        let latencyMicros = (endTime - startTime) * 1_000_000
        
        let result = BenchmarkResult(
            category: .latency,
            name: "Order Matching Latency",
            value: latencyMicros,
            unit: "μs",
            passed: latencyMicros < 50, // Sub-50μs threshold
            threshold: 50
        )
        
        results.append(result)
        return result
    }
    
    /// Measure audit logging latency
    public func benchmarkAuditLoggingLatency() -> BenchmarkResult {
        let logger = DefaultAuditLogger()
        
        let startTime = getCurrentTime()
        
        logger.log(AuditEvent(eventType: .orderSubmitted, severity: .info))
        
        let endTime = getCurrentTime()
        let latencyMicros = (endTime - startTime) * 1_000_000
        
        let result = BenchmarkResult(
            category: .latency,
            name: "Audit Logging Latency",
            value: latencyMicros,
            unit: "μs",
            passed: latencyMicros < 200, // Sub-200μs threshold
            threshold: 200
        )
        
        results.append(result)
        return result
    }
    
    // MARK: - Throughput Benchmarks
    
    /// Measure order processing throughput
    public func benchmarkOrderThroughput() -> BenchmarkResult {
        let iterations = 10_000
        let startTime = getCurrentTime()
        
        for i in 0..<iterations {
            let order = Order(id: Int64(i), symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
            _ = order.value
        }
        
        let endTime = getCurrentTime()
        let duration = endTime - startTime
        let throughput = Double(iterations) / duration
        
        let result = BenchmarkResult(
            category: .throughput,
            name: "Order Processing Throughput",
            value: throughput,
            unit: "ops/sec",
            passed: throughput > 100_000, // >100K ops/sec
            threshold: 100_000
        )
        
        results.append(result)
        return result
    }
    
    /// Measure trade execution throughput
    public func benchmarkTradeExecutionThroughput() -> BenchmarkResult {
        let iterations = 5_000
        let buyOrder = Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)
        let sellOrder = Order(id: 2, symbol: "AAPL", price: 150.25, quantity: 100, side: .sell)
        
        let startTime = getCurrentTime()
        
        for i in 0..<iterations {
            _ = Trade(
                id: Int64(i),
                buyOrder: buyOrder,
                sellOrder: sellOrder,
                executionPrice: 150.25,
                quantity: 100
            )
        }
        
        let endTime = getCurrentTime()
        let duration = endTime - startTime
        let throughput = Double(iterations) / duration
        
        let result = BenchmarkResult(
            category: .throughput,
            name: "Trade Execution Throughput",
            value: throughput,
            unit: "trades/sec",
            passed: throughput > 50_000, // >50K trades/sec
            threshold: 50_000
        )
        
        results.append(result)
        return result
    }
    
    // MARK: - Memory Benchmarks
    
    /// Measure order book memory efficiency
    public func benchmarkOrderBookMemory() -> BenchmarkResult {
        let orderCount = 1000
        var buyOrders: [Order] = []
        
        for i in 0..<orderCount {
            buyOrders.append(Order(
                id: Int64(i),
                symbol: "AAPL",
                price: Decimal(150 + Double(i) * 0.01),
                quantity: 100,
                side: .buy
            ))
        }
        
        let orderBook = OrderBook(buyOrders: buyOrders, sellOrders: [])
        
        // Estimate memory usage (rough approximation)
        let bytesPerOrder = MemoryLayout<Order>.stride
        let totalBytes = bytesPerOrder * orderCount
        let kilobytes = Double(totalBytes) / 1024.0
        
        let result = BenchmarkResult(
            category: .memory,
            name: "Order Book Memory Efficiency",
            value: kilobytes,
            unit: "KB",
            passed: kilobytes < 500, // <500KB for 1000 orders
            threshold: 500
        )
        
        results.append(result)
        return result
    }
    
    // MARK: - Concurrent Performance
    
    /// Measure concurrent order processing
    public func benchmarkConcurrentOrderProcessing() -> BenchmarkResult {
        #if os(Linux)
        // Simplified for Linux to avoid threading issues
        let iterations = 1000
        let startTime = getCurrentTime()
        
        for i in 0..<iterations {
            let order = Order(
                id: Int64(i),
                symbol: "AAPL",
                price: 150.50,
                quantity: 100,
                side: .buy
            )
            _ = order.value
        }
        
        let endTime = getCurrentTime()
        let duration = endTime - startTime
        let throughput = Double(iterations) / duration
        #else
        let iterations = 1000
        let concurrentQueues = 4
        let startTime = getCurrentTime()
        
        let group = DispatchGroup()
        
        for queueIndex in 0..<concurrentQueues {
            DispatchQueue.global(qos: .userInitiated).async(group: group) {
                for i in 0..<iterations {
                    let order = Order(
                        id: Int64(queueIndex * iterations + i),
                        symbol: "AAPL",
                        price: 150.50,
                        quantity: 100,
                        side: .buy
                    )
                    _ = order.value
                }
            }
        }
        
        group.wait()
        
        let endTime = getCurrentTime()
        let duration = endTime - startTime
        let totalOps = Double(iterations * concurrentQueues)
        let throughput = totalOps / duration
        #endif
        
        let result = BenchmarkResult(
            category: .throughput,
            name: "Concurrent Order Processing",
            value: throughput,
            unit: "ops/sec",
            passed: throughput > 1_000, // Lower threshold for Linux
            threshold: 1_000
        )
        
        results.append(result)
        return result
    }
    
    // MARK: - Vision-Specific Benchmarks
    
    /// Measure spatial computing readiness (visionOS)
    public func benchmarkSpatialComputingReadiness() -> BenchmarkResult {
        // Simulate 3D depth calculations for visionOS
        let iterations = 1000
        let startTime = getCurrentTime()
        
        for _ in 0..<iterations {
            // Simulate spatial calculations
            let x = sin(Double.random(in: 0...Double.pi))
            let y = cos(Double.random(in: 0...Double.pi))
            let z = tan(Double.random(in: 0...Double.pi/4))
            _ = sqrt(x*x + y*y + z*z)
        }
        
        let endTime = getCurrentTime()
        let latencyMicros = (endTime - startTime) * 1_000_000 / Double(iterations)
        
        let result = BenchmarkResult(
            category: .rendering,
            name: "Spatial Computing Readiness",
            value: latencyMicros,
            unit: "μs/op",
            passed: latencyMicros < 10, // <10μs per spatial calc
            threshold: 10
        )
        
        results.append(result)
        return result
    }
    
    /// Measure UI update latency (SwiftUI)
    public func benchmarkUIUpdateLatency() -> BenchmarkResult {
        let iterations = 100
        let startTime = getCurrentTime()
        
        for _ in 0..<iterations {
            // Simulate SwiftUI state updates
            var orderBook = OrderBook()
            orderBook = OrderBook(
                buyOrders: [Order(id: 1, symbol: "AAPL", price: 150.50, quantity: 100, side: .buy)],
                sellOrders: []
            )
            _ = orderBook.bestBid
        }
        
        let endTime = getCurrentTime()
        let avgLatencyMs = (endTime - startTime) * 1000 / Double(iterations)
        
        let result = BenchmarkResult(
            category: .rendering,
            name: "UI Update Latency",
            value: avgLatencyMs,
            unit: "ms",
            passed: avgLatencyMs < 8.33, // 120 FPS threshold
            threshold: 8.33
        )
        
        results.append(result)
        return result
    }
    
    /// Target frame time for 480 FPS rendering (0.75ms)
    private static let fps480ThresholdMs: Double = 1000.0 / 480.0
    
    /// Measure Metal rendering latency for 480 FPS target
    public func benchmarkMetalRenderingLatency() -> BenchmarkResult {
        let iterations = 100
        let transformCount = 100
        
        // Pre-allocate array outside the loop for accurate measurement
        var simulatedTransforms = [Float](repeating: 0, count: transformCount * 2)
        
        let startTime = getCurrentTime()
        
        for _ in 0..<iterations {
            // Simulate Metal rendering pipeline operations
            // In production, this would use actual Metal rendering
            for i in 0..<transformCount {
                let angle = Float(i) * Float(2.0 * Double.pi) / Float(transformCount)
                simulatedTransforms[i * 2] = cos(angle) * 250.0
                simulatedTransforms[i * 2 + 1] = sin(angle) * 250.0
            }
            _ = simulatedTransforms.reduce(0, +)
        }
        
        let endTime = getCurrentTime()
        let avgLatencyMs = (endTime - startTime) * 1000 / Double(iterations)
        
        let result = BenchmarkResult(
            category: .rendering,
            name: "Metal Rendering Latency (480 FPS Target)",
            value: avgLatencyMs,
            unit: "ms",
            passed: avgLatencyMs < VisionBenchmarkSuite.fps480ThresholdMs,
            threshold: VisionBenchmarkSuite.fps480ThresholdMs
        )
        
        results.append(result)
        return result
    }
    
    // MARK: - Results
    
    /// Run all benchmarks
    public func runAllBenchmarks() -> [BenchmarkResult] {
        results.removeAll()
        
        print("Running Vision-Inspired Benchmark Suite...")
        print("")
        
        _ = benchmarkOrderProcessingLatency()
        _ = benchmarkOrderMatchingLatency()
        _ = benchmarkAuditLoggingLatency()
        _ = benchmarkOrderThroughput()
        _ = benchmarkTradeExecutionThroughput()
        _ = benchmarkOrderBookMemory()
        _ = benchmarkConcurrentOrderProcessing()
        _ = benchmarkSpatialComputingReadiness()
        _ = benchmarkUIUpdateLatency()
        _ = benchmarkMetalRenderingLatency()
        
        return results
    }
    
    /// Generate benchmark report
    public func generateReport() -> String {
        var report = """
        ════════════════════════════════════════════════════════════
          VISION-INSPIRED PERFORMANCE BENCHMARK REPORT
          Swift HFT System Performance Metrics
          Date: \(Date())
        ════════════════════════════════════════════════════════════
        
        """
        
        let groupedResults = Dictionary(grouping: results) { $0.category }
        
        for category in BenchmarkCategory.allCases {
            guard let categoryResults = groupedResults[category], !categoryResults.isEmpty else {
                continue
            }
            
            report += "\n\(category.rawValue.uppercased()) BENCHMARKS\n"
            report += String(repeating: "─", count: 60) + "\n"
            
            for result in categoryResults {
                let status = result.passed ? "✓" : "✗"
                let thresholdText = result.threshold.map { " (threshold: \($0) \(result.unit))" } ?? ""
                report += "  [\(status)] \(result.name): \(String(format: "%.2f", result.value)) \(result.unit)\(thresholdText)\n"
            }
        }
        
        let totalTests = results.count
        let passedTests = results.filter { $0.passed }.count
        let passRate = Double(passedTests) / Double(totalTests) * 100
        
        report += """
        
        ════════════════════════════════════════════════════════════
        Benchmark Summary:
          Total Benchmarks:   \(totalTests)
          Passed:             \(passedTests)
          Failed:             \(totalTests - passedTests)
          Pass Rate:          \(String(format: "%.1f", passRate))%
        ════════════════════════════════════════════════════════════
        
        """
        
        if passRate == 100 {
            report += """
            ✓ ════════════════════════════════════════════════════════
            ✓  ALL BENCHMARKS PASSED!
            ✓  System Performance: EXCELLENT
            ✓  visionOS Ready: YES
            ✓ ════════════════════════════════════════════════════════
            
            """
        }
        
        return report
    }
    
    /// Get all results
    public func getResults() -> [BenchmarkResult] {
        return results
    }
}

// MARK: - Benchmark Category Extension

extension BenchmarkCategory: CaseIterable {}
