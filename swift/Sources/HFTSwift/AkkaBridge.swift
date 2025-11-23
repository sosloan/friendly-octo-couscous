import Foundation

#if canImport(Combine)
import Combine

/// Bridge between Swift Combine and Akka Reactive System
/// Provides integration with the Scala/Akka actor system via HTTP/TCP
public class AkkaBridge: ObservableObject {
    
    // MARK: - Configuration
    
    public struct Configuration {
        public let akkaHost: String
        public let akkaPort: Int
        public let useHTTP: Bool
        
        public init(akkaHost: String = "localhost", akkaPort: Int = 8080, useHTTP: Bool = true) {
            self.akkaHost = akkaHost
            self.akkaPort = akkaPort
            self.useHTTP = useHTTP
        }
        
        public var baseURL: URL? {
            if useHTTP {
                return URL(string: "http://\(akkaHost):\(akkaPort)")
            } else {
                return URL(string: "tcp://\(akkaHost):\(akkaPort)")
            }
        }
    }
    
    // MARK: - Published Properties
    
    @Published public private(set) var isConnected: Bool = false
    @Published public private(set) var lastError: String?
    
    // MARK: - Properties
    
    private let configuration: Configuration
    private let engine: ReactiveTradeEngine
    private var cancellables = Set<AnyCancellable>()
    private let session: URLSession
    
    // MARK: - Initialization
    
    public init(configuration: Configuration = Configuration(), engine: ReactiveTradeEngine) {
        self.configuration = configuration
        self.engine = engine
        
        let config = URLSessionConfiguration.default
        config.timeoutIntervalForRequest = 5.0
        config.timeoutIntervalForResource = 10.0
        self.session = URLSession(configuration: config)
        
        setupSubscriptions()
    }
    
    // MARK: - Setup
    
    private func setupSubscriptions() {
        // Forward orders to Akka
        engine.orderSubmittedPublisher
            .sink { [weak self] order in
                self?.sendOrderToAkka(order)
            }
            .store(in: &cancellables)
        
        // Forward trades to Akka
        engine.tradeExecutedPublisher
            .sink { [weak self] trade in
                self?.sendTradeToAkka(trade)
            }
            .store(in: &cancellables)
    }
    
    // MARK: - Public Methods
    
    /// Connect to the Akka system
    public func connect() async throws {
        guard let baseURL = configuration.baseURL else {
            throw TradingError.akkaIntegrationError(reason: "Invalid Akka URL configuration")
        }
        
        // Attempt health check
        let healthURL = baseURL.appendingPathComponent("health")
        
        do {
            let (data, response) = try await session.data(from: healthURL)
            
            guard let httpResponse = response as? HTTPURLResponse,
                  httpResponse.statusCode == 200 else {
                await MainActor.run {
                    self.isConnected = false
                    self.lastError = "Health check failed"
                }
                throw TradingError.akkaIntegrationError(reason: "Health check failed")
            }
            
            await MainActor.run {
                self.isConnected = true
                self.lastError = nil
            }
        } catch {
            await MainActor.run {
                self.isConnected = false
                self.lastError = error.localizedDescription
            }
            throw error
        }
    }
    
    /// Disconnect from the Akka system
    public func disconnect() {
        isConnected = false
    }
    
    /// Fetch order book from Akka
    public func fetchOrderBook(for symbol: String) async throws -> OrderBook {
        guard let baseURL = configuration.baseURL else {
            throw TradingError.akkaIntegrationError(reason: "Invalid Akka URL configuration")
        }
        
        let orderBookURL = baseURL
            .appendingPathComponent("api/orderbook")
            .appendingPathComponent(symbol)
        
        let (data, response) = try await session.data(from: orderBookURL)
        
        guard let httpResponse = response as? HTTPURLResponse,
              httpResponse.statusCode == 200 else {
            throw TradingError.akkaIntegrationError(reason: "Failed to fetch order book")
        }
        
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        
        return try decoder.decode(OrderBook.self, from: data)
    }
    
    // MARK: - Private Methods
    
    private func sendOrderToAkka(_ order: Order) {
        guard let baseURL = configuration.baseURL else {
            engine.errorPublisher.send(.akkaIntegrationError(reason: "Invalid Akka URL"))
            return
        }
        
        Task {
            do {
                let orderURL = baseURL.appendingPathComponent("api/orders")
                var request = URLRequest(url: orderURL)
                request.httpMethod = "POST"
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                
                let encoder = JSONEncoder()
                encoder.dateEncodingStrategy = .iso8601
                request.httpBody = try encoder.encode(order)
                
                let (_, response) = try await session.data(for: request)
                
                guard let httpResponse = response as? HTTPURLResponse,
                      (200...299).contains(httpResponse.statusCode) else {
                    await MainActor.run {
                        self.lastError = "Failed to submit order to Akka"
                    }
                    return
                }
                
                // Success - order sent to Akka
                print("✓ Order \(order.id) sent to Akka system")
                
            } catch {
                await MainActor.run {
                    self.lastError = error.localizedDescription
                }
                engine.errorPublisher.send(.akkaIntegrationError(reason: error.localizedDescription))
            }
        }
    }
    
    private func sendTradeToAkka(_ trade: Trade) {
        guard let baseURL = configuration.baseURL else { return }
        
        Task {
            do {
                let tradeURL = baseURL.appendingPathComponent("api/trades")
                var request = URLRequest(url: tradeURL)
                request.httpMethod = "POST"
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                
                let encoder = JSONEncoder()
                encoder.dateEncodingStrategy = .iso8601
                request.httpBody = try encoder.encode(trade)
                
                let (_, response) = try await session.data(for: request)
                
                guard let httpResponse = response as? HTTPURLResponse,
                      (200...299).contains(httpResponse.statusCode) else {
                    return
                }
                
                // Success - trade sent to Akka
                print("✓ Trade \(trade.id) sent to Akka system")
                
            } catch {
                // Log error but don't propagate (trades are already executed locally)
                print("⚠ Failed to send trade to Akka: \(error.localizedDescription)")
            }
        }
    }
}
#endif
