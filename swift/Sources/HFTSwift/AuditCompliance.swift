import Foundation

#if canImport(Combine)
import Combine
#endif

/// Audit event types for compliance tracking
public enum AuditEventType: String, Codable, Sendable {
    case orderSubmitted = "ORDER_SUBMITTED"
    case orderAccepted = "ORDER_ACCEPTED"
    case orderRejected = "ORDER_REJECTED"
    case orderCancelled = "ORDER_CANCELLED"
    case tradeExecuted = "TRADE_EXECUTED"
    case configurationChanged = "CONFIGURATION_CHANGED"
    case connectionEstablished = "CONNECTION_ESTABLISHED"
    case connectionLost = "CONNECTION_LOST"
    case validationFailed = "VALIDATION_FAILED"
    case complianceViolation = "COMPLIANCE_VIOLATION"
}

/// Severity level for audit events
public enum AuditSeverity: String, Codable, Sendable {
    case info = "INFO"
    case warning = "WARNING"
    case error = "ERROR"
    case critical = "CRITICAL"
}

/// Audit event structure for compliance logging
public struct AuditEvent: Codable, Identifiable, Sendable {
    public let id: UUID
    public let timestamp: Date
    public let eventType: AuditEventType
    public let severity: AuditSeverity
    public let userId: String?
    public let sessionId: String?
    public let details: [String: String]
    public let entityId: String?
    public let entityType: String?
    public let previousValue: String?
    public let newValue: String?
    public let ipAddress: String?
    
    public init(
        eventType: AuditEventType,
        severity: AuditSeverity = .info,
        userId: String? = nil,
        sessionId: String? = nil,
        details: [String: String] = [:],
        entityId: String? = nil,
        entityType: String? = nil,
        previousValue: String? = nil,
        newValue: String? = nil,
        ipAddress: String? = nil
    ) {
        self.id = UUID()
        self.timestamp = Date()
        self.eventType = eventType
        self.severity = severity
        self.userId = userId
        self.sessionId = sessionId
        self.details = details
        self.entityId = entityId
        self.entityType = entityType
        self.previousValue = previousValue
        self.newValue = newValue
        self.ipAddress = ipAddress
    }
}

/// Compliance rules for trading operations
public struct ComplianceRule {
    public let id: String
    public let name: String
    public let description: String
    public let validator: (Order) -> ComplianceResult
    
    public init(id: String, name: String, description: String, validator: @escaping (Order) -> ComplianceResult) {
        self.id = id
        self.name = name
        self.description = description
        self.validator = validator
    }
}

/// Result of compliance validation
public enum ComplianceResult {
    case passed
    case failed(reason: String)
    case warning(message: String)
}

/// Audit logger protocol for dependency injection
public protocol AuditLogger {
    func log(_ event: AuditEvent)
    func query(from: Date, to: Date, eventType: AuditEventType?) -> [AuditEvent]
    func exportAuditLog(from: Date, to: Date) -> Data?
}

/// Default audit logger implementation
public final class DefaultAuditLogger: AuditLogger, @unchecked Sendable {
    private var events: [AuditEvent] = []
    private let queue = DispatchQueue(label: "com.hft.audit.logger", qos: .userInitiated)
    private let maxEvents: Int
    
    public init(maxEvents: Int = 10000) {
        self.maxEvents = maxEvents
    }
    
    public func log(_ event: AuditEvent) {
        queue.async { [weak self] in
            guard let self = self else { return }
            
            // Add event to in-memory store
            self.events.append(event)
            
            // Rotate logs if exceeding max
            if self.events.count > self.maxEvents {
                self.events.removeFirst(self.events.count - self.maxEvents)
            }
            
            // Log to console for development
            self.logToConsole(event)
            
            // In production, would also write to persistent storage
            self.persistEvent(event)
        }
    }
    
    public func query(from: Date, to: Date, eventType: AuditEventType? = nil) -> [AuditEvent] {
        var result: [AuditEvent] = []
        
        queue.sync {
            result = events.filter { event in
                guard event.timestamp >= from && event.timestamp <= to else { return false }
                
                if let type = eventType {
                    return event.eventType == type
                }
                
                return true
            }
        }
        
        return result
    }
    
    public func exportAuditLog(from: Date, to: Date) -> Data? {
        let filteredEvents = query(from: from, to: to)
        
        let encoder = JSONEncoder()
        encoder.dateEncodingStrategy = .iso8601
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        
        return try? encoder.encode(filteredEvents)
    }
    
    private func logToConsole(_ event: AuditEvent) {
        let timestamp = ISO8601DateFormatter().string(from: event.timestamp)
        let prefix: String
        
        switch event.severity {
        case .info:
            prefix = "ℹ️"
        case .warning:
            prefix = "⚠️"
        case .error:
            prefix = "❌"
        case .critical:
            prefix = "🚨"
        }
        
        print("\(prefix) AUDIT [\(timestamp)] \(event.eventType.rawValue) - \(event.details)")
    }
    
    private func persistEvent(_ event: AuditEvent) {
        // In production, this would write to:
        // - Persistent database (SQLite, Core Data, Realm)
        // - Remote audit service
        // - Encrypted log files
        // For now, this is a placeholder
    }
}

/// Compliance checker for trading operations
public class ComplianceChecker {
    private var rules: [ComplianceRule] = []
    private let auditLogger: AuditLogger
    
    public init(auditLogger: AuditLogger) {
        self.auditLogger = auditLogger
        setupDefaultRules()
    }
    
    private func setupDefaultRules() {
        // Rule 1: Maximum order size
        let maxOrderSize = ComplianceRule(
            id: "MAX_ORDER_SIZE",
            name: "Maximum Order Size",
            description: "Orders cannot exceed 100,000 shares"
        ) { order in
            if order.quantity > 100_000 {
                return .failed(reason: "Order quantity \(order.quantity) exceeds maximum of 100,000")
            }
            if order.quantity > 50_000 {
                return .warning(message: "Large order: \(order.quantity) shares")
            }
            return .passed
        }
        
        // Rule 2: Minimum price validation
        let minPrice = ComplianceRule(
            id: "MIN_PRICE",
            name: "Minimum Price",
            description: "Orders must have a price greater than 0.01"
        ) { order in
            if order.price < 0.01 {
                return .failed(reason: "Order price \(order.price) is below minimum of 0.01")
            }
            return .passed
        }
        
        // Rule 3: Maximum order value
        let maxOrderValue = ComplianceRule(
            id: "MAX_ORDER_VALUE",
            name: "Maximum Order Value",
            description: "Orders cannot exceed $10,000,000 in value"
        ) { order in
            let value = order.value
            if value > 10_000_000 {
                return .failed(reason: "Order value $\(value) exceeds maximum of $10,000,000")
            }
            if value > 5_000_000 {
                return .warning(message: "High value order: $\(value)")
            }
            return .passed
        }
        
        // Rule 4: Symbol validation
        let symbolValidation = ComplianceRule(
            id: "SYMBOL_VALIDATION",
            name: "Symbol Validation",
            description: "Symbol must be 1-5 uppercase letters"
        ) { order in
            let symbol = order.symbol
            if symbol.isEmpty || symbol.count > 5 {
                return .failed(reason: "Invalid symbol length: \(symbol)")
            }
            if symbol.uppercased() != symbol {
                return .failed(reason: "Symbol must be uppercase: \(symbol)")
            }
            if !symbol.allSatisfy({ $0.isLetter }) {
                return .failed(reason: "Symbol must contain only letters: \(symbol)")
            }
            return .passed
        }
        
        rules = [maxOrderSize, minPrice, maxOrderValue, symbolValidation]
    }
    
    public func addRule(_ rule: ComplianceRule) {
        rules.append(rule)
    }
    
    public func validateOrder(_ order: Order) -> [ComplianceResult] {
        var results: [ComplianceResult] = []
        
        for rule in rules {
            let result = rule.validator(order)
            results.append(result)
            
            // Log compliance violations
            switch result {
            case .failed(let reason):
                auditLogger.log(AuditEvent(
                    eventType: .complianceViolation,
                    severity: .error,
                    details: [
                        "ruleId": rule.id,
                        "ruleName": rule.name,
                        "reason": reason,
                        "orderId": String(order.id),
                        "symbol": order.symbol
                    ],
                    entityId: String(order.id),
                    entityType: "Order"
                ))
                
            case .warning(let message):
                auditLogger.log(AuditEvent(
                    eventType: .validationFailed,
                    severity: .warning,
                    details: [
                        "ruleId": rule.id,
                        "ruleName": rule.name,
                        "message": message,
                        "orderId": String(order.id),
                        "symbol": order.symbol
                    ],
                    entityId: String(order.id),
                    entityType: "Order"
                ))
                
            case .passed:
                break
            }
        }
        
        return results
    }
    
    public func hasViolations(_ results: [ComplianceResult]) -> Bool {
        results.contains { result in
            if case .failed = result {
                return true
            }
            return false
        }
    }
}

#if canImport(Combine)
/// Audit-enabled reactive trading engine extension
extension ReactiveTradeEngine {
    
    /// Submit order with audit logging
    public func submitOrderWithAudit(
        _ order: Order,
        auditLogger: AuditLogger,
        complianceChecker: ComplianceChecker,
        userId: String? = nil,
        sessionId: String? = nil
    ) {
        // Log order submission
        auditLogger.log(AuditEvent(
            eventType: .orderSubmitted,
            severity: .info,
            userId: userId,
            sessionId: sessionId,
            details: [
                "orderId": String(order.id),
                "symbol": order.symbol,
                "side": order.side.rawValue,
                "price": String(describing: order.price),
                "quantity": String(order.quantity)
            ],
            entityId: String(order.id),
            entityType: "Order"
        ))
        
        // Validate compliance
        let validationResults = complianceChecker.validateOrder(order)
        
        if complianceChecker.hasViolations(validationResults) {
            // Log rejection
            auditLogger.log(AuditEvent(
                eventType: .orderRejected,
                severity: .error,
                userId: userId,
                sessionId: sessionId,
                details: [
                    "orderId": String(order.id),
                    "reason": "Compliance violation"
                ],
                entityId: String(order.id),
                entityType: "Order"
            ))
            
            errorPublisher.send(.invalidOrder(reason: "Compliance validation failed"))
            return
        }
        
        // Log acceptance
        auditLogger.log(AuditEvent(
            eventType: .orderAccepted,
            severity: .info,
            userId: userId,
            sessionId: sessionId,
            details: [
                "orderId": String(order.id)
            ],
            entityId: String(order.id),
            entityType: "Order"
        ))
        
        // Submit order through normal flow
        submitOrder(order)
    }
}
#endif
