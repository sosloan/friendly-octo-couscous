import Foundation

/// NIL (National Instrument List) Compliance Framework
/// Validates trading against regulatory instrument lists and restrictions

// MARK: - NIL Compliance Types

/// NIL instrument status
public enum NILStatus: String, Codable, Sendable {
    case approved = "APPROVED"
    case restricted = "RESTRICTED"
    case prohibited = "PROHIBITED"
    case suspended = "SUSPENDED"
    case pending = "PENDING"
}

/// NIL jurisdiction codes
public enum NILJurisdiction: String, Codable, Sendable {
    case us = "US"
    case eu = "EU"
    case uk = "UK"
    case ca = "CA"
    case jp = "JP"
    case global = "GLOBAL"
}

/// NIL instrument classification
public struct NILInstrument: Codable, Sendable {
    public let symbol: String
    public let isin: String?
    public let cusip: String?
    public let status: NILStatus
    public let jurisdiction: NILJurisdiction
    public let restrictions: [String]
    public let lastUpdated: Date
    
    public init(symbol: String, isin: String? = nil, cusip: String? = nil, status: NILStatus, jurisdiction: NILJurisdiction, restrictions: [String] = [], lastUpdated: Date = Date()) {
        self.symbol = symbol
        self.isin = isin
        self.cusip = cusip
        self.status = status
        self.jurisdiction = jurisdiction
        self.restrictions = restrictions
        self.lastUpdated = lastUpdated
    }
}

/// NIL compliance result
public enum NILComplianceResult: Sendable {
    case approved
    case restricted(reasons: [String])
    case prohibited(reason: String)
    case unknown(symbol: String)
}

// MARK: - NIL Compliance Checker

public final class NILComplianceChecker: @unchecked Sendable {
    private var instrumentList: [String: NILInstrument] = [:]
    private let queue = DispatchQueue(label: "com.hft.nil.compliance", attributes: .concurrent)
    
    public init() {
        loadDefaultInstruments()
    }
    
    private func loadDefaultInstruments() {
        // Load approved instruments
        let approvedSymbols = ["AAPL", "GOOGL", "MSFT", "AMZN", "TSLA", "META", "NVDA", "JPM", "BAC", "WMT"]
        for symbol in approvedSymbols {
            instrumentList[symbol] = NILInstrument(
                symbol: symbol,
                status: .approved,
                jurisdiction: .us
            )
        }
        
        // Load restricted instruments
        let restrictedSymbols = ["PNNY", "CRYPTO", "OTC"]
        for symbol in restrictedSymbols {
            instrumentList[symbol] = NILInstrument(
                symbol: symbol,
                status: .restricted,
                jurisdiction: .us,
                restrictions: ["Penny stock restrictions", "Enhanced due diligence required"]
            )
        }
        
        // Load prohibited instruments
        let prohibitedSymbols = ["SANC", "EMBARGO"]
        for symbol in prohibitedSymbols {
            instrumentList[symbol] = NILInstrument(
                symbol: symbol,
                status: .prohibited,
                jurisdiction: .global,
                restrictions: ["Sanctions list", "OFAC prohibited"]
            )
        }
    }
    
    /// Check NIL compliance for a symbol
    public func checkCompliance(symbol: String, jurisdiction: NILJurisdiction = .us) -> NILComplianceResult {
        var result: NILComplianceResult = .unknown(symbol: symbol)
        
        queue.sync {
            guard let instrument = instrumentList[symbol] else {
                result = .unknown(symbol: symbol)
                return
            }
            
            switch instrument.status {
            case .approved:
                result = .approved
                
            case .restricted:
                result = .restricted(reasons: instrument.restrictions)
                
            case .prohibited, .suspended:
                let reason = instrument.restrictions.first ?? "Instrument not permitted"
                result = .prohibited(reason: reason)
                
            case .pending:
                result = .restricted(reasons: ["Pending regulatory approval"])
            }
        }
        
        return result
    }
    
    /// Add instrument to NIL
    public func addInstrument(_ instrument: NILInstrument) {
        queue.async(flags: .barrier) { [weak self] in
            self?.instrumentList[instrument.symbol] = instrument
        }
    }
    
    /// Update instrument status
    public func updateStatus(symbol: String, status: NILStatus) {
        queue.async(flags: .barrier) { [weak self] in
            guard var instrument = self?.instrumentList[symbol] else { return }
            instrument = NILInstrument(
                symbol: instrument.symbol,
                isin: instrument.isin,
                cusip: instrument.cusip,
                status: status,
                jurisdiction: instrument.jurisdiction,
                restrictions: instrument.restrictions,
                lastUpdated: Date()
            )
            self?.instrumentList[symbol] = instrument
        }
    }
    
    /// Get all instruments by status
    public func getInstrumentsByStatus(_ status: NILStatus) -> [NILInstrument] {
        var result: [NILInstrument] = []
        
        queue.sync {
            result = instrumentList.values.filter { $0.status == status }
        }
        
        return result
    }
    
    /// Validate order against NIL
    public func validateOrder(_ order: Order) -> (isValid: Bool, reason: String?) {
        let complianceResult = checkCompliance(symbol: order.symbol)
        
        switch complianceResult {
        case .approved:
            return (true, nil)
            
        case .restricted(let reasons):
            let reasonText = reasons.joined(separator: ", ")
            return (false, "Restricted: \(reasonText)")
            
        case .prohibited(let reason):
            return (false, "Prohibited: \(reason)")
            
        case .unknown(let symbol):
            return (false, "Unknown instrument: \(symbol)")
        }
    }
}

// MARK: - NIL Integration with Order Processing

#if canImport(Combine)
import Combine

extension ReactiveTradeEngine {
    /// Submit order with NIL compliance validation
    public func submitOrderWithNILCompliance(
        _ order: Order,
        nilChecker: NILComplianceChecker,
        auditLogger: AuditLogger? = nil
    ) {
        // Validate NIL compliance
        let (isValid, reason) = nilChecker.validateOrder(order)
        
        if !isValid {
            // Log NIL violation
            auditLogger?.log(AuditEvent(
                eventType: .complianceViolation,
                severity: .critical,
                details: [
                    "orderId": String(order.id),
                    "symbol": order.symbol,
                    "violation": "NIL_COMPLIANCE",
                    "reason": reason ?? "Unknown"
                ],
                entityId: String(order.id),
                entityType: "Order"
            ))
            
            errorPublisher.send(.invalidOrder(reason: "NIL compliance violation: \(reason ?? "")"))
            return
        }
        
        // Log NIL approval
        auditLogger?.log(AuditEvent(
            eventType: .orderAccepted,
            severity: .info,
            details: [
                "orderId": String(order.id),
                "symbol": order.symbol,
                "nilStatus": "APPROVED"
            ],
            entityId: String(order.id),
            entityType: "Order"
        ))
        
        // Submit order
        submitOrder(order)
    }
}
#endif
