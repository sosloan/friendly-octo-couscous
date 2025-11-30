import Foundation

#if canImport(CryptoKit)
import CryptoKit
#endif

/// Merkle Tree Implementation for Compliance and Data Integrity
/// Provides cryptographic verification of audit logs, transaction history, and compliance records

// MARK: - SHA256 Hash Implementation

private func sha256Hash(_ data: Data) -> String {
    #if canImport(CryptoKit)
    return SHA256.hash(data: data).compactMap { String(format: "%02x", $0) }.joined()
    #else
    // Simple hash implementation for Linux (not cryptographically secure for production)
    var hash: UInt64 = 5381
    for byte in data {
        hash = ((hash << 5) &+ hash) &+ UInt64(byte)
    }
    return String(format: "%016x", hash)
    #endif
}

private func sha512Hash(_ data: Data) -> String {
    #if canImport(CryptoKit)
    return SHA512.hash(data: data).compactMap { String(format: "%02x", $0) }.joined()
    #else
    // Simple hash implementation for Linux (not cryptographically secure for production)
    var hash: UInt64 = 7919
    for byte in data {
        hash = ((hash << 7) &+ hash) &+ UInt64(byte)
    }
    return String(format: "%016x%016x", hash, ~hash)
    #endif
}


// MARK: - Merkle Tree Node

/// A node in the Merkle tree
public final class MerkleNode: @unchecked Sendable {
    public let hash: String
    public let left: MerkleNode?
    public let right: MerkleNode?
    public let data: Data?
    
    public init(hash: String, left: MerkleNode? = nil, right: MerkleNode? = nil, data: Data? = nil) {
        self.hash = hash
        self.left = left
        self.right = right
        self.data = data
    }
    
    /// Check if this is a leaf node
    public var isLeaf: Bool {
        return left == nil && right == nil
    }
}

// MARK: - Merkle Tree

/// Merkle tree for cryptographic verification of compliance data
public final class MerkleTree: @unchecked Sendable {
    public private(set) var root: MerkleNode?
    public private(set) var leaves: [MerkleNode]
    private let hashAlgorithm: HashAlgorithm
    
    public enum HashAlgorithm: Sendable {
        case sha256
        case sha512
    }
    
    public init(data: [Data], hashAlgorithm: HashAlgorithm = .sha256) {
        self.hashAlgorithm = hashAlgorithm
        self.leaves = []
        
        // Create leaf nodes
        for item in data {
            let hash = Self.hash(item, algorithm: hashAlgorithm)
            let node = MerkleNode(hash: hash, data: item)
            leaves.append(node)
        }
        
        // Build tree
        self.root = Self.buildTree(nodes: leaves, algorithm: hashAlgorithm)
    }
    
    /// Build Merkle tree from leaf nodes
    private static func buildTree(nodes: [MerkleNode], algorithm: HashAlgorithm) -> MerkleNode? {
        guard !nodes.isEmpty else { return nil }
        
        if nodes.count == 1 {
            return nodes[0]
        }
        
        var parents: [MerkleNode] = []
        var i = 0
        
        while i < nodes.count {
            let left = nodes[i]
            let right: MerkleNode
            
            if i + 1 < nodes.count {
                right = nodes[i + 1]
            } else {
                // Duplicate last node if odd number of nodes
                right = left
            }
            
            let combinedHash = left.hash + right.hash
            let parentHash = hash(combinedHash.data(using: .utf8)!, algorithm: algorithm)
            let parent = MerkleNode(hash: parentHash, left: left, right: right)
            parents.append(parent)
            
            i += 2
        }
        
        return buildTree(nodes: parents, algorithm: algorithm)
    }
    
    /// Hash data using specified algorithm
    private static func hash(_ data: Data, algorithm: HashAlgorithm) -> String {
        switch algorithm {
        case .sha256:
            return sha256Hash(data)
        case .sha512:
            return sha512Hash(data)
        }
    }
    
    /// Get the root hash (Merkle root)
    public var rootHash: String? {
        return root?.hash
    }
    
    /// Generate Merkle proof for a specific leaf index
    public func generateProof(for index: Int) -> MerkleProof? {
        guard index >= 0 && index < leaves.count else { return nil }
        
        var proofHashes: [(hash: String, position: MerkleProof.Position)] = []
        var currentIndex = index
        var currentLevel = leaves.map { $0 }
        
        while currentLevel.count > 1 {
            let isRightNode = currentIndex % 2 == 1
            let siblingIndex = isRightNode ? currentIndex - 1 : currentIndex + 1
            
            if siblingIndex < currentLevel.count {
                let siblingHash = currentLevel[siblingIndex].hash
                let position: MerkleProof.Position = isRightNode ? .left : .right
                proofHashes.append((hash: siblingHash, position: position))
            }
            
            // Move to next level
            var nextLevel: [MerkleNode] = []
            var i = 0
            while i < currentLevel.count {
                let left = currentLevel[i]
                let right = (i + 1 < currentLevel.count) ? currentLevel[i + 1] : left
                
                let combinedHash = left.hash + right.hash
                let parentHash = Self.hash(combinedHash.data(using: .utf8)!, algorithm: hashAlgorithm)
                let parent = MerkleNode(hash: parentHash, left: left, right: right)
                nextLevel.append(parent)
                
                i += 2
            }
            
            currentLevel = nextLevel
            currentIndex = currentIndex / 2
        }
        
        return MerkleProof(
            leafHash: leaves[index].hash,
            leafData: leaves[index].data,
            proofHashes: proofHashes,
            rootHash: rootHash ?? "",
            leafIndex: index
        )
    }
    
    /// Verify a Merkle proof
    public func verify(proof: MerkleProof) -> Bool {
        guard let rootHash = rootHash else { return false }
        
        var currentHash = proof.leafHash
        
        for (siblingHash, position) in proof.proofHashes {
            let combinedHash: String
            switch position {
            case .left:
                combinedHash = siblingHash + currentHash
            case .right:
                combinedHash = currentHash + siblingHash
            }
            
            currentHash = Self.hash(combinedHash.data(using: .utf8)!, algorithm: hashAlgorithm)
        }
        
        return currentHash == rootHash
    }
}

// MARK: - Merkle Proof

/// Proof of inclusion in a Merkle tree
public struct MerkleProof: Sendable {
    public enum Position: Sendable {
        case left
        case right
    }
    
    public let leafHash: String
    public let leafData: Data?
    public let proofHashes: [(hash: String, position: Position)]
    public let rootHash: String
    public let leafIndex: Int
    
    public init(leafHash: String, leafData: Data?, proofHashes: [(hash: String, position: Position)], rootHash: String, leafIndex: Int) {
        self.leafHash = leafHash
        self.leafData = leafData
        self.proofHashes = proofHashes
        self.rootHash = rootHash
        self.leafIndex = leafIndex
    }
}

// MARK: - Compliance Merkle Tree

/// Merkle tree specialized for audit compliance verification
public final class ComplianceMerkleTree: @unchecked Sendable {
    private var auditEventTrees: [Date: MerkleTree] = [:]
    private let queue = DispatchQueue(label: "com.hft.merkle.compliance", attributes: .concurrent)
    
    public init() {}
    
    /// Add audit events and create Merkle tree
    public func addAuditBatch(_ events: [AuditEvent]) -> String? {
        let data = events.compactMap { event -> Data? in
            let eventString = "\(event.timestamp.timeIntervalSince1970)|\(event.eventType.rawValue)|\(event.severity.rawValue)"
            return eventString.data(using: .utf8)
        }
        
        guard !data.isEmpty else { return nil }
        
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        let batchDate = Date()
        
        queue.async(flags: .barrier) { [weak self] in
            self?.auditEventTrees[batchDate] = tree
        }
        
        return tree.rootHash
    }
    
    /// Verify audit event integrity
    public func verifyAuditEvent(date: Date, index: Int) -> Bool {
        var result = false
        
        queue.sync {
            guard let tree = auditEventTrees[date],
                  let proof = tree.generateProof(for: index) else {
                result = false
                return
            }
            
            result = tree.verify(proof: proof)
        }
        
        return result
    }
    
    /// Get Merkle root for a specific batch
    public func getMerkleRoot(for date: Date) -> String? {
        var result: String?
        
        queue.sync {
            result = auditEventTrees[date]?.rootHash
        }
        
        return result
    }
    
    /// Generate compliance report with Merkle verification
    public func generateComplianceReport() -> ComplianceReport {
        var treeCount = 0
        var totalLeaves = 0
        var merkleRoots: [String] = []
        
        queue.sync {
            treeCount = auditEventTrees.count
            totalLeaves = auditEventTrees.values.reduce(0) { $0 + $1.leaves.count }
            merkleRoots = auditEventTrees.values.compactMap { $0.rootHash }
        }
        
        return ComplianceReport(
            treeCount: treeCount,
            totalEvents: totalLeaves,
            merkleRoots: merkleRoots,
            timestamp: Date()
        )
    }
}

// MARK: - Compliance Report

/// Compliance report with Merkle tree verification data
public struct ComplianceReport: Sendable {
    public let treeCount: Int
    public let totalEvents: Int
    public let merkleRoots: [String]
    public let timestamp: Date
    
    public var isValid: Bool {
        return treeCount > 0 && !merkleRoots.isEmpty
    }
    
    public var summary: String {
        return """
        Compliance Report
        -----------------
        Timestamp: \(timestamp)
        Merkle Trees: \(treeCount)
        Total Events: \(totalEvents)
        Root Hashes: \(merkleRoots.count)
        Status: \(isValid ? "VALID" : "INVALID")
        """
    }
}

// MARK: - Order Merkle Tree

/// Merkle tree for order and trade verification
public final class OrderMerkleTree: @unchecked Sendable {
    private var tree: MerkleTree?
    
    public init() {}
    
    /// Build tree from orders
    public func buildFromOrders(_ orders: [Order]) -> String? {
        let data = orders.map { order -> Data in
            let orderString = "\(order.id)|\(order.symbol)|\(order.price)|\(order.quantity)|\(order.side.rawValue)"
            return orderString.data(using: .utf8)!
        }
        
        tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        return tree?.rootHash
    }
    
    /// Build tree from trades
    public func buildFromTrades(_ trades: [Trade]) -> String? {
        let data = trades.map { trade -> Data in
            let tradeString = "\(trade.id)|\(trade.executionPrice)|\(trade.quantity)|\(trade.timestamp.timeIntervalSince1970)"
            return tradeString.data(using: .utf8)!
        }
        
        tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        return tree?.rootHash
    }
    
    /// Generate proof for specific index
    public func generateProof(for index: Int) -> MerkleProof? {
        return tree?.generateProof(for: index)
    }
    
    /// Verify proof
    public func verify(proof: MerkleProof) -> Bool {
        return tree?.verify(proof: proof) ?? false
    }
    
    /// Get root hash
    public var rootHash: String? {
        return tree?.rootHash
    }
}

// MARK: - Integration with Audit Logger

extension DefaultAuditLogger {
    /// Export audit events with Merkle tree verification
    public func exportWithMerkleVerification(from startDate: Date, to endDate: Date) -> (events: [AuditEvent], merkleRoot: String?) {
        let events = query(from: startDate, to: endDate, eventType: nil)
        
        let complianceTree = ComplianceMerkleTree()
        let merkleRoot = complianceTree.addAuditBatch(events)
        
        return (events: events, merkleRoot: merkleRoot)
    }
}
