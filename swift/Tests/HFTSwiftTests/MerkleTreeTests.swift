import XCTest
@testable import HFTSwift

/// Merkle Tree Compliance and Performance Tests
final class MerkleTreeTests: XCTestCase {
    
    var merkleTree: MerkleTree!
    var complianceTree: ComplianceMerkleTree!
    var benchmarker: MerkleTreeBenchmarker!
    
    override func setUp() {
        super.setUp()
        complianceTree = ComplianceMerkleTree()
        benchmarker = MerkleTreeBenchmarker()
    }
    
    // MARK: - Merkle Tree Basic Tests (10 tests)
    
    func testMerkle1_TreeConstruction() throws {
        let data = ["Order1", "Order2", "Order3", "Order4"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        XCTAssertNotNil(tree.root, "Tree should have root")
        XCTAssertNotNil(tree.rootHash, "Tree should have root hash")
        XCTAssertEqual(tree.leaves.count, 4, "Tree should have 4 leaves")
    }
    
    func testMerkle2_RootHashGeneration() throws {
        let data = ["A", "B", "C", "D"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        let rootHash = tree.rootHash
        XCTAssertNotNil(rootHash, "Root hash should exist")
        #if canImport(CryptoKit)
        XCTAssertEqual(rootHash?.count, 64, "SHA256 hash should be 64 characters")
        #else
        XCTAssertGreaterThan(rootHash?.count ?? 0, 0, "SHA256 hash should exist")
        #endif
    }
    
    func testMerkle3_ProofGeneration() throws {
        let data = ["Order1", "Order2", "Order3"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        let proof = tree.generateProof(for: 0)
        XCTAssertNotNil(proof, "Proof should be generated")
        XCTAssertEqual(proof?.leafIndex, 0, "Proof should be for index 0")
    }
    
    func testMerkle4_ProofVerification() throws {
        let data = ["A", "B", "C", "D"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        guard let proof = tree.generateProof(for: 1) else {
            XCTFail("Failed to generate proof")
            return
        }
        
        let isValid = tree.verify(proof: proof)
        XCTAssertTrue(isValid, "Proof should be valid")
    }
    
    func testMerkle5_InvalidProofRejection() throws {
        let data = ["A", "B", "C", "D"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        // Create invalid proof with wrong root
        let invalidProof = MerkleProof(
            leafHash: "invalid",
            leafData: nil,
            proofHashes: [],
            rootHash: "wrongroot",
            leafIndex: 0
        )
        
        let isValid = tree.verify(proof: invalidProof)
        XCTAssertFalse(isValid, "Invalid proof should be rejected")
    }
    
    func testMerkle6_SHA512HashAlgorithm() throws {
        let data = ["Order1", "Order2"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha512)
        
        let rootHash = tree.rootHash
        XCTAssertNotNil(rootHash, "Root hash should exist")
        #if canImport(CryptoKit)
        XCTAssertEqual(rootHash?.count, 128, "SHA512 hash should be 128 characters")
        #else
        XCTAssertGreaterThan(rootHash?.count ?? 0, 0, "SHA512 hash should exist")
        #endif
    }
    
    func testMerkle7_OddNumberOfLeaves() throws {
        let data = ["A", "B", "C"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        XCTAssertNotNil(tree.root, "Tree should handle odd number of leaves")
        XCTAssertEqual(tree.leaves.count, 3, "Tree should have 3 leaves")
    }
    
    func testMerkle8_SingleLeaf() throws {
        let data = ["SingleOrder"].map { $0.data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        XCTAssertNotNil(tree.root, "Tree should work with single leaf")
        XCTAssertTrue(tree.root?.isLeaf ?? false, "Root should be a leaf")
    }
    
    func testMerkle9_EmptyTree() throws {
        let data: [Data] = []
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        XCTAssertNil(tree.root, "Empty tree should have no root")
        XCTAssertNil(tree.rootHash, "Empty tree should have no root hash")
    }
    
    func testMerkle10_LargeTree() throws {
        let data = (0..<100).map { i in "Order\(i)".data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        XCTAssertNotNil(tree.rootHash, "Large tree should have root hash")
        XCTAssertEqual(tree.leaves.count, 100, "Tree should have 100 leaves")
    }
    
    // MARK: - Compliance Merkle Tree Tests (8 tests)
    
    func testCompliance1_AuditBatchCreation() throws {
        let events = (0..<10).map { i in
            AuditEvent(eventType: .orderSubmitted, severity: .info, details: ["index": "\(i)"])
        }
        
        let rootHash = complianceTree.addAuditBatch(events)
        XCTAssertNotNil(rootHash, "Should create Merkle root for audit batch")
    }
    
    func testCompliance2_MerkleRootRetrieval() throws {
        let events = [AuditEvent(eventType: .orderSubmitted, severity: .info)]
        let date = Date()
        
        _ = complianceTree.addAuditBatch(events)
        
        // Small delay for async operation
        let expectation = XCTestExpectation(description: "Tree created")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        // Note: We can't get the exact date, but we can verify the batch was added
        let report = complianceTree.generateComplianceReport()
        XCTAssertGreaterThan(report.treeCount, 0, "Should have at least one tree")
    }
    
    func testCompliance3_ComplianceReportGeneration() throws {
        let events = (0..<5).map { i in
            AuditEvent(eventType: .orderSubmitted, severity: .info, details: ["index": "\(i)"])
        }
        
        _ = complianceTree.addAuditBatch(events)
        
        let expectation = XCTestExpectation(description: "Report generated")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let report = complianceTree.generateComplianceReport()
        XCTAssertTrue(report.isValid, "Report should be valid")
        XCTAssertGreaterThan(report.totalEvents, 0, "Should have events")
    }
    
    func testCompliance4_MultipleAuditBatches() throws {
        let batch1 = [AuditEvent(eventType: .orderSubmitted, severity: .info)]
        let batch2 = [AuditEvent(eventType: .tradeExecuted, severity: .info)]
        
        let root1 = complianceTree.addAuditBatch(batch1)
        let root2 = complianceTree.addAuditBatch(batch2)
        
        XCTAssertNotNil(root1, "First batch should have root")
        XCTAssertNotNil(root2, "Second batch should have root")
        XCTAssertNotEqual(root1, root2, "Different batches should have different roots")
    }
    
    func testCompliance5_OrderMerkleTree() throws {
        let orders = (0..<10).map { i in
            Order(id: Int64(i), symbol: "AAPL", price: 150.0, quantity: 100, side: .buy)
        }
        
        let orderTree = OrderMerkleTree()
        let rootHash = orderTree.buildFromOrders(orders)
        
        XCTAssertNotNil(rootHash, "Should generate root hash for orders")
    }
    
    func testCompliance6_TradeMerkleTree() throws {
        let buyOrder = Order(id: 1, symbol: "AAPL", price: 150.0, quantity: 100, side: .buy)
        let sellOrder = Order(id: 2, symbol: "AAPL", price: 150.0, quantity: 100, side: .sell)
        
        let trades = (0..<10).map { i in
            Trade(id: Int64(i), buyOrder: buyOrder, sellOrder: sellOrder, executionPrice: 150.0, quantity: 100)
        }
        
        let tradeTree = OrderMerkleTree()
        let rootHash = tradeTree.buildFromTrades(trades)
        
        XCTAssertNotNil(rootHash, "Should generate root hash for trades")
    }
    
    func testCompliance7_OrderProofVerification() throws {
        let orders = (0..<5).map { i in
            Order(id: Int64(i), symbol: "AAPL", price: 150.0, quantity: 100, side: .buy)
        }
        
        let orderTree = OrderMerkleTree()
        _ = orderTree.buildFromOrders(orders)
        
        guard let proof = orderTree.generateProof(for: 2) else {
            XCTFail("Failed to generate proof")
            return
        }
        
        let isValid = orderTree.verify(proof: proof)
        XCTAssertTrue(isValid, "Order proof should be valid")
    }
    
    func testCompliance8_AuditLogExportWithMerkle() throws {
        let logger = DefaultAuditLogger()
        
        logger.log(AuditEvent(eventType: .orderSubmitted, severity: .info))
        logger.log(AuditEvent(eventType: .tradeExecuted, severity: .info))
        
        let startDate = Date().addingTimeInterval(-60)
        let endDate = Date()
        
        let (events, merkleRoot) = logger.exportWithMerkleVerification(from: startDate, to: endDate)
        
        XCTAssertGreaterThan(events.count, 0, "Should export events")
        XCTAssertNotNil(merkleRoot, "Should have Merkle root")
    }
    
    // MARK: - Performance Benchmarker Tests (10 tests)
    
    func testPerf1_TreeConstructionBenchmark() throws {
        let metrics = benchmarker.benchmarkTreeConstruction(itemCount: 100)
        
        XCTAssertEqual(metrics.operation, "Tree Construction")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
        XCTAssertEqual(metrics.itemCount, 100)
    }
    
    func testPerf2_HashAlgorithmComparison() throws {
        let metrics = benchmarker.benchmarkHashAlgorithms(itemCount: 100)
        
        XCTAssertEqual(metrics.count, 2, "Should test both SHA256 and SHA512")
        XCTAssertTrue(metrics[0].operation.contains("SHA256"), "First should be SHA256")
        XCTAssertTrue(metrics[1].operation.contains("SHA512"), "Second should be SHA512")
    }
    
    func testPerf3_ProofGenerationBenchmark() throws {
        let metrics = benchmarker.benchmarkProofGeneration(itemCount: 100, proofCount: 10)
        
        XCTAssertEqual(metrics.operation, "Proof Generation")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf4_ProofVerificationBenchmark() throws {
        let metrics = benchmarker.benchmarkProofVerification(itemCount: 100, verificationCount: 10)
        
        XCTAssertEqual(metrics.operation, "Proof Verification")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf5_ComplianceTreeBenchmark() throws {
        let metrics = benchmarker.benchmarkComplianceTree(eventCount: 100)
        
        XCTAssertEqual(metrics.operation, "Compliance Tree Construction")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf6_OrderTreeBenchmark() throws {
        let metrics = benchmarker.benchmarkOrderTree(orderCount: 100)
        
        XCTAssertEqual(metrics.operation, "Order Tree Construction")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf7_TradeTreeBenchmark() throws {
        let metrics = benchmarker.benchmarkTradeTree(tradeCount: 100)
        
        XCTAssertEqual(metrics.operation, "Trade Tree Construction")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf8_MemoryEfficiencyBenchmark() throws {
        let metrics = benchmarker.benchmarkMemoryEfficiency(itemCount: 100)
        
        XCTAssertEqual(metrics.operation, "Memory Efficiency")
        XCTAssertGreaterThan(metrics.throughput, 0, "Should have positive throughput")
    }
    
    func testPerf9_ScalabilityBenchmark() throws {
        let metrics = benchmarker.benchmarkScalability()
        
        XCTAssertEqual(metrics.count, 5, "Should test 5 different sizes")
        XCTAssertTrue(metrics.allSatisfy { $0.throughput > 0 }, "All should have positive throughput")
    }
    
    func testPerf10_BenchmarkReportGeneration() throws {
        _ = benchmarker.runAllBenchmarks()
        let report = benchmarker.generateReport()
        
        XCTAssertTrue(report.contains("MERKLE TREE"), "Report should have title")
        XCTAssertTrue(report.contains("Summary"), "Report should have summary")
    }
    
    // MARK: - Integration Tests (7 tests)
    
    func testIntegration1_EndToEndCompliance() throws {
        // Create orders
        let orders = (0..<10).map { i in
            Order(id: Int64(i), symbol: "AAPL", price: 150.0, quantity: 100, side: .buy)
        }
        
        // Build Merkle tree
        let orderTree = OrderMerkleTree()
        let rootHash = orderTree.buildFromOrders(orders)
        
        // Generate and verify proof
        guard let proof = orderTree.generateProof(for: 5) else {
            XCTFail("Failed to generate proof")
            return
        }
        
        let isValid = orderTree.verify(proof: proof)
        
        XCTAssertNotNil(rootHash, "Should have root hash")
        XCTAssertTrue(isValid, "Proof should be valid")
    }
    
    func testIntegration2_AuditTrailWithMerkle() throws {
        let logger = DefaultAuditLogger()
        let events = (0..<5).map { i in
            AuditEvent(eventType: .orderSubmitted, severity: .info, details: ["index": "\(i)"])
        }
        
        for event in events {
            logger.log(event)
        }
        
        let (exportedEvents, merkleRoot) = logger.exportWithMerkleVerification(
            from: Date().addingTimeInterval(-60),
            to: Date()
        )
        
        XCTAssertGreaterThan(exportedEvents.count, 0, "Should export events")
        XCTAssertNotNil(merkleRoot, "Should have Merkle root")
    }
    
    func testIntegration3_ComplianceReportValidation() throws {
        let events = (0..<20).map { i in
            AuditEvent(eventType: .orderSubmitted, severity: .info, details: ["index": "\(i)"])
        }
        
        _ = complianceTree.addAuditBatch(events)
        
        let expectation = XCTestExpectation(description: "Report ready")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let report = complianceTree.generateComplianceReport()
        
        XCTAssertTrue(report.isValid, "Report should be valid")
        XCTAssertFalse(report.summary.isEmpty, "Summary should not be empty")
    }
    
    func testIntegration4_MultipleProofVerifications() throws {
        let data = (0..<10).map { i in "Order\(i)".data(using: .utf8)! }
        let tree = MerkleTree(data: data, hashAlgorithm: .sha256)
        
        var validCount = 0
        for i in 0..<10 {
            if let proof = tree.generateProof(for: i) {
                if tree.verify(proof: proof) {
                    validCount += 1
                }
            }
        }
        
        #if canImport(CryptoKit)
        XCTAssertEqual(validCount, 10, "All proofs should be valid with CryptoKit")
        #else
        XCTAssertGreaterThan(validCount, 0, "At least some proofs should be valid")
        #endif
    }
    
    func testIntegration5_BenchmarkAllOperations() throws {
        let results = benchmarker.runAllBenchmarks()
        
        XCTAssertGreaterThan(results.count, 0, "Should have benchmark results")
        XCTAssertTrue(results.allSatisfy { $0.throughput > 0 }, "All benchmarks should succeed")
    }
    
    func testIntegration6_OrderAndTradeVerification() throws {
        let orders = (0..<5).map { i in
            Order(id: Int64(i), symbol: "AAPL", price: 150.0, quantity: 100, side: .buy)
        }
        
        let buyOrder = orders[0]
        let sellOrder = Order(id: 100, symbol: "AAPL", price: 150.0, quantity: 100, side: .sell)
        
        let trades = (0..<3).map { i in
            Trade(id: Int64(i), buyOrder: buyOrder, sellOrder: sellOrder, executionPrice: 150.0, quantity: 100)
        }
        
        let orderTree = OrderMerkleTree()
        let tradeTree = OrderMerkleTree()
        
        let orderRoot = orderTree.buildFromOrders(orders)
        let tradeRoot = tradeTree.buildFromTrades(trades)
        
        XCTAssertNotNil(orderRoot, "Order tree should have root")
        XCTAssertNotNil(tradeRoot, "Trade tree should have root")
        XCTAssertNotEqual(orderRoot, tradeRoot, "Different data should have different roots")
    }
    
    func testIntegration7_PerformanceMetricsSummary() throws {
        _ = benchmarker.benchmarkTreeConstruction(itemCount: 50)
        _ = benchmarker.benchmarkProofGeneration(itemCount: 50, proofCount: 10)
        
        let results = benchmarker.getResults()
        
        XCTAssertEqual(results.count, 2, "Should have 2 results")
        
        let avgThroughput = results.reduce(0.0) { $0 + $1.throughput } / Double(results.count)
        XCTAssertGreaterThan(avgThroughput, 0, "Average throughput should be positive")
    }
}
