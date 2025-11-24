import XCTest
@testable import HFTSwift

/// Attribution Chain Tests
final class AttributionChainTests: XCTestCase {
    
    var attributionChain: AttributionChain!
    
    override func setUp() {
        super.setUp()
        attributionChain = AttributionChain()
    }
    
    // MARK: - Basic Attribution Tests (10 tests)
    
    func testAttribution1_NodeCreation() throws {
        let node = AttributionNode(
            component: "Test Component",
            contributionType: .originalDesign,
            originalityLevel: .high,
            originalityPercentage: 85.0,
            description: "Test description"
        )
        
        XCTAssertNotNil(node.id, "Node should have ID")
        XCTAssertEqual(node.component, "Test Component")
        XCTAssertEqual(node.originalityPercentage, 85.0)
    }
    
    func testAttribution2_AddNode() throws {
        let node = AttributionNode(
            component: "Component A",
            contributionType: .originalDesign,
            originalityLevel: .high,
            originalityPercentage: 90.0,
            description: "Original component"
        )
        
        attributionChain.addNode(node)
        
        // Give async operation time
        let expectation = XCTestExpectation(description: "Node added")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let retrieved = attributionChain.getNode(id: node.id)
        XCTAssertNotNil(retrieved, "Should retrieve added node")
        XCTAssertEqual(retrieved?.component, "Component A")
    }
    
    func testAttribution3_GetNodesByComponent() throws {
        let node1 = AttributionNode(
            component: "Component B",
            contributionType: .originalDesign,
            originalityLevel: .high,
            originalityPercentage: 80.0,
            description: "First node"
        )
        
        let node2 = AttributionNode(
            component: "Component B",
            contributionType: .algorithmAdaptation,
            originalityLevel: .medium,
            originalityPercentage: 60.0,
            description: "Second node"
        )
        
        attributionChain.addNode(node1)
        attributionChain.addNode(node2)
        
        let expectation = XCTestExpectation(description: "Nodes added")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let nodes = attributionChain.getNodes(forComponent: "Component B")
        XCTAssertEqual(nodes.count, 2, "Should retrieve both nodes")
    }
    
    func testAttribution4_CalculateOriginality() throws {
        let node1 = AttributionNode(
            component: "Component C",
            contributionType: .originalDesign,
            originalityLevel: .high,
            originalityPercentage: 80.0,
            description: "Node 1"
        )
        
        let node2 = AttributionNode(
            component: "Component C",
            contributionType: .originalDesign,
            originalityLevel: .medium,
            originalityPercentage: 60.0,
            description: "Node 2"
        )
        
        attributionChain.addNode(node1)
        attributionChain.addNode(node2)
        
        let expectation = XCTestExpectation(description: "Calculation ready")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let avgOriginality = attributionChain.calculateOriginality(forComponent: "Component C")
        XCTAssertEqual(avgOriginality, 70.0, accuracy: 0.1, "Should calculate average")
    }
    
    func testAttribution5_AcademicReference() throws {
        let reference = AcademicReference(
            authors: ["Smith, J.", "Doe, A."],
            title: "Advanced Trading Systems",
            publication: "Journal of Finance",
            year: 2020,
            doi: "10.1234/example"
        )
        
        XCTAssertEqual(reference.authors.count, 2)
        XCTAssertTrue(reference.citation.contains("Smith, J., Doe, A. (2020)"))
    }
    
    func testAttribution6_AttributionChain() throws {
        let parent = AttributionNode(
            component: "Parent",
            contributionType: .originalDesign,
            originalityLevel: .high,
            originalityPercentage: 85.0,
            description: "Parent node"
        )
        
        let child = AttributionNode(
            component: "Child",
            contributionType: .algorithmAdaptation,
            originalityLevel: .medium,
            originalityPercentage: 65.0,
            description: "Child node",
            parentNodes: [parent.id]
        )
        
        attributionChain.addNode(parent)
        attributionChain.addNode(child)
        
        let expectation = XCTestExpectation(description: "Chain built")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            expectation.fulfill()
        }
        wait(for: [expectation], timeout: 1.0)
        
        let chain = attributionChain.getChain(forNode: child.id)
        XCTAssertGreaterThan(chain.count, 0, "Should have chain nodes")
    }
    
    func testAttribution7_OriginalityLevels() throws {
        XCTAssertEqual(OriginalityLevel.high.rawValue, "High (>75%)")
        XCTAssertEqual(OriginalityLevel.medium.rawValue, "Medium (40-75%)")
        XCTAssertEqual(OriginalityLevel.low.rawValue, "Low (<40%)")
    }
    
    func testAttribution8_ContributionTypes() throws {
        XCTAssertEqual(ContributionType.originalDesign.rawValue, "Original Design")
        XCTAssertEqual(ContributionType.algorithmAdaptation.rawValue, "Algorithm Adaptation")
        XCTAssertEqual(ContributionType.standardPattern.rawValue, "Standard Pattern")
    }
    
    func testAttribution9_GenerateReport() throws {
        let report = attributionChain.generateReport()
        
        XCTAssertGreaterThan(report.components.count, 0, "Should have components")
        XCTAssertGreaterThan(report.totalNodes, 0, "Should have nodes")
        XCTAssertFalse(report.summary.isEmpty, "Summary should not be empty")
    }
    
    func testAttribution10_SystemInitialization() throws {
        // System should be pre-initialized with components
        let reactiveEngine = attributionChain.getNodes(forComponent: "Swift Combine Reactive Engine")
        XCTAssertGreaterThan(reactiveEngine.count, 0, "Should have reactive engine attribution")
    }
    
    // MARK: - Academic Reference Tests (5 tests)
    
    func testAcademic1_CitationFormat() throws {
        let ref = AcademicReference(
            authors: ["Merkle, R.C."],
            title: "Digital Signatures",
            publication: "CRYPTO '87",
            year: 1987
        )
        
        let citation = ref.citation
        XCTAssertTrue(citation.contains("Merkle, R.C."))
        XCTAssertTrue(citation.contains("(1987)"))
        XCTAssertTrue(citation.contains("Digital Signatures"))
    }
    
    func testAcademic2_MultipleAuthors() throws {
        let ref = AcademicReference(
            authors: ["Author A", "Author B", "Author C"],
            title: "Collaborative Research",
            publication: "Journal",
            year: 2020
        )
        
        XCTAssertEqual(ref.authors.count, 3)
        XCTAssertTrue(ref.citation.contains("Author A, Author B, Author C"))
    }
    
    func testAcademic3_DOIIncluded() throws {
        let ref = AcademicReference(
            authors: ["Smith, J."],
            title: "Test Paper",
            publication: "Nature",
            year: 2021,
            doi: "10.1038/test"
        )
        
        XCTAssertEqual(ref.doi, "10.1038/test")
    }
    
    func testAcademic4_GetAllReferences() throws {
        let references = attributionChain.getAllReferences()
        XCTAssertGreaterThan(references.count, 0, "Should have system references")
    }
    
    func testAcademic5_ExportCitations() throws {
        let citations = attributionChain.exportCitations()
        XCTAssertFalse(citations.isEmpty, "Should export citations")
        XCTAssertTrue(citations.contains("ACADEMIC REFERENCES"))
    }
    
    // MARK: - Report Generation Tests (5 tests)
    
    func testReport1_ComponentAttribution() throws {
        let report = attributionChain.generateReport()
        
        XCTAssertFalse(report.components.isEmpty, "Should have component attributions")
        
        for comp in report.components {
            XCTAssertGreaterThanOrEqual(comp.averageOriginality, 0.0)
            XCTAssertLessThanOrEqual(comp.averageOriginality, 100.0)
        }
    }
    
    func testReport2_SummaryGeneration() throws {
        let report = attributionChain.generateReport()
        let summary = report.summary
        
        XCTAssertTrue(summary.contains("ATTRIBUTION CHAIN"))
        XCTAssertTrue(summary.contains("Swift HFT System"))
        XCTAssertTrue(summary.contains("Overall System Originality"))
    }
    
    func testReport3_OriginalityBreakdown() throws {
        let breakdown = attributionChain.getBreakdownByType()
        
        XCTAssertFalse(breakdown.isEmpty, "Should have breakdown by type")
        XCTAssertTrue(breakdown.keys.contains(.originalDesign))
    }
    
    func testReport4_ComponentSorting() throws {
        let report = attributionChain.generateReport()
        
        // Components should be sorted by originality (descending)
        for i in 0..<(report.components.count - 1) {
            XCTAssertGreaterThanOrEqual(
                report.components[i].averageOriginality,
                report.components[i + 1].averageOriginality,
                "Components should be sorted by originality"
            )
        }
    }
    
    func testReport5_JSONExport() throws {
        let jsonData = attributionChain.exportJSON()
        XCTAssertNotNil(jsonData, "Should export JSON")
        
        if let data = jsonData {
            XCTAssertGreaterThan(data.count, 0, "JSON should not be empty")
        }
    }
    
    // MARK: - Integration Tests (5 tests)
    
    func testIntegration1_SystemAttributions() throws {
        // Verify key system components are attributed
        let merkle = attributionChain.getNodes(forComponent: "Merkle Tree Compliance")
        XCTAssertGreaterThan(merkle.count, 0, "Merkle tree should be attributed")
        
        let reactive = attributionChain.getNodes(forComponent: "Swift Combine Reactive Engine")
        XCTAssertGreaterThan(reactive.count, 0, "Reactive engine should be attributed")
    }
    
    func testIntegration2_OriginalityCalculation() throws {
        let originalityMerkle = attributionChain.calculateOriginality(forComponent: "Merkle Tree Compliance")
        XCTAssertGreaterThan(originalityMerkle, 0.0, "Should have originality score")
        XCTAssertLessThanOrEqual(originalityMerkle, 100.0, "Should be valid percentage")
    }
    
    func testIntegration3_FullReportGeneration() throws {
        let report = attributionChain.generateReport()
        
        XCTAssertGreaterThan(report.totalNodes, 5, "Should have multiple nodes")
        XCTAssertGreaterThan(report.components.count, 3, "Should have multiple components")
        
        // Verify report contains expected sections
        let summary = report.summary
        XCTAssertTrue(summary.contains("Total Attribution Nodes"))
        XCTAssertTrue(summary.contains("Components Analyzed"))
    }
    
    func testIntegration4_AcademicReferencesLinked() throws {
        let merkleNodes = attributionChain.getNodes(forComponent: "Merkle Tree Compliance")
        
        if let node = merkleNodes.first {
            XCTAssertGreaterThan(node.academicReferences.count, 0, "Should have academic references")
        }
    }
    
    func testIntegration5_ContributionTypeDistribution() throws {
        let breakdown = attributionChain.getBreakdownByType()
        
        // System should have mix of contribution types
        XCTAssertTrue(breakdown.keys.contains(.originalDesign), "Should have original designs")
        
        // Original designs should generally have higher originality
        if let originalDesignScore = breakdown[.originalDesign],
           let standardPatternScore = breakdown[.standardPattern] {
            XCTAssertGreaterThan(originalDesignScore, standardPatternScore, "Original designs should score higher")
        }
    }
}
