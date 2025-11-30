// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.
// Updated to Swift 6.0+ with Distributed Actors support for high-frequency trading

import PackageDescription

let package = Package(
    name: "HFTSwift",
    platforms: [
        .iOS(.v18),
        .macOS(.v15),
        .visionOS(.v2)
    ],
    products: [
        .library(
            name: "HFTSwift",
            targets: ["HFTSwift"]),
        .executable(
            name: "HFTSwiftDemo",
            targets: ["HFTSwiftDemo"])
    ],
    dependencies: [
        // Swift Distributed Actors cluster for distributed trading system
        // Using main branch for latest Swift 6 compatibility
        .package(url: "https://github.com/apple/swift-distributed-actors.git", branch: "main")
    ],
    targets: [
        .target(
            name: "HFTSwift",
            dependencies: [
                .product(name: "DistributedCluster", package: "swift-distributed-actors")
            ],
            swiftSettings: [
                .enableExperimentalFeature("StrictConcurrency")
            ]
        ),
        .executableTarget(
            name: "HFTSwiftDemo",
            dependencies: ["HFTSwift"]),
        .testTarget(
            name: "HFTSwiftTests",
            dependencies: ["HFTSwift"])
    ]
)
