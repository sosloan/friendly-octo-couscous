// swift-tools-version: 5.9
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "HFTSwift",
    platforms: [
        .iOS(.v17),
        .macOS(.v14),
        .visionOS(.v1)
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
        // Add any Swift package dependencies here
    ],
    targets: [
        .target(
            name: "HFTSwift",
            dependencies: []),
        .executableTarget(
            name: "HFTSwiftDemo",
            dependencies: ["HFTSwift"]),
        .testTarget(
            name: "HFTSwiftTests",
            dependencies: ["HFTSwift"])
    ]
)
