// swift-tools-version:5.2

import PackageDescription

let package = Package(
    name: "CLMetal",
    products: [
        .library(
            name: "CLMetal",
            type: .dynamic,
            targets: ["CLMetal"]),
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "CLMetal",
            dependencies: []),
        .testTarget(
          name: "CLMetalTest",
          dependencies: ["CLMetal"])
    ]
)

