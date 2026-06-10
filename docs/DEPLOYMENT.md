# Cross-Platform Deployment Guide

## Supported Platforms

### Desktop Operating Systems
- ✅ Linux (Ubuntu 20.04+, RHEL 8+, Debian 11+)
- ✅ macOS 11+ (Intel and Apple Silicon)
- ✅ Windows 10+ (via WSL2 or native builds)

### Mobile & Tablet
- 📱 iOS 14+ / iPadOS 14+
- 🤖 Android 10+ (API level 29+)
- 💻 MacBook Air (M1/M2/M3 native support)

## Platform-Specific Instructions

### macOS (including MacBook Air)

#### Prerequisites
```bash
# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install dependencies
brew install gnat
brew install lean
brew install openjdk@25
brew install erlang
brew install gradle
```

#### Build
```bash
make all
```

#### Native ARM64 (M1/M2/M3)
All components compile natively for Apple Silicon:
- Ada: Uses ARM64-optimized GNAT
- Java: Native ARM64 JIT
- Erlang: Native ARM64 BEAM
- Akka: JVM scales to ARM64

### Linux

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install -y gnat gprbuild
sudo apt install -y openjdk-25-jdk
sudo apt install -y erlang

# Lean
curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh

# Gradle
sdk install gradle
```

#### Build
```bash
make all
```

### Windows

#### Using WSL2
1. Install WSL2 with Ubuntu
2. Follow Linux instructions above

#### Native Windows
```bash
# Install GNAT from AdaCore
# Install Java from Adoptium
# Install Erlang from erlang.org

make all  # Using MSYS2 or Git Bash
```

### iOS / iPadOS

#### Prerequisites
- Xcode 14+
- iOS SDK
- Swift toolchain

#### Build for iOS
```bash
# Swift bridge for Ada/Java components
cd ios
xcodebuild -scheme HFT-iOS -configuration Release -sdk iphoneos

# Or use Xcode directly
open HFT-iOS.xcodeproj
```

#### Capabilities
- Native Swift UI
- Background processing for order monitoring
- Push notifications for trade execution
- Touch ID / Face ID for authentication

### Android

#### Prerequisites
- Android Studio
- Android SDK (API 29+)
- Android NDK for native components

#### Build for Android
```bash
cd android
./gradlew assembleRelease

# Or use Android Studio
# Open android/ directory
```

#### Features
- Material Design 3 UI
- Background services for order processing
- Biometric authentication
- Notifications for trade alerts

### Tablet-Specific Optimizations

#### iPad
- Split-view support for multi-tasking
- Apple Pencil support for charting
- Landscape-optimized layouts
- Stage Manager compatibility

#### Android Tablets
- Responsive layouts for various screen sizes
- Stylus support
- Foldable device optimization
- DeX mode support (Samsung)

## Docker Deployment

### Build Container
```bash
docker build -t hft-system .
docker run -p 8080:8080 hft-system
```

### Docker Compose
```bash
docker-compose up -d
```

## Cloud Deployment

### AWS
```bash
# ECS/Fargate deployment
aws ecs create-cluster --cluster-name hft-cluster
```

### Google Cloud
```bash
# GKE deployment
gcloud container clusters create hft-cluster
```

### Azure
```bash
# AKS deployment
az aks create --name hft-cluster
```

## Performance Tuning by Platform

### Linux
- PREEMPT_RT kernel for real-time
- CPU pinning for latency-sensitive threads
- Huge pages for memory performance

### macOS
- Metal for GPU acceleration
- Grand Central Dispatch optimization
- Network.framework for low latency

### iOS/iPadOS
- Background processing entitlements
- Network.framework for efficient I/O
- Combine for reactive streams

### Android
- WorkManager for background tasks
- Kotlin coroutines for concurrency
- Android NDK for critical paths

## Monitoring

All platforms support:
- Prometheus metrics export
- OpenTelemetry tracing
- Structured JSON logging
- Health check endpoints

## Security

- TLS 1.3 for all network communication
- Certificate pinning on mobile
- Biometric authentication
- Encrypted storage for sensitive data
