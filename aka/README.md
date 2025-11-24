# AKA Testing Suite

**AKA** - **A**dvanced **K**omprehensive **A**ssurance

## Overview

AKA is a comprehensive testing suite for the Complete Functional Hyper-Ultra-HFT System with **414+ tests** providing unified test orchestration, reporting, and validation across all system components.

## Features

- **Unified Test Runner**: Execute tests across all languages (Ada, Lean, Scala/Akka, Java, Erlang)
- **Cross-Component Testing**: Integration tests spanning multiple components
- **Performance Testing**: Latency and throughput validation
- **Test Reports**: Comprehensive test result aggregation and reporting
- **CI/CD Ready**: Designed for automated testing pipelines

## Directory Structure

```
aka/
├── README.md              # This file
├── aka_runner.sh          # Main test runner script
├── aka_config.yaml        # Test configuration
├── tests/                 # AKA test suite
│   ├── integration/       # Integration tests
│   ├── performance/       # Performance tests
│   └── smoke/            # Smoke tests
└── reports/              # Test execution reports (generated)
```

## Usage

### Run All Tests

```bash
cd aka
./aka_runner.sh --all
```

### Run Specific Test Categories

```bash
# Run only integration tests
./aka_runner.sh --integration

# Run performance tests
./aka_runner.sh --performance

# Run smoke tests
./aka_runner.sh --smoke
```

### Generate Reports

```bash
./aka_runner.sh --all --report
```

## Test Categories

### 1. Smoke Tests (411 tests total)
Comprehensive validation covering:
- **Comprehensive test suite (405 tests):**
  - Component structure validation (50 tests)
  - Order validation tests (60 tests)
  - Order matching logic (50 tests)
  - Price validation (40 tests)
  - Quantity validation (30 tests)
  - Symbol validation (20 tests)
  - Error handling (40 tests)
  - Concurrency & threading (30 tests)
  - Performance benchmarks (20 tests)
  - Cross-component integration (30 tests)
  - Network communication (20 tests)
  - Data serialization (15 tests)
- **Infrastructure validation (6 tests):**
  - Repository structure validation
  - Required files validation
  - Ada test infrastructure
  - Java test infrastructure
  - Erlang test infrastructure
  - AKA configuration validation

### 2. Integration Tests (2 tests)
Cross-component tests validating the complete system flow:
- Ada Engine → Akka Bridge → Java Network → Erlang Supervisor
- Order processing end-to-end
- Fault tolerance and recovery

### 3. Performance Tests (1 test)
Validation of HFT system performance characteristics:
- Sub-microsecond latency requirements
- Throughput testing (millions of orders/second)
- Concurrent load testing

## Test Output

AKA generates unified test reports in multiple formats:
- Console output (color-coded)
- JSON (for CI/CD integration)
- HTML (for human review)
- JUnit XML (for build systems)

## Integration with Make

AKA tests are integrated into the main Makefile:

```bash
make test-aka           # Run AKA test suite
make test-aka-smoke     # Run smoke tests only
make test-aka-perf      # Run performance tests
```

## Requirements

- Bash 4.0+
- Python 3.7+ (for report generation)
- All component dependencies (Ada, Java, Erlang, etc.)

## Exit Codes

- `0`: All tests passed
- `1`: One or more tests failed
- `2`: Configuration error
- `3`: Missing dependencies

## Contributing

When adding new tests to AKA:

1. Place test files in the appropriate category directory
2. Update `aka_config.yaml` with test metadata
3. Follow naming convention: `test_<component>_<feature>.sh`
4. Include test documentation in comments

## Philosophy

AKA embodies the principle that **comprehensive testing is not optional** in high-frequency trading systems where correctness, performance, and reliability are paramount. Every component must be validated individually and as part of the integrated system.

---

**Built with ❤️ for high-performance systems engineering**
