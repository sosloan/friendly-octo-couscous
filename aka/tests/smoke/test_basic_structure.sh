#!/bin/bash
# AKA Smoke Test - Basic System Validation

# This test verifies that all basic system components are present and accessible

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

echo "Running AKA Smoke Tests..."

# Test 1: Check directory structure
echo "  Checking directory structure..."
[ -d "${PROJECT_ROOT}/ada" ] || exit 1
[ -d "${PROJECT_ROOT}/java" ] || exit 1
[ -d "${PROJECT_ROOT}/erlang" ] || exit 1
[ -d "${PROJECT_ROOT}/akka" ] || exit 1
[ -d "${PROJECT_ROOT}/lean" ] || exit 1

# Test 2: Check key files
echo "  Checking key files..."
[ -f "${PROJECT_ROOT}/Makefile" ] || exit 1
[ -f "${PROJECT_ROOT}/README.md" ] || exit 1

# Test 3: Check Ada test files
echo "  Checking Ada test files..."
[ -f "${PROJECT_ROOT}/ada/hft_test.adb" ] || exit 1
[ -f "${PROJECT_ROOT}/ada/hft_engine.ads" ] || exit 1

# Test 4: Check Java test files
echo "  Checking Java test files..."
[ -f "${PROJECT_ROOT}/java/src/test/java/com/hft/java/OrderTest.java" ] || exit 1

# Test 5: Check Erlang test files
echo "  Checking Erlang test files..."
[ -f "${PROJECT_ROOT}/erlang/test/hft_order_processor_tests.erl" ] || exit 1

echo "✓ All smoke tests passed"
exit 0
