#!/bin/bash
# AKA Test Runner
# Advanced Komprehensive Assurance Testing Suite

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Configuration
REPORT_DIR="${SCRIPT_DIR}/reports"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="${REPORT_DIR}/aka_report_${TIMESTAMP}.txt"

# Functions
print_header() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                                                            ║${NC}"
    echo -e "${BLUE}║        AKA - Advanced Komprehensive Assurance              ║${NC}"
    echo -e "${BLUE}║        HFT System Testing Suite                            ║${NC}"
    echo -e "${BLUE}║                                                            ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
    echo ""
}

print_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}  $1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

test_result() {
    local test_name=$1
    local result=$2
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if [ "$result" -eq 0 ]; then
        echo -e "  ${GREEN}✓${NC} ${test_name}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "  ${RED}✗${NC} ${test_name}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

skip_test() {
    local test_name=$1
    local reason=$2
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    echo -e "  ${YELLOW}⊘${NC} ${test_name} (${reason})"
}

run_smoke_tests() {
    print_section "Running Smoke Tests"
    
    # Test 1: Repository structure validation
    echo "Testing repository structure..."
    if [ -d "${PROJECT_ROOT}/ada" ] && [ -d "${PROJECT_ROOT}/java" ] && \
       [ -d "${PROJECT_ROOT}/erlang" ] && [ -d "${PROJECT_ROOT}/akka" ]; then
        test_result "Repository structure validation" 0
    else
        test_result "Repository structure validation" 1
    fi
    
    # Test 2: Required files exist
    echo "Testing required files..."
    if [ -f "${PROJECT_ROOT}/Makefile" ] && [ -f "${PROJECT_ROOT}/README.md" ]; then
        test_result "Required files present" 0
    else
        test_result "Required files present" 1
    fi
    
    # Test 3: Ada test file exists
    echo "Testing Ada test infrastructure..."
    if [ -f "${PROJECT_ROOT}/ada/hft_test.adb" ]; then
        test_result "Ada test infrastructure" 0
    else
        test_result "Ada test infrastructure" 1
    fi
    
    # Test 4: Java test file exists
    echo "Testing Java test infrastructure..."
    if [ -f "${PROJECT_ROOT}/java/src/test/java/com/hft/java/OrderTest.java" ]; then
        test_result "Java test infrastructure" 0
    else
        test_result "Java test infrastructure" 1
    fi
    
    # Test 5: Erlang test files exist
    echo "Testing Erlang test infrastructure..."
    if [ -f "${PROJECT_ROOT}/erlang/test/hft_order_processor_tests.erl" ]; then
        test_result "Erlang test infrastructure" 0
    else
        test_result "Erlang test infrastructure" 1
    fi
    
    # Test 6: AKA configuration exists
    echo "Testing AKA configuration..."
    if [ -f "${SCRIPT_DIR}/aka_config.yaml" ]; then
        test_result "AKA configuration present" 0
    else
        test_result "AKA configuration present" 1
    fi
    
    # Run comprehensive test suite
    echo ""
    echo "Running comprehensive test suite..."
    if [ -f "${SCRIPT_DIR}/tests/smoke/test_comprehensive_suite.sh" ]; then
        # Capture output and count tests
        COMP_OUTPUT=$(bash "${SCRIPT_DIR}/tests/smoke/test_comprehensive_suite.sh" 2>&1)
        # Use sed for portability instead of grep -P
        COMP_COUNT=$(echo "$COMP_OUTPUT" | sed -n 's/Test count: \([0-9]*\)/\1/p' | head -1)
        COMP_PASS=$(echo "$COMP_OUTPUT" | sed -n 's/Pass count: \([0-9]*\)/\1/p' | head -1)
        
        # Default to 0 if empty
        COMP_COUNT=${COMP_COUNT:-0}
        COMP_PASS=${COMP_PASS:-0}
        
        # Add to total
        TOTAL_TESTS=$((TOTAL_TESTS + COMP_COUNT))
        PASSED_TESTS=$((PASSED_TESTS + COMP_PASS))
        
        echo "$COMP_OUTPUT" | grep "Category"
        echo "  ✓ Comprehensive suite: ${COMP_COUNT} tests executed"
    fi
}

run_integration_tests() {
    print_section "Running Integration Tests"
    
    # Integration test 1: Check if test directories exist
    echo "Testing integration test structure..."
    if [ -d "${SCRIPT_DIR}/tests/integration" ]; then
        test_result "Integration test directory structure" 0
        
        # Run integration test scripts if they exist
        if [ -f "${SCRIPT_DIR}/tests/integration/test_order_flow.sh" ]; then
            echo "Executing order flow integration test..."
            if bash "${SCRIPT_DIR}/tests/integration/test_order_flow.sh" > /dev/null 2>&1; then
                test_result "Order flow integration test" 0
            else
                test_result "Order flow integration test" 1
            fi
        else
            skip_test "Order flow integration test" "script not found"
        fi
    else
        skip_test "Integration tests" "directory not found"
    fi
}

run_performance_tests() {
    print_section "Running Performance Tests"
    
    # Performance test placeholder
    echo "Checking performance test infrastructure..."
    if [ -d "${SCRIPT_DIR}/tests/performance" ]; then
        test_result "Performance test directory structure" 0
    else
        skip_test "Performance tests" "directory not found"
    fi
}

run_all_tests() {
    run_smoke_tests
    run_integration_tests
    run_performance_tests
}

generate_report() {
    mkdir -p "${REPORT_DIR}"
    
    {
        echo "AKA Test Report"
        echo "==============="
        echo ""
        echo "Execution Time: $(date)"
        echo "Project Root: ${PROJECT_ROOT}"
        echo ""
        echo "Results:"
        echo "--------"
        echo "Total Tests:   ${TOTAL_TESTS}"
        echo "Passed:        ${PASSED_TESTS}"
        echo "Failed:        ${FAILED_TESTS}"
        echo "Skipped:       ${SKIPPED_TESTS}"
        echo ""
        
        if [ ${FAILED_TESTS} -eq 0 ]; then
            echo "Status: ✓ ALL TESTS PASSED"
        else
            echo "Status: ✗ SOME TESTS FAILED"
        fi
    } > "${REPORT_FILE}"
    
    echo ""
    echo -e "${BLUE}Report saved to: ${REPORT_FILE}${NC}"
}

print_summary() {
    echo ""
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                     Test Summary                           ║${NC}"
    echo -e "${BLUE}╠════════════════════════════════════════════════════════════╣${NC}"
    printf "${BLUE}║${NC}  Total Tests:   %-40s ${BLUE}║${NC}\n" "${TOTAL_TESTS}"
    printf "${BLUE}║${NC}  ${GREEN}Passed:${NC}        %-40s ${BLUE}║${NC}\n" "${PASSED_TESTS}"
    printf "${BLUE}║${NC}  ${RED}Failed:${NC}        %-40s ${BLUE}║${NC}\n" "${FAILED_TESTS}"
    printf "${BLUE}║${NC}  ${YELLOW}Skipped:${NC}       %-40s ${BLUE}║${NC}\n" "${SKIPPED_TESTS}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
    
    if [ ${FAILED_TESTS} -eq 0 ] && [ ${TOTAL_TESTS} -gt 0 ]; then
        echo ""
        echo -e "${GREEN}✓ ALL TESTS PASSED!${NC}"
        echo ""
    elif [ ${TOTAL_TESTS} -eq 0 ]; then
        echo ""
        echo -e "${YELLOW}⚠ NO TESTS EXECUTED${NC}"
        echo ""
    else
        echo ""
        echo -e "${RED}✗ SOME TESTS FAILED${NC}"
        echo ""
    fi
}

show_usage() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  --all           Run all tests (default)"
    echo "  --smoke         Run smoke tests only"
    echo "  --integration   Run integration tests only"
    echo "  --performance   Run performance tests only"
    echo "  --report        Generate detailed report"
    echo "  --help          Show this help message"
    echo ""
}

# Main execution
main() {
    local run_mode="all"
    local generate_report_flag=false
    
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --all)
                run_mode="all"
                shift
                ;;
            --smoke)
                run_mode="smoke"
                shift
                ;;
            --integration)
                run_mode="integration"
                shift
                ;;
            --performance)
                run_mode="performance"
                shift
                ;;
            --report)
                generate_report_flag=true
                shift
                ;;
            --help)
                show_usage
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    print_header
    
    # Run tests based on mode
    case ${run_mode} in
        all)
            run_all_tests
            ;;
        smoke)
            run_smoke_tests
            ;;
        integration)
            run_integration_tests
            ;;
        performance)
            run_performance_tests
            ;;
    esac
    
    # Generate report if requested
    if [ "${generate_report_flag}" = true ]; then
        generate_report
    fi
    
    print_summary
    
    # Exit with appropriate code
    if [ ${FAILED_TESTS} -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Run main function
main "$@"
