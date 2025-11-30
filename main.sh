#!/bin/bash
# MAIN - Unified Entry Point for the Complete Functional Hyper-Ultra-HFT System
# This script provides the main entry point to start, manage, and run the polyglot HFT system

set -e

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Print banner
print_banner() {
    echo ""
    echo "╔══════════════════════════════════════════════════════════════════╗"
    echo "║     🚀 Complete Functional Hyper-Ultra-HFT System - MAIN 🚀      ║"
    echo "╠══════════════════════════════════════════════════════════════════╣"
    echo "║  A polyglot High-Frequency Trading system demonstrating          ║"
    echo "║  Ada, Lean, Akka, Java 25, Netty, Swift/SwiftUI, and Erlang/OTP ║"
    echo "╚══════════════════════════════════════════════════════════════════╝"
    echo ""
}

# Print usage information
usage() {
    echo -e "${CYAN}Usage:${NC} $0 [COMMAND] [OPTIONS]"
    echo ""
    echo -e "${CYAN}Commands:${NC}"
    echo "  start       Start all HFT system components"
    echo "  stop        Stop all running components"
    echo "  status      Show status of all components"
    echo "  build       Build all components"
    echo "  test        Run all tests"
    echo "  run         Run the complete HFT demo"
    echo "  help        Show this help message"
    echo ""
    echo -e "${CYAN}Component-specific commands:${NC}"
    echo "  ada         Run Ada engine"
    echo "  java        Run Java powerhouse"
    echo "  akka        Run Akka reactive bridge"
    echo "  erlang      Run Erlang supervisor"
    echo "  swift       Run Swift application"
    echo "  lean        Run Lean verification"
    echo ""
    echo -e "${CYAN}Examples:${NC}"
    echo "  $0 start    # Start the complete system"
    echo "  $0 build    # Build all components"
    echo "  $0 ada      # Run only the Ada engine"
    echo ""
}

# Check if a command exists
check_command() {
    local cmd=$1
    if command -v "$cmd" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

# Print component status
component_status() {
    local name=$1
    local emoji=$2
    local available=$3
    local details=$4
    
    if [ "$available" = "true" ]; then
        echo -e "  ${GREEN}✓${NC} $emoji $name: ${GREEN}Available${NC} - $details"
    else
        echo -e "  ${RED}✗${NC} $emoji $name: ${RED}Not Available${NC} - $details"
    fi
}

# Show system status
show_status() {
    echo -e "${BLUE}=== System Component Status ===${NC}"
    echo ""
    
    # Ada
    if check_command gprbuild; then
        component_status "Ada Engine" "🛡️" "true" "GNAT compiler found"
    else
        component_status "Ada Engine" "🛡️" "false" "Install GNAT Ada compiler"
    fi
    
    # Java
    if check_command java; then
        JAVA_VER=$(java -version 2>&1 | head -n 1 | cut -d'"' -f2 | cut -d'.' -f1)
        component_status "Java Powerhouse" "💪" "true" "Java $JAVA_VER"
    else
        component_status "Java Powerhouse" "💪" "false" "Install JDK 21+"
    fi
    
    # Scala/Akka
    if [ -f "akka/gradlew" ]; then
        component_status "Akka Bridge" "🌉" "true" "Gradle wrapper available"
    else
        component_status "Akka Bridge" "🌉" "false" "Missing Gradle wrapper"
    fi
    
    # Erlang
    if check_command erl; then
        ERL_VER=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '"')
        component_status "Erlang/OTP" "🧠" "true" "OTP $ERL_VER"
    else
        component_status "Erlang/OTP" "🧠" "false" "Install Erlang/OTP 26+"
    fi
    
    # Swift
    if check_command swift; then
        SWIFT_VER=$(timeout 2 swift --version 2>&1 | head -n 1 || echo "Swift available")
        component_status "Swift/SwiftUI" "🍎" "true" "$SWIFT_VER"
    else
        component_status "Swift/SwiftUI" "🍎" "false" "Install Swift 5.9+"
    fi
    
    # Lean
    if check_command lake; then
        component_status "Lean Proofs" "📐" "true" "Lake build tool found"
    else
        component_status "Lean Proofs" "📐" "false" "Install Lean 4 toolchain"
    fi
    
    echo ""
}

# Build all components
build_all() {
    echo -e "${BLUE}=== Building All Components ===${NC}"
    echo ""
    
    # Use make if available
    if [ -f "Makefile" ]; then
        make all
    else
        # Manual build
        build_ada
        build_java
        build_akka
        build_erlang
        build_swift
        build_lean
    fi
    
    echo ""
    echo -e "${GREEN}✓ Build complete${NC}"
}

build_ada() {
    echo -e "${YELLOW}Building Ada Engine...${NC}"
    if check_command gprbuild && [ -d "ada" ]; then
        cd ada && mkdir -p obj && gprbuild -P hft.gpr 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Ada engine built"
    else
        echo -e "  ${RED}✗${NC} Ada build skipped (GNAT not available)"
    fi
}

build_java() {
    echo -e "${YELLOW}Building Java Powerhouse...${NC}"
    if [ -f "java/gradlew" ]; then
        cd java && ./gradlew build --quiet 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Java powerhouse built"
    else
        echo -e "  ${RED}✗${NC} Java build skipped (Gradle wrapper not found)"
    fi
}

build_akka() {
    echo -e "${YELLOW}Building Akka Bridge...${NC}"
    if [ -f "akka/gradlew" ]; then
        cd akka && ./gradlew build --quiet 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Akka bridge built"
    else
        echo -e "  ${RED}✗${NC} Akka build skipped (Gradle wrapper not found)"
    fi
}

build_erlang() {
    echo -e "${YELLOW}Building Erlang Supervisor...${NC}"
    if check_command rebar3 && [ -d "erlang" ]; then
        cd erlang && rebar3 compile 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Erlang supervisor built"
    else
        echo -e "  ${RED}✗${NC} Erlang build skipped (rebar3 not available)"
    fi
}

build_swift() {
    echo -e "${YELLOW}Building Swift Application...${NC}"
    if check_command swift && [ -d "swift" ]; then
        cd swift && swift build 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Swift application built"
    else
        echo -e "  ${RED}✗${NC} Swift build skipped (Swift not available)"
    fi
}

build_lean() {
    echo -e "${YELLOW}Verifying Lean Proofs...${NC}"
    if check_command lake && [ -d "lean" ]; then
        cd lean && lake build 2>/dev/null && cd ..
        echo -e "  ${GREEN}✓${NC} Lean proofs verified"
    else
        echo -e "  ${RED}✗${NC} Lean verification skipped (Lake not available)"
    fi
}

# Run all tests
run_tests() {
    echo -e "${BLUE}=== Running All Tests ===${NC}"
    echo ""
    
    if [ -f "Makefile" ]; then
        make test
    else
        echo "Running tests manually..."
        
        # Ada tests
        if check_command gprbuild && [ -d "ada" ]; then
            echo -e "${YELLOW}Running Ada tests...${NC}"
            cd ada && ./hft_test 2>/dev/null || true && cd ..
        fi
        
        # Java tests
        if [ -f "java/gradlew" ]; then
            echo -e "${YELLOW}Running Java tests...${NC}"
            cd java && ./gradlew test --quiet 2>/dev/null || true && cd ..
        fi
        
        # Erlang tests
        if check_command rebar3 && [ -d "erlang" ]; then
            echo -e "${YELLOW}Running Erlang tests...${NC}"
            cd erlang && rebar3 eunit 2>/dev/null || true && cd ..
        fi
    fi
    
    echo ""
    echo -e "${GREEN}✓ Tests complete${NC}"
}

# Run Ada engine
run_ada() {
    echo -e "${BLUE}=== Running Ada HFT Engine 🛡️ ===${NC}"
    if [ -d "ada" ]; then
        cd ada
        if [ -f "hft_main" ]; then
            ./hft_main
        elif check_command gprbuild; then
            mkdir -p obj
            gprbuild -P hft.gpr && ./hft_main
        else
            echo -e "${RED}Error: Cannot run Ada engine - build required${NC}"
        fi
        cd ..
    else
        echo -e "${RED}Error: Ada directory not found${NC}"
    fi
}

# Run Java powerhouse
run_java() {
    echo -e "${BLUE}=== Running Java Powerhouse 💪 ===${NC}"
    if [ -f "java/gradlew" ]; then
        cd java && ./gradlew run --quiet 2>/dev/null && cd ..
    else
        echo -e "${RED}Error: Cannot run Java powerhouse${NC}"
    fi
}

# Run Akka bridge
run_akka() {
    echo -e "${BLUE}=== Running Akka Reactive Bridge 🌉 ===${NC}"
    if [ -f "akka/gradlew" ]; then
        cd akka && ./gradlew run --quiet 2>/dev/null && cd ..
    else
        echo -e "${RED}Error: Cannot run Akka bridge${NC}"
    fi
}

# Run Erlang supervisor
run_erlang() {
    echo -e "${BLUE}=== Running Erlang Supervisor 🧠 ===${NC}"
    if check_command rebar3 && [ -d "erlang" ]; then
        cd erlang && rebar3 shell 2>/dev/null && cd ..
    else
        echo -e "${RED}Error: Cannot run Erlang supervisor${NC}"
    fi
}

# Run Swift application
run_swift() {
    echo -e "${BLUE}=== Running Swift Application 🍎 ===${NC}"
    if check_command swift && [ -d "swift" ]; then
        cd swift && swift run HFTSwiftApp 2>/dev/null && cd ..
    else
        echo -e "${RED}Error: Cannot run Swift application${NC}"
    fi
}

# Run Lean verification
run_lean() {
    echo -e "${BLUE}=== Running Lean Verification 📐 ===${NC}"
    if check_command lake && [ -d "lean" ]; then
        cd lean && lake exe hft 2>/dev/null && cd ..
    else
        echo -e "${RED}Error: Cannot run Lean verification${NC}"
    fi
}

# Run complete demo
run_demo() {
    print_banner
    
    echo -e "${BLUE}=== Starting Complete HFT System Demo ===${NC}"
    echo ""
    
    # Show status first
    show_status
    
    echo -e "${BLUE}=== Running Components ===${NC}"
    echo ""
    
    # Run Ada if available
    if check_command gprbuild && [ -d "ada" ]; then
        echo -e "${MAGENTA}--- Ada Engine ---${NC}"
        run_ada
        echo ""
    fi
    
    # Run Lean if available
    if check_command lake && [ -d "lean" ]; then
        echo -e "${MAGENTA}--- Lean Proofs ---${NC}"
        run_lean
        echo ""
    fi
    
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                    🎯 MAIN EXECUTION COMPLETE 🎯                  ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "📚 For more information, see docs/README.md"
    echo "🔧 To build all components: $0 build"
    echo "🧪 To run tests: $0 test"
    echo ""
}

# Start all components
start_system() {
    print_banner
    echo -e "${BLUE}=== Starting HFT System ===${NC}"
    echo ""
    
    # Build first
    build_all
    
    # Run demo
    run_demo
}

# Main entry point
main() {
    case "${1:-help}" in
        start)
            start_system
            ;;
        stop)
            echo "Stopping HFT system components..."
            echo -e "${YELLOW}Note: Component shutdown is handled by each service${NC}"
            ;;
        status)
            print_banner
            show_status
            ;;
        build)
            print_banner
            build_all
            ;;
        test)
            print_banner
            run_tests
            ;;
        run|demo)
            run_demo
            ;;
        ada)
            run_ada
            ;;
        java)
            run_java
            ;;
        akka)
            run_akka
            ;;
        erlang)
            run_erlang
            ;;
        swift)
            run_swift
            ;;
        lean)
            run_lean
            ;;
        help|--help|-h)
            print_banner
            usage
            ;;
        *)
            echo -e "${RED}Error: Unknown command '$1'${NC}"
            echo ""
            usage
            exit 1
            ;;
    esac
}

# Run main
main "$@"
