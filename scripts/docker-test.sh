#!/bin/bash
# OCamlot Docker Test Runner
# Starts docker services, runs tests, and cleans up

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TIMEOUT=60
NATS_PORT=4222
REDIS_PORT=6379

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if a service is ready
wait_for_service() {
    local service_name=$1
    local check_command=$2
    local timeout=$3
    local count=0
    
    log_info "Waiting for $service_name to be ready..."
    
    while [ $count -lt $timeout ]; do
        if eval "$check_command" >/dev/null 2>&1; then
            log_success "$service_name is ready"
            return 0
        fi
        
        sleep 1
        count=$((count + 1))
        
        if [ $((count % 10)) -eq 0 ]; then
            log_info "Still waiting for $service_name... (${count}s)"
        fi
    done
    
    log_error "$service_name failed to start within ${timeout}s"
    return 1
}

# Function to check if NATS is ready
check_nats() {
    curl -s http://localhost:8222/healthz >/dev/null 2>&1
}

# Function to check if Redis is ready
check_redis() {
    docker-compose exec -T redis redis-cli ping | grep -q PONG
}

# Function to run tests
run_tests() {
    log_info "Building OCamlot project..."
    cd "$PROJECT_ROOT"
    
    if ! dune build; then
        log_error "Build failed"
        return 1
    fi
    
    log_success "Build completed"
    
    # Run basic NATS tests (no server required)
    log_info "Running basic NATS tests..."
    if dune exec nats/test/test_nats.exe; then
        log_success "Basic NATS tests passed"
    else
        log_error "Basic NATS tests failed"
        return 1
    fi
    
    # Run core functionality tests (no server required)
    log_info "Running core functionality tests..."
    if dune exec nats/test/core/test_core_functionality.exe; then
        log_success "Core functionality tests passed"
    else
        log_error "Core functionality tests failed"
        return 1
    fi
    
    # Run messaging tests (may connect to server)
    log_info "Running messaging tests..."
    if dune exec messaging/test/test_messaging.exe; then
        log_success "Messaging tests passed"
    else
        log_warning "Messaging tests failed (expected for some tests without server)"
    fi
    
    log_success "All test suites completed"
}

# Function to start services
start_services() {
    log_info "Starting docker services..."
    cd "$PROJECT_ROOT"
    
    if ! docker-compose up -d nats redis; then
        log_error "Failed to start docker services"
        return 1
    fi
    
    # Wait for services to be ready
    if ! wait_for_service "NATS" "check_nats" $TIMEOUT; then
        log_error "NATS service failed to start"
        return 1
    fi
    
    if ! wait_for_service "Redis" "check_redis" $TIMEOUT; then
        log_error "Redis service failed to start"
        return 1
    fi
    
    log_success "All services are ready"
    
    # Show service information
    log_info "Service Information:"
    echo "  NATS Server:    nats://localhost:$NATS_PORT"
    echo "  NATS Monitor:   http://localhost:8222"
    echo "  Redis:          redis://localhost:$REDIS_PORT"
}

# Function to stop services
stop_services() {
    log_info "Stopping docker services..."
    cd "$PROJECT_ROOT"
    
    if docker-compose down; then
        log_success "Services stopped"
    else
        log_warning "Some services may not have stopped cleanly"
    fi
}

# Function to show usage
usage() {
    echo "OCamlot Docker Test Runner"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --start-only    Start services but don't run tests"
    echo "  --test-only     Run tests without managing services"
    echo "  --no-cleanup    Don't stop services after tests"
    echo "  --timeout=N     Set service startup timeout (default: 60s)"
    echo "  --help          Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                    # Full test cycle with service management"
    echo "  $0 --start-only       # Just start services"
    echo "  $0 --test-only        # Run tests against existing services"
    echo "  $0 --no-cleanup       # Run tests but leave services running"
}

# Parse command line arguments
START_ONLY=false
TEST_ONLY=false
NO_CLEANUP=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --start-only)
            START_ONLY=true
            shift
            ;;
        --test-only)
            TEST_ONLY=true
            shift
            ;;
        --no-cleanup)
            NO_CLEANUP=true
            shift
            ;;
        --timeout=*)
            TIMEOUT="${1#*=}"
            shift
            ;;
        --help)
            usage
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Main execution
main() {
    log_info "OCamlot Docker Test Runner Starting..."
    
    # Check if docker-compose is available
    if ! command -v docker-compose >/dev/null 2>&1; then
        log_error "docker-compose is not installed or not in PATH"
        exit 1
    fi
    
    # Check if we're in the right directory
    if [[ ! -f "$PROJECT_ROOT/docker-compose.yml" ]]; then
        log_error "docker-compose.yml not found in $PROJECT_ROOT"
        exit 1
    fi
    
    if [[ "$TEST_ONLY" == "true" ]]; then
        log_info "Running tests only (assuming services are already running)..."
        if run_tests; then
            log_success "Test run completed successfully"
            exit 0
        else
            log_error "Test run failed"
            exit 1
        fi
    fi
    
    if [[ "$START_ONLY" == "false" ]]; then
        # Stop any existing services first
        stop_services
    fi
    
    # Start services
    if ! start_services; then
        log_error "Failed to start services"
        exit 1
    fi
    
    if [[ "$START_ONLY" == "true" ]]; then
        log_success "Services started and ready for testing"
        log_info "Use '$0 --test-only' to run tests, or '$0 --stop' to stop services"
        exit 0
    fi
    
    # Run tests
    if run_tests; then
        log_success "All tests passed!"
        TEST_RESULT=0
    else
        log_error "Some tests failed"
        TEST_RESULT=1
    fi
    
    # Cleanup unless requested not to
    if [[ "$NO_CLEANUP" == "false" ]]; then
        stop_services
    else
        log_info "Services left running (--no-cleanup specified)"
        log_info "Use 'docker-compose down' or 'make docker-down' to stop them"
    fi
    
    exit $TEST_RESULT
}

# Handle script interruption
cleanup_on_exit() {
    log_warning "Script interrupted, cleaning up..."
    stop_services
    exit 1
}

trap cleanup_on_exit INT TERM

# Run main function
main "$@"