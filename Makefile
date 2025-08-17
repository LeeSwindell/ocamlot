# OCamlot Trading System Makefile
# Provides convenient targets for building, testing, and running the trading system

.PHONY: help build test clean docker-up docker-down docker-logs docker-test docker-clean

# Default target
help:
	@echo "OCamlot Trading System"
	@echo "====================="
	@echo ""
	@echo "Development Commands:"
	@echo "  build                - Build all OCaml components"
	@echo "  build-web            - Build web server and client"
	@echo "  test                 - Run all tests"
	@echo "  test-core            - Run core NATS functionality tests"
	@echo "  test-nats            - Run basic NATS tests"
	@echo "  clean                - Clean build artifacts"
	@echo ""
	@echo "Local Component Management:"
	@echo "  serve                - Start web server (foreground)"
	@echo "  serve-bg             - Start web server (background)"
	@echo "  market-data-publisher - Start market data publisher (foreground)"
	@echo "  market-data-publisher-bg - Start market data publisher (background)"
	@echo "  start-all-bg         - Start complete system (infrastructure + services)"
	@echo "  stop-all             - Stop all components"
	@echo "  status               - Show status of all components"
	@echo "  logs                 - Watch logs from all components"
	@echo ""
	@echo "Infrastructure Commands:"
	@echo "  docker-up-vendor     - Start NATS and Redis infrastructure only"
	@echo "  docker-up-market-data - Start infrastructure + market data publisher"
	@echo "  docker-up-full       - Start complete system"
	@echo "  docker-down          - Stop infrastructure services"
	@echo "  docker-logs          - Show infrastructure service logs"
	@echo "  docker-clean         - Stop services and remove volumes"
	@echo "  docker-health        - Check infrastructure health"
	@echo "  docker-monitor       - Start with monitoring (Grafana/Prometheus)"
	@echo ""
	@echo "Docker Build Commands:"
	@echo "  docker-build-base    - Build OCamlot base image with dependencies"
	@echo "  docker-build-market-data - Build market data publisher image"
	@echo "  docker-build         - Build all Docker images"
	@echo "  docker-rebuild-market-data - Quick rebuild of market data publisher"
	@echo ""
	@echo "Quick Start Workflows:"
	@echo "  start-all-bg         - Complete development setup (recommended)"
	@echo "  dev                  - Legacy: infrastructure + manual component start"
	@echo "  dev-hybrid           - Legacy: hybrid Docker + local development"
	@echo ""
	@echo "Service URLs (when docker-up is running):"
	@echo "  NATS Server:    nats://localhost:4222"
	@echo "  NATS Monitor:   http://localhost:8222"
	@echo "  Redis:          redis://localhost:6379"
	@echo "  Redis UI:       http://localhost:8081 (with --profile monitoring)"
	@echo "  Grafana:        http://localhost:3000 (with --profile metrics)"
	@echo ""
	@echo "Web Interface:"
	@echo "  Dashboard:      http://localhost:8080 (when serve is running)"

# Build commands
build:
	@echo "Building OCamlot trading system..."
	dune build

build-web: build
	@echo "Building web components..."
	dune build web/

serve: build-web
	@echo "Starting OCamlot web server on http://localhost:8080"
	@echo "Press Ctrl+C to stop the server"
	dune exec ocamlot-web-server

test: build
	@echo "Running all tests..."
	dune runtest

test-core: build
	@echo "Running core NATS functionality tests..."
	dune exec nats/test/core/test_core_functionality.exe

test-nats: build
	@echo "Running basic NATS tests..."
	dune exec nats/test/test_nats.exe

test-messaging: build
	@echo "Running messaging tests..."
	dune exec messaging/test/test_messaging.exe

clean:
	@echo "Cleaning build artifacts..."
	dune clean

# Docker commands
docker-up-vendor:
	@echo "Starting NATS and Redis services..."
	docker-compose up -d nats redis
	@echo "Waiting for services to be healthy..."
	@sleep 5
	@echo "Services started. NATS: localhost:4222, Redis: localhost:6379"
	@echo "NATS Monitor: http://localhost:8222"

docker-up-market-data: docker-up-vendor
	@echo "Starting OCamlot market data publisher..."
	docker-compose up -d market-data-publisher
	@echo "Waiting for market data publisher to be healthy..."
	@sleep 5
	@echo "Market data publisher started:"
	@echo "  NATS Server:        nats://localhost:4222"
	@echo "  NATS Monitor:       http://localhost:8222"
	@echo "  Redis:              redis://localhost:6379" 
	@echo "  Market Data:        Publishing to NATS"

docker-up-full:
	@echo "Starting complete OCamlot system (NATS, Redis, Market Data Publisher)..."
	docker-compose up -d
	@echo "Waiting for services to be healthy..."
	@sleep 10
	@echo "Complete system started:"
	@echo "  NATS Server:        nats://localhost:4222"
	@echo "  NATS Monitor:       http://localhost:8222"
	@echo "  Redis:              redis://localhost:6379" 
	@echo "  Market Data:        Publishing to NATS"

# Docker build commands - Optimized for fast builds
docker-build-base:
	@echo "Building OCamlot base image with dependencies..."
	docker-compose --profile build build ocamlot-base
	@echo "Base image build complete!"

docker-build-market-data:
	@echo "Building OCamlot market data publisher..."
	docker-compose build market-data-publisher
	@echo "Market data publisher build complete!"

docker-build: docker-build-base docker-build-market-data
	@echo "All Docker images built successfully!"

# Quick rebuild for development (only market data service)
docker-rebuild-market-data:
	@echo "Quick rebuild of market data publisher..."
	docker-compose build --no-cache market-data-publisher
	@echo "Rebuild complete!"

# docker-build-dev:
# 	@echo "Building OCamlot development environment..."
# 	docker-compose --profile build build ocamlot-base
# 	docker-compose --profile dev build ocamlot-dev
# 	@echo "Development environment ready!"

# docker-rebuild: docker-clean
# 	@echo "Rebuilding OCamlot Docker images from scratch..."
# 	docker-compose --profile build build --no-cache ocamlot-base
# 	docker-compose build --no-cache market-data-publisher web-server

docker-down:
	@echo "Stopping all services..."
	docker-compose down

docker-logs:
	@echo "Showing service logs..."
	docker-compose logs -f

docker-logs-nats:
	@echo "Showing NATS logs..."
	docker-compose logs -f nats

docker-logs-redis:
	@echo "Showing Redis logs..."
	docker-compose logs -f redis

docker-test: docker-up
	@echo "Running tests against docker services..."
	@sleep 10  # Wait for services to be fully ready
	$(MAKE) test-core
	$(MAKE) test-nats
	@echo "Integration tests (when implemented):"
	# $(MAKE) test-messaging
	@echo "Docker test suite completed"

docker-clean:
	@echo "Stopping services and removing volumes..."
	docker-compose down -v
	docker-compose rm -f

docker-reset: docker-clean docker-up
	@echo "Reset complete - fresh services are now running"

# Monitoring profiles
docker-monitor:
	@echo "Starting services with monitoring..."
	docker-compose --profile monitoring up -d
	@echo "Services with monitoring started:"
	@echo "  NATS Monitor:   http://localhost:8222"
	@echo "  Redis UI:       http://localhost:8081 (admin/admin)"

docker-metrics:
	@echo "Starting services with metrics..."
	docker-compose --profile metrics up -d
	@echo "Services with metrics started:"
	@echo "  Grafana:        http://localhost:3000 (admin/admin)"
	@echo "  Prometheus:     http://localhost:9090"

# Health checks
docker-health:
	@echo "Checking service health..."
	@echo "NATS Health:"
	@curl -s http://localhost:8222/healthz && echo " ✓ NATS OK" || echo " ✗ NATS Failed"
	@echo "Redis Health:"
	@docker-compose exec redis redis-cli ping && echo " ✓ Redis OK" || echo " ✗ Redis Failed"

# Development workflow - Local development with Docker infrastructure
dev: docker-up build test-core
	@echo "Development environment ready!"
	@echo "NATS: localhost:4222, Redis: localhost:6379"
	@echo "Use 'make serve' to start the web server locally"
	@echo "Use 'make market-data-publisher' to start market data publishing"

# Full system workflow - Everything in Docker
# dev-full: docker-build docker-up-full
# 	@echo "Complete dockerized development environment ready!"
# 	@echo "All services running in containers"

# Local Components - Build and Run Separately

# Local market data publisher (foreground)
market-data-publisher: build
	@echo "Starting local market data publisher..."
	@echo "Publishing synthetic market data to NATS (Ctrl+C to stop)"
	dune exec services/market_data_publisher/bin/main.exe

# Local market data publisher (background)
market-data-publisher-bg: build
	@echo "Starting market data publisher in background..."
	@mkdir -p logs
	@exec dune exec ocamlot.market_data > logs/market-data-publisher.log 2>&1 &
	@echo "Market data publisher started in background (process: market-data-exe-01)"
	@echo "Logs: tail -f logs/market-data-publisher.log"

# Local web server (background)
serve-bg: build-web
	@echo "Starting web server in background..."
	@mkdir -p logs
	@exec dune exec ocamlot-web-server > logs/web-server.log 2>&1 &
	@echo "Web server started in background (process: web-server-exe-01)"
	@echo "Web interface: http://localhost:8080"
	@echo "Logs: tail -f logs/web-server.log"

# Start all local components in background
start-all: docker-up build
	@echo ""
	@exec bash scripts/start-all.local.sh
	@sleep 2
	@echo ""
	@echo "=== OCamlot Local Development Environment Started ==="
	@echo "Infrastructure:"
	@echo "  NATS Server:        nats://localhost:4222"
	@echo "  NATS Monitor:       http://localhost:8222"
	@echo "  Redis:              redis://localhost:6379"
	@echo ""
	@echo "OCamlot Services:"
	@echo "  Market Data:        Publishing to NATS (process: market-data-exe-01)"
	@echo "  Web Dashboard:      http://localhost:8080 (process: web-server-exe-01)"
	@echo ""
	@echo "Logs:"
	@echo "  Market Data:        tail -f logs/market-data-publisher.log"
	@echo "  Web Server:         tail -f logs/web-server.log"
	@echo ""
	@echo "To stop: make stop-all"

# Stop all local components
stop-all:
	@echo "Stopping all OCamlot components..."
	@pkill -f "market-data-exe" 2>/dev/null && echo "Market data publisher stopped" || echo "Market data publisher not running"
	@pkill -f "web-server-exe" 2>/dev/null && echo "Web server stopped" || echo "Web server not running"
	@make docker-down 2>/dev/null || echo "Docker services not running"
	@echo "All components stopped"

# Show status of all components
status:
	@echo "=== OCamlot Component Status ==="
	@echo ""
	@echo "Docker Infrastructure:"
	@docker-compose ps 2>/dev/null || echo "  Docker services not running"
	@echo ""
	@echo "Local OCamlot Processes:"
	@if pgrep -f "market-data-exe" >/dev/null 2>&1; then \
		echo "  ✓ Market Data Publisher (process: market-data-exe-01)"; \
	else \
		echo "  ✗ Market Data Publisher not running"; \
	fi
	@if pgrep -f "web-server-exe" >/dev/null 2>&1; then \
		echo "  ✓ Web Server (process: web-server-exe-01)"; \
	else \
		echo "  ✗ Web Server not running"; \
	fi
	@echo ""
	@echo "Service Health:"
	@curl -s http://localhost:8080 >/dev/null && echo "  ✓ Web Dashboard: http://localhost:8080" || echo "  ✗ Web Dashboard not accessible"
	@curl -s http://localhost:8222 >/dev/null && echo "  ✓ NATS Monitor: http://localhost:8222" || echo "  ✗ NATS Monitor not accessible"

# Watch logs from all components
logs:
	@echo "Watching logs from all components (Ctrl+C to stop)..."
	@echo "Press Ctrl+C to stop watching logs"
	@mkdir -p logs
	@touch logs/market-data-publisher.log logs/web-server.log
	@tail -f logs/market-data-publisher.log logs/web-server.log

# Hybrid development - Docker infrastructure + local services
dev-hybrid: docker-up build
	@echo "Starting hybrid development environment..."
	@echo "Infrastructure (NATS/Redis) in Docker, services running locally"
	@echo ""
	@echo "To start services:"
	@echo "  Terminal 1: make market-data-publisher"
	@echo "  Terminal 2: make serve"
	@echo ""
	@echo "Then visit: http://localhost:8080"

# Fast development in container with volume mounts
dev-container: docker-up docker-build-dev
	@echo "Starting containerized development environment..."
	@echo "Use: docker-compose --profile dev run --rm ocamlot-dev"
	@echo ""
	@echo "Inside container:"
	@echo "  dune build                    # Fast builds"
	@echo "  dune exec services/market_data_publisher/bin/main.exe"
	@echo "  dune exec ocamlot-web-server"

# Quick rebuild for development (only changed services)
dev-rebuild:
	@echo "Quick rebuild of changed OCamlot services..."
	docker-compose build market-data-publisher web-server
	@echo "Rebuild complete!"

# Quick test cycle
quick-test: build test-core test-nats
	@echo "Quick test cycle completed"

# Full test cycle with docker
full-test: docker-test
	@echo "Full test cycle with docker services completed"