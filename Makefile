# OCamlot Trading System Makefile
# Provides convenient targets for building, testing, and running the trading system

.PHONY: help build test clean docker-up docker-down docker-logs docker-test docker-clean

# Default target
help:
	@echo "OCamlot Trading System"
	@echo "====================="
	@echo ""
	@echo "Development Commands:"
	@echo "  build          - Build all OCaml components"
	@echo "  build-web      - Build web server and client"
	@echo "  serve          - Start web server (localhost:8080)"
	@echo "  test           - Run all tests"
	@echo "  test-core      - Run core NATS functionality tests"
	@echo "  test-nats      - Run basic NATS tests"
	@echo "  clean          - Clean build artifacts"
	@echo ""
	@echo "Docker Commands:"
	@echo "  docker-up      - Start NATS and Redis services"
	@echo "  docker-down    - Stop all services"
	@echo "  docker-logs    - Show service logs"
	@echo "  docker-test    - Run tests against docker services"
	@echo "  docker-clean   - Stop services and remove volumes"
	@echo "  docker-monitor - Start with monitoring (Grafana/Prometheus)"
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
	@echo "Copying JavaScript to static directory..."
	cp _build/default/web/client/main.js web/static/

serve: build-web
	@echo "Starting OCamlot web server on http://localhost:8080"
	@echo "Press Ctrl+C to stop the server"
	dune exec web/server/main.exe

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
docker-up:
	@echo "Starting NATS and Redis services..."
	docker-compose up -d nats redis
	@echo "Waiting for services to be healthy..."
	@sleep 5
	@echo "Services started. NATS: localhost:4222, Redis: localhost:6379"
	@echo "NATS Monitor: http://localhost:8222"

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

# Development workflow
dev: docker-up build test-core
	@echo "Development environment ready!"
	@echo "NATS: localhost:4222, Redis: localhost:6379"

# Quick test cycle
quick-test: build test-core test-nats
	@echo "Quick test cycle completed"

# Full test cycle with docker
full-test: docker-test
	@echo "Full test cycle with docker services completed"