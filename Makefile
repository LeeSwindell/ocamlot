# OCamlot Infrastructure Makefile
# Provides convenient targets for managing infrastructure and development with Overmind

.PHONY: help infra-up infra-down infra-logs infra-clean overmind-start overmind-stop

# Default target
help:
	@echo "OCamlot Infrastructure Management"
	@echo "================================="
	@echo ""
	@echo "Infrastructure Commands:"
	@echo "  infra-up             - Start NATS and Redis infrastructure"
	@echo "  infra-down           - Stop infrastructure services"
	@echo "  infra-logs           - Show infrastructure service logs"
	@echo "  infra-clean          - Stop services and remove volumes"
	@echo "  infra-health         - Check infrastructure health"
	@echo "  infra-monitor        - Start with monitoring (Redis Commander, NATS Surveyor)"
	@echo "  infra-metrics        - Start with metrics (Grafana/Prometheus)"
	@echo ""
	@echo "Development Commands (Overmind):"
	@echo "  overmind-start       - Start all OCaml services with Overmind"
	@echo "  overmind-stop        - Stop Overmind services"
	@echo "  overmind-connect     - Connect to running Overmind session"
	@echo ""
	@echo "OCaml Development:"
	@echo "  build                - Build all OCaml components"
	@echo "  test                 - Run all tests"
	@echo "  clean                - Clean build artifacts"
	@echo ""
	@echo "Service URLs:"
	@echo "  NATS Server:    nats://localhost:4222"
	@echo "  NATS Monitor:   http://localhost:8222"
	@echo "  Redis:          redis://localhost:6379"
	@echo "  Redis UI:       http://localhost:8081 (with monitoring profile)"
	@echo "  NATS UI:        http://localhost:7777 (with monitoring profile)"
	@echo "  Grafana:        http://localhost:3000 (with metrics profile)"
	@echo "  Web Dashboard:  http://localhost:8080 (when services running)"

# OCaml build commands
build:
	@echo "Building OCamlot components..."
	dune build

test: build
	@echo "Running all tests..."
	dune runtest

clean:
	@echo "Cleaning build artifacts..."
	dune clean

# Infrastructure commands
infra-up:
	@echo "Starting NATS and Redis infrastructure..."
	docker-compose up -d nats redis
	@echo "Waiting for services to be healthy..."
	@sleep 5
	@echo "Infrastructure ready:"
	@echo "  NATS Server:   nats://localhost:4222"
	@echo "  NATS Monitor:  http://localhost:8222"
	@echo "  Redis:         redis://localhost:6379"

infra-down:
	@echo "Stopping infrastructure services..."
	docker-compose down

infra-logs:
	@echo "Showing infrastructure logs..."
	docker-compose logs -f nats redis

infra-clean:
	@echo "Stopping services and removing volumes..."
	docker-compose down -v
	docker-compose rm -f

infra-health:
	@echo "Checking infrastructure health..."
	@echo "NATS Health:"
	@curl -s http://localhost:8222/healthz && echo " ✓ NATS OK" || echo " ✗ NATS Failed"
	@echo "Redis Health:"
	@docker-compose exec redis redis-cli ping && echo " ✓ Redis OK" || echo " ✗ Redis Failed"

infra-monitor:
	@echo "Starting infrastructure with monitoring..."
	docker-compose --profile monitoring up -d
	@echo "Services with monitoring started:"
	@echo "  NATS Server:    nats://localhost:4222"
	@echo "  NATS Monitor:   http://localhost:8222"
	@echo "  NATS UI:        http://localhost:7777"
	@echo "  Redis:          redis://localhost:6379"
	@echo "  Redis UI:       http://localhost:8081 (admin/admin)"

infra-metrics:
	@echo "Starting infrastructure with metrics..."
	docker-compose --profile metrics up -d
	@echo "Services with metrics started:"
	@echo "  NATS Server:    nats://localhost:4222"
	@echo "  NATS Monitor:   http://localhost:8222"
	@echo "  Redis:          redis://localhost:6379"
	@echo "  Grafana:        http://localhost:3000 (admin/admin)"
	@echo "  Prometheus:     http://localhost:9090"

# Overmind development commands
overmind-start: infra-up build
	@echo "Starting OCamlot services with Overmind..."
	@echo "Infrastructure running, starting application services..."
	@echo ""
	@echo "Overmind will run:"
	@echo "  - watcher: dune build --watch (rebuilds on file changes)"
	@echo "  - web: Web server at http://localhost:8080"
	@echo "  - market-data: Market data publisher"
	@echo ""
	@echo "After file changes, restart services with: overmind restart <service>"
	@sleep 2
	overmind start

overmind-stop:
	@echo "Stopping Overmind services..."
	overmind stop || true

overmind-connect:
	@echo "Connecting to Overmind session..."
	overmind connect

overmind-restart:
	@echo "Restarting specific Overmind service..."
	@echo "Usage: overmind restart <service_name>"
	@echo "Available services: watcher, web, market-data"
	@echo ""
	@echo "Example workflow after code changes:"
	@echo "  1. watcher rebuilds automatically"
	@echo "  2. overmind restart web (to reload web server)"
	@echo "  3. overmind restart market-data (to reload publisher)"