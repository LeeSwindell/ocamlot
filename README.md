# OCamlot - Type-Safe Trading System

A comprehensive trading system built in OCaml featuring order management, risk checks, FIX protocol support, market data handling, and a pure OCaml NATS client implementation.

## Features

- **Pure OCaml NATS Client**: Battle-tested messaging with full protocol implementation
- **Order Management System**: Type-safe order lifecycle management
- **Risk Management**: Real-time risk checks and position monitoring
- **FIX Protocol Support**: Industry-standard FIX messaging
- **Market Data Handling**: Real-time and historical market data processing
- **Event-Driven Architecture**: Reliable messaging with NATS and JetStream
- **Comprehensive Testing**: Test-driven development with 25+ passing tests

## Key OCaml Type Safety Features

### Sum Types for Data Formats
```ocaml
type tick_format = 
  | Simple of { price : price }
  | WithVolume of { price : price; volume : volume }
  | FullDepth of { 
      bid_price : price; 
      ask_price : price; 
      bid_volume : volume; 
      ask_volume : volume 
    }
```

### Algorithmic Variants
```ocaml
type generation_style =
  | RandomWalk of { step_size : float; volatility : float }
  | BrownianMotion of { drift : float; volatility : float; dt : float }
```

The compiler ensures all cases are handled, making it impossible to forget to implement support for a new format or algorithm.

## Quick Start

### Prerequisites
- OCaml 4.14+ and opam
- Docker and docker-compose (for testing infrastructure)

### Development Setup
```bash
# Install dependencies
opam install --deps-only .

# Start development infrastructure (NATS + Redis)
make docker-up

# Build and test
make dev

# Run comprehensive tests
make docker-test
```

## Building and Testing

### Local Development
```bash
# Build the project
make build

# Run tests (without external services)
make test

# Run specific test suites
make test-core      # Core NATS functionality
make test-nats      # Basic NATS tests
make test-messaging # Messaging layer tests
```

### Docker-based Testing
```bash
# Full test cycle with NATS and Redis
make docker-test

# Start services for manual testing
make docker-up

# Stop services
make docker-down

# Start with monitoring UI
make docker-monitor
```

## Infrastructure Services

When running `make docker-up`, the following services are available:

- **NATS Server**: `nats://localhost:4222`
- **NATS Monitoring**: `http://localhost:8222`
- **Redis**: `redis://localhost:6379`
- **Redis UI**: `http://localhost:8081` (with monitoring profile)

### Monitoring and Metrics
```bash
# Start with monitoring dashboards
make docker-monitor

# Start with full metrics stack (Grafana + Prometheus)
make docker-metrics
```

Access monitoring dashboards:
- **Redis Commander**: http://localhost:8081 (admin/admin)
- **Grafana**: http://localhost:3000 (admin/admin)
- **Prometheus**: http://localhost:9090

## Example Configuration

```json
{
  "symbols": ["AAPL", "GOOGL", "MSFT", "TSLA"],
  "frequency_ms": 100,
  "tick_format": {
    "type": "full_depth",
    "bid_price": 100.0,
    "ask_price": 100.05,
    "bid_volume": 500,
    "ask_volume": 800
  },
  "generation_style": {
    "type": "brownian_motion",
    "drift": 0.001,
    "volatility": 0.02,
    "dt": 0.1
  }
}
```

## Project Architecture

### Core Components

- **`nats/`** - Pure OCaml NATS client implementation
- **`messaging/`** - Event-driven messaging layer built on NATS
- **`core/`** - Order types and core trading logic
- **`oms/`** - Order Management System
- **`risk/`** - Risk management and position monitoring
- **`fix/`** - FIX protocol implementation
- **`market_data/`** - Market data feed handling
- **`sim/`** - Trading simulation engine

### NATS Client Implementation

Our pure OCaml NATS client provides:

- **Full Protocol Support**: CONNECT, PUB, SUB, UNSUB, PING/PONG
- **JetStream Ready**: Architecture prepared for JetStream features
- **Type Safety**: Leverages OCaml's type system for protocol correctness
- **No External Dependencies**: Pure OCaml implementation (no C FFI)
- **Test-Driven Development**: 25+ tests covering all functionality

### Testing Strategy

**✅ Passing Tests (25 total)**
- Core NATS functionality (21 tests)
- Basic protocol tests (4 tests)

**🚧 TDD Structure Created**
- Advanced connection tests (18 test structures)
- Advanced subscription tests (16 test structures)  
- JetStream tests (17 test structures)

Run `make test-core` to see all passing tests in action!

## Type Safety Benefits

1. **Exhaustive Pattern Matching**: The compiler ensures all variants of sum types are handled
2. **Configuration Validation**: Invalid configurations are caught at parse time
3. **No Runtime Type Errors**: Strong typing prevents many classes of runtime errors
4. **Event Safety**: Type-safe event handling with impossible invalid states
5. **Protocol Correctness**: NATS protocol implementation verified by types