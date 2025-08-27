# OCamlot - Trading System in OCaml

A trading system written in OCaml with order management, risk checks, FIX protocol support, market data handling, and a Redis client.

## Features

- **Redis Client**: OCaml implementation of the RESP3 protocol
- **Order Management**: Handles order lifecycle and state transitions
- **Risk Management**: Checks positions and risk limits
- **FIX Protocol**: Supports FIX messaging for trading
- **Market Data**: Processes market data feeds
- **Caching**: Uses Redis for caching market data and order state
- **Testing**: Includes test suite for core functionality

## OCaml Type System Usage

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

The compiler checks that all cases are handled when pattern matching.

## Quick Start

### Prerequisites
- OCaml 4.14+ and opam
- Docker and docker-compose (for infrastructure)
- [Overmind](https://github.com/DarthSim/overmind) for process management

### Development Setup
```bash
# Install dependencies
opam install --deps-only .

# Start infrastructure (Redis + supporting services)
make infra-up

# Start all services with file watching
make overmind-start

# View service logs
overmind connect
```

## Building and Testing

### Local Development
```bash
# Build the project
make build

# Run tests
make test

# Development workflow
make overmind-start     # Start all services
overmind connect        # View logs
overmind stop          # Stop services

# Infrastructure
make infra-up          # Start Redis + supporting services
make infra-down        # Stop infrastructure
make infra-monitor     # Start with monitoring UI
make infra-metrics     # Start with Grafana/Prometheus
```

## Infrastructure Services

When running `make infra-up`, the following services are available:

- **Redis Server**: `redis://localhost:6379`
- **Message Broker**: `localhost:4222`
- **Monitoring Dashboard**: `http://localhost:8222`

### Monitoring and Metrics
```bash
# Start with monitoring dashboards
make infra-monitor

# Start with full metrics stack (Grafana + Prometheus)
make infra-metrics
```

Access monitoring dashboards:
- **Redis Commander**: http://localhost:8081 (admin/admin, with monitoring profile)
- **Grafana**: http://localhost:3000 (admin/admin, with metrics profile)
- **Prometheus**: http://localhost:9090 (with metrics profile)

## Development Workflow

### Process Management with Overmind

We use [Overmind](https://github.com/DarthSim/overmind) to manage development processes:

```bash
# Start everything
make overmind-start

# View logs in another terminal
overmind connect

# After editing code:
# 1. Files are automatically rebuilt by the watcher
# 2. Restart the service: overmind restart web
# 3. Check changes at http://localhost:8080

# Stop everything
make overmind-stop
```

Services managed by Overmind:
- **watcher**: Runs `dune build --watch` to rebuild on file changes
- **web**: Web server on port 8080
- **market-data**: Market data publisher

Note: We use a single `dune build --watch` process to avoid build lock issues. After changes rebuild, restart services manually with `overmind restart <service>`.

### Environment Configuration

Environment variables are in `.overmind.env`:
- `REDIS_HOST=localhost` - Redis server hostname
- `REDIS_PORT=6379` - Redis port
- `WEB_PORT=8080` - Web server port

Services connect to Docker containers via localhost ports.

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

- **`redis/`** - Redis client implementation
- **`messaging/`** - Message handling 
- **`core/`** - Order types and trading logic
- **`oms/`** - Order Management System
- **`risk/`** - Risk checks and position tracking
- **`fix/`** - FIX protocol support
- **`market_data/`** - Market data processing
- **`sim/`** - Trading simulator

## Redis Client

The Redis client is written in OCaml and implements the RESP3 protocol.

### Features
- **RESP3 Protocol**: Implements Redis Serialization Protocol version 3
- **Connection Management**: Handles connections and reconnection
- **Command Pipelining**: Can batch multiple commands together
- **Type Safety**: Uses OCaml's type system for command validation
- **No C Dependencies**: Written entirely in OCaml
- **Buffered Parser**: Uses buffering for parsing RESP3 responses

### Supported Commands
- **Connection**: PING, AUTH, SELECT, QUIT
- **String Operations**: GET, SET, MGET, MSET, INCR, DECR, APPEND
- **Key Management**: EXISTS, DEL, EXPIRE, TTL, KEYS, SCAN
- **List Operations**: LPUSH, RPUSH, LPOP, RPOP, LRANGE, LLEN
- **Hash Operations**: HSET, HGET, HMSET, HMGET, HGETALL, HDEL
- **Set Operations**: SADD, SREM, SMEMBERS, SISMEMBER, SCARD
- **Sorted Sets**: ZADD, ZREM, ZRANGE, ZRANK, ZSCORE
- **Pub/Sub**: PUBLISH, SUBSCRIBE, UNSUBSCRIBE, PSUBSCRIBE
- **Transactions**: MULTI, EXEC, DISCARD, WATCH
- **Streams**: XADD, XREAD, XRANGE (RESP3 native support)

### Testing

The test suite includes:
- Redis functionality tests
- RESP3 serialization/deserialization tests
- Connection handling tests
- Command pipelining tests
- Error handling tests

Run `make test` to run the tests.

## Why OCaml

1. **Pattern Matching**: The compiler ensures all variant cases are handled
2. **Configuration Validation**: Invalid configurations are caught at compile time
3. **Type Safety**: The type system catches many errors before runtime
4. **Sum Types**: Makes invalid states unrepresentable
5. **Protocol Implementation**: Types help ensure protocol correctness