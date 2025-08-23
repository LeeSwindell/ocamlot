# Market Data Caching Strategy & Architecture

## Message Queue Comparison for OMS/Risk Systems

### Data Types & Requirements

**Real-time Market Data** (microsecond-critical):
- Price ticks, quotes, trades
- Requires: Ultra-low latency, high throughput
- Best fit: NATS (in-memory) or Redis

**Order Management** (millisecond-critical):
- Order placement, fills, cancellations
- Requires: Persistence, guaranteed delivery, audit trail
- Best fit: Kafka or Redis Streams

**Risk Calculations** (second-level):
- Position updates, P&L, Greeks
- Requires: Stateful processing, replay capability
- Best fit: Kafka (event sourcing)

**Reference Data** (minute-level):
- Symbol info, trading rules
- Requires: Fast lookups, TTL support
- Best fit: Redis Cache

## Service Performance Comparison

### NATS
- Latency: <1ms
- Throughput: 18M msgs/sec
- Persistence: JetStream (optional)
- Use for: Market data fan-out, ephemeral notifications

### Kafka
- Latency: 2-10ms
- Throughput: 1M msgs/sec per partition
- Persistence: Configurable retention (days/GB)
- Use for: Order events, trade history, compliance

### Redis Streams
- Latency: <1ms
- Throughput: 1M msgs/sec
- Persistence: AOF/RDB snapshots
- Use for: Order queues, recent trades buffer

### RabbitMQ
- Latency: 1-5ms
- Throughput: 50K msgs/sec
- Persistence: Durable queues
- Use for: Task distribution, RPC patterns

## Redis Streams Deep Dive

### Architecture
Redis Streams is essentially a **persistent, replicated append-only log** with consumer groups - fundamentally a distributed WAL.

**Storage Model:**
- In-memory first with optional persistence (AOF/RDB)
- Radix tree structure for O(1) access by ID
- Each entry: `<timestamp-sequence>` ID + field-value pairs
- Memory-optimized: listpacks for small streams, rax trees for large

**Replication:**
- Single-master, multiple replicas
- Async replication by default
- No built-in partitioning (must shard manually)

### Performance Characteristics
```
Redis Streams:
- Write latency: 100-500μs
- Read latency: 50-200μs  
- Throughput: 500K-1M msgs/sec (single node)
- Message size: Optimal <1MB
- Retention: Limited by RAM (typically hours/days)

Kafka:
- Write latency: 2-10ms (acks=1), 10-30ms (acks=all)
- Read latency: 1-5ms
- Throughput: 1M+ msgs/sec per partition
- Message size: Optimal <1MB, max 1GB
- Retention: Limited by disk (typically days/years)
```

### Why Redis Streams for Trading

Given the daily cycle of trading:
- **9:30 AM**: Market open, positions loaded from file/API
- **4:00 PM**: Market close, non-GTC orders cancelled
- **Post-close**: Reconciliation, position export, clear working state

This is fundamentally a **daily ephemeral workload** with checkpoint/restore semantics.

**Cost Comparison:**
```
Redis Streams (32GB RAM + S3 backup): ~$300/month
Kafka (3-node cluster, 7-year retention): ~$1400/month
```

## Scaling Redis Streams

### Sharding Strategy with Consistent Hashing

```python
class ShardedRedisStreams:
    def __init__(self, shards):
        self.shards = shards  # List of Redis connections
        self.num_shards = len(shards)
    
    def get_shard(self, key):
        # Consistent hashing
        hash_val = hashlib.md5(key.encode()).hexdigest()
        shard_id = int(hash_val, 16) % self.num_shards
        return self.shards[shard_id]
    
    def xadd(self, symbol, data):
        # Shard by symbol for locality
        shard = self.get_shard(symbol)
        stream_key = f"orders:{symbol}"
        return shard.xadd(stream_key, data)
```

### Common Sharding Patterns

1. **By Symbol** (Best for market data)
   - All AAPL data on same node (cache locality)
   - Risk: Hot symbols can overload shards

2. **By Account** (Best for orders)
   - Each account's orders on same shard
   - Risk: Large accounts create hotspots

3. **By Time Window** (Best for logs)
   - Round-robin with time buckets
   - Even distribution but queries span shards

### Performance at Scale
- Single Redis Stream: ~500K msgs/sec, 100GB practical max
- 10-node sharded setup: ~5M msgs/sec, 1TB aggregate, <1ms latency

## Market Data Architecture Patterns

### The Options/Futures Scale Problem
```
Equities: ~8,000 symbols
Options: ~600,000 contracts (150 per underlying × 4,000 underlyings)
Futures: ~50,000 contracts across all expiries
```

### Approach 1: Queue + Local Cache (Push Model)
**Pros:**
- Ultra-low latency (<1μs lookups)
- No network calls for calculations
- Temporal consistency

**Cons:**
- Memory explosion (10 consumers × 10GB = 100GB)
- Long startup time
- Stale data risk
- Impossible for 600K+ options

### Approach 2: Shared Redis Cache (Pull Model)
**Pros:**
- Memory efficient (single copy)
- Always fresh data
- Fast startup
- Scales to options

**Cons:**
- Network latency (50-200μs)
- Redis bottleneck potential
- No temporal consistency

### Recommended Hybrid Architecture

```python
class HybridMarketDataSystem:
    """
    Best of both worlds approach
    """
    def __init__(self):
        # Layer 1: NATS for broadcasting (push)
        self.nats = NATS()
        
        # Layer 2: Redis for latest quotes (pull)
        self.redis = Redis()
        
        # Layer 3: Local cache for hot symbols
        self.local = LRUCache(10000)
        
        # Options: Calculate on-demand
        self.iv_cache = {}  # Cache IV, calc prices
```

## Greeks-Based Options Caching

### The Problem
Instead of caching 600K option prices (200MB per snapshot), cache the inputs and calculate on demand.

### The Solution
Cache:
- Underlying prices (8,000 symbols)
- Implied Volatility surfaces
- Risk-free rate
- Dividend yields

Calculate on-demand:
- Option prices via Black-Scholes
- Greeks (Delta, Gamma, Theta, Vega, Rho)

## OCaml Implementation

### Black-Scholes Pricing Engine

```ocaml
module OptionPricer = struct
  let black_scholes ~spot ~strike ~time ~rate ~volatility ~is_call =
    let d1 = (log(spot /. strike) +. (rate +. 0.5 *. volatility ** 2.) *. time) 
             /. (volatility *. sqrt time) in
    let d2 = d1 -. volatility *. sqrt time in
    
    if is_call then
      spot *. normal_cdf d1 -. strike *. exp(-. rate *. time) *. normal_cdf d2
    else
      strike *. exp(-. rate *. time) *. normal_cdf (-. d2) -. spot *. normal_cdf (-. d1)
  
  let calculate_greeks ~spot ~strike ~time ~rate ~volatility ~is_call =
    (* Returns record with delta, gamma, theta, vega, rho *)
    ...
end
```

### Hierarchical Cache Implementation

```ocaml
module MarketDataCache = struct
  type t = {
    conn: Redis.connection;
    mutable local_cache: (string, Quote.t) Hashtbl.t;
    cache_size: int;
  }
  
  let get_quote t symbol =
    (* L1: Local memory *)
    match Hashtbl.find_opt t.local_cache symbol with
    | Some quote when Quote.is_fresh quote -> 
      Lwt.return quote
    | _ ->
      (* L2: Redis *)
      let* redis_data = Redis.hgetall t.conn (Printf.sprintf "q:%s" symbol) in
      match redis_data with
      | Some data ->
        let quote = Quote.of_redis data in
        Hashtbl.replace t.local_cache symbol quote;
        Lwt.return quote
      | None ->
        Lwt.fail (Not_found)
end
```

### LRU Cache

```ocaml
module LRU = struct
  type ('k, 'v) t = {
    capacity: int;
    mutable size: int;
    table: ('k, 'v node) Hashtbl.t;
    mutable head: 'v node option;
    mutable tail: 'v node option;
  }
  and 'v node = {
    key: 'k;
    mutable value: 'v;
    mutable prev: 'v node option;
    mutable next: 'v node option;
  }
  
  let get t key =
    match Hashtbl.find_opt t.table key with
    | Some node ->
      move_to_front t node;
      Some node.value
    | None -> None
  
  let put t key value =
    (* Add/update with LRU eviction *)
    ...
end
```

### Integrated Options Engine

```ocaml
module OptionsEngine = struct
  type t = {
    market_data: MarketDataStream.t;
    iv_surfaces: (string, IVSurface.t) Hashtbl.t;
    risk_free_rate: float ref;
    pricing_cache: LRU.t;
  }
  
  let get_option_quote t symbol =
    (* Check cache first *)
    match LRU.get t.pricing_cache symbol with
    | Some quote when Quote.is_fresh quote -> 
      Lwt.return quote
    | _ ->
      (* Parse option symbol *)
      let (underlying, expiry, is_call, strike) = parse_option_symbol symbol in
      
      (* Get underlying price *)
      let* spot_quote = MarketDataCache.get_quote t.market_data.cache underlying in
      
      (* Get IV from surface *)
      let iv = IVSurface.interpolate_iv surface ~expiry_days ~strike in
      
      (* Calculate price and Greeks *)
      let price = OptionPricer.black_scholes 
        ~spot ~strike ~time ~rate ~volatility:iv ~is_call in
      
      let greeks = OptionPricer.calculate_greeks
        ~spot ~strike ~time ~rate ~volatility:iv ~is_call in
      
      (* Cache and return *)
      LRU.put t.pricing_cache symbol quote;
      Lwt.return quote
end
```

## Required OCaml Libraries

```dune
(executable
 (name trading_engine)
 (libraries
  core lwt lwt.unix
  redis redis-lwt
  nats
  websocket-lwt-unix
  yojson ptime owl
  logs logs.lwt))
```

## Installation

```bash
# Install OPAM packages
opam install redis redis-lwt nats websocket-lwt-unix yojson ptime owl logs

# Some might need custom packages
git clone https://github.com/nats-io/nats.ml
cd nats.ml && opam install .
```

## Performance Considerations

### OCaml Advantages:
- No GC pauses with proper tuning (important for HFT)
- Native compilation rivals C++ performance
- Type safety prevents runtime errors
- Functors for generic market data handling
- Multicore support (OCaml 5.0+) for parallel processing

### OCaml Challenges:
- Fewer libraries than Python/Java
- NATS client less mature
- Numerical libraries (Owl) not as complete as NumPy

## Final Architecture Recommendation

For a modern OMS starting today:

1. **Redis Streams** for hot path (orders, positions, real-time risk)
   - 24-hour retention in memory
   - Daily backups to S3/GCS

2. **NATS** for market data fan-out
   - Pure pub/sub for speed
   - No persistence needed

3. **PostgreSQL** for searchable trade history
   - Daily ETL from Redis
   - Indexed for compliance queries

4. **Greeks-based** options pricing
   - Cache underlyings + IV surfaces
   - Calculate options on-demand
   - 600K contracts → 4K cached items

This architecture provides:
- μs-latency where needed
- Durability where required  
- Manageable operational complexity
- Cost-effective scaling
- Compliance-ready archival