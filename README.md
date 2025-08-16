# OCamlot - Type-Safe Market Data Generator

A synthetic market data generator built in OCaml that leverages the language's powerful type system to make invalid states unrepresentable.

## Features

- **Type-Safe Configuration**: Strong typing prevents invalid configurations at compile time
- **Pluggable Data Formats**: Sum types allow easy extension of tick data formats
- **Multiple Generation Algorithms**: Random walk and Brownian motion for realistic price movements
- **Configurable Timing**: Control tick generation frequency from milliseconds to seconds
- **Symbol Support**: Generate data for multiple financial instruments simultaneously

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

## Building

```bash
# Install dependencies
opam install --deps-only .

# Build
dune build

# Install
dune install
```

## Usage

### Interactive Demo Mode
```bash
ocamlot
```

### Using Configuration File
```bash
ocamlot --config config.json
```

### Create Example Configuration
```bash
ocamlot --create-example example-config.json
```

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

## Type Safety Benefits

1. **Exhaustive Pattern Matching**: The compiler ensures all variants of sum types are handled
2. **Configuration Validation**: Invalid configurations are caught at parse time
3. **No Runtime Type Errors**: Strong typing prevents many classes of runtime errors
4. **Easy Extension**: Adding new formats or algorithms is safe and guided by the compiler