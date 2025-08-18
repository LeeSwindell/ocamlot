# OCamlot VS Code Devcontainer

This directory contains VS Code devcontainer configuration for the OCamlot trading system.

## Quick Start

### Option 1: VS Code Devcontainer (Recommended)
1. Open this repository in VS Code
2. Click "Reopen in Container" when prompted (or use Command Palette: "Dev Containers: Reopen in Container")
3. Wait for services to start automatically
4. Start coding! Changes will auto-reload.

### Option 2: Command Line
```bash
# Start complete development environment
make dev-up

# Watch logs
make dev-logs

# Stop environment
make dev-down
```

## What's Included

- **OCaml Development Environment**: Base image with all dependencies
- **Hot Reload**: `dune exec --watch` automatically rebuilds on file changes
- **Complete Infrastructure**: NATS, Redis, all OCamlot services
- **Port Forwarding**: All services accessible on localhost
- **VS Code Extensions**: OCaml Platform, Docker, and more

## Services Started

| Service | Port | Description |
|---------|------|-------------|
| NATS Server | 4222 | Message broker |
| NATS Monitor | 8222 | NATS web UI |
| Redis | 6379 | Cache/storage |
| Web Dashboard | 8080 | Trading dashboard |
| Market Data | - | Publishing to NATS |

## Development Workflow

1. **Edit Code**: Make changes to any `.ml` files
2. **Auto Rebuild**: Dune automatically detects changes and rebuilds
3. **Hot Reload**: Services restart with new code
4. **Test**: Use web dashboard at http://localhost:8080

## Commands

```bash
make dev-up       # Start everything
make dev-down     # Stop everything  
make dev-logs     # Watch all logs
make dev-rebuild  # Rebuild when dependencies change
```

## Troubleshooting

**Services not starting?**
```bash
make dev-down
make dev-rebuild
make dev-up
```

**Port conflicts?**
Check if ports 4222, 6379, 8080, 8222 are available.

**Build issues?**
Run `make dev-rebuild` to rebuild the base image with fresh dependencies.