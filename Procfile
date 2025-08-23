# Overmind Procfile for OCamlot Development
# Single dune watch process + service runners to avoid build lock conflicts

# Single dune process watches for changes and rebuilds everything
watcher: dune build --watch

# Services run the built binaries (restart manually with 'overmind restart <service>')
oms: _build/default/services/oms/bin/main.exe
risk: _build/default/services/risk/bin/main.exe
simulator: _build/default/services/simulator/bin/main.exe
web: _build/default/web/server/main.exe
market-data: _build/default/services/market_data_publisher/bin/main.exe
ingress: _build/default/services/ingress/bin/main.exe
egress: _build/default/services/egress/bin/main.exe
monitor: _build/default/services/monitor/bin/main.exe