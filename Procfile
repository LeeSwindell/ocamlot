# Overmind Procfile for OCamlot Development
# Single dune watch process + service runners to avoid build lock conflicts

# Single dune process watches for changes and rebuilds everything
watcher: dune build --watch

# Services run the built binaries (restart manually with 'overmind restart <service>')
web: _build/default/web/server/main.exe
market-data: _build/default/services/market_data_publisher/bin/main.exe