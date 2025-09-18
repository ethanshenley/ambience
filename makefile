.PHONY: all build test clean run-example

all: build

build:
	dune build

test:
	dune test

clean:
	dune clean

# Run GPU marketplace example
run-gpu-provider:
	dune exec examples/compute_market/main.exe -- provider nvidia:4090 5.0

run-gpu-consumer:
	dune exec examples/compute_market/main.exe -- consumer nvidia:4090 2.0 6.0

run-market-maker:
	dune exec examples/compute_market/main.exe -- market-maker

# Run energy trading example
run-energy-producer:
	dune exec examples/energy_trading/main.exe -- producer 10.0 0.12

run-energy-consumer:
	dune exec examples/energy_trading/main.exe -- consumer 5.0 0.15

run-grid:
	dune exec examples/energy_trading/main.exe -- grid

run-smart-meter:
	dune exec examples/energy_trading/main.exe -- smart-meter house_001

# CLI commands
cli-post:
	dune exec bin/cli.exe -- post -o compute:gpu:nvidia:4090 -w currency:fiat:USD -q 1.0 -p 5.0

cli-list:
	dune exec bin/cli.exe -- list

cli-stats:
	dune exec bin/cli.exe -- stats

cli-monitor:
	dune exec bin/cli.exe -- monitor

# Development
watch:
	dune build -w

format:
	dune fmt

doc:
	dune build @doc
	open _build/default/_doc/_html/index.html

install:
	dune install

uninstall:
	dune uninstall