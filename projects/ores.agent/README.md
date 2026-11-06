# OreStudio Agent

This component now has a read-only agent UI and a standard MCP endpoint in one
process. OreStudio access uses NATS only; llama.cpp is a separate inference
process. Neither is part of the main CMake build or existing deployment fleet.

See [deployment](docs/deployment.md), [CUDA inference](docs/inference.md),
[agent usage](docs/agent.md), [external MCP](docs/mcp.md) and
[backend contract](docs/backend-contract.md) for current runnable instructions.
Version 0.1.1 adds [FX spot and basic IR lookups](docs/market-lookups.md), with
exact point matching, provider clarification and dated freshness information.
The [staged business-use-case plan](docs/business-use-cases-plan.md) covers the
next reporting, booking, quotation and workflow deliveries and their gates.
The [Stage 0 baseline](docs/business-baseline.md) records live data readiness
and how to rerun the read-only operator inspection.
Version 0.1.2 adds [Stage 1 discovery](docs/discovery.md): trades, books,
portfolios, counterparties and scoped trade economics.
Version 0.1.3 adds [stored trade results and run health](docs/reporting.md),
with explicit handling of partial output and a still-open clean live NPV gate.

Requires Node 22.11+. From this component directory:

```sh
npm ci --ignore-scripts
npm test
npm run package
```

Build/package creates an artifact without starting services. Install and
configure that artifact outside the checkout as described in the deployment
guide; no production npm install is needed. Use the dedicated inference
installer for LiquidAI/LFM3-1.7B QAD Q4_0 and CUDA llama.cpp. Models, private
configuration, keys, logs and process state remain outside Git.

`/` serves the agent UI, `/mcp` serves authenticated external clients,
`/healthz` checks process liveness and `/readyz` checks NATS readiness.
The process binds loopback; remote hosting requires a TLS proxy and explicit
Origin configuration. See [plan](docs/plan.md) for the agreed scope and
[validation](docs/validation.md) for tested deployments and remaining limits.
