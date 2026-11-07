# OreStudio Agent

This component now has a read-only agent UI and a standard MCP endpoint in one
process. OreStudio access uses NATS only; llama.cpp is a separate inference
process. Neither is part of the main CMake build or existing deployment fleet.

See [deployment](docs/deployment.md), [CUDA inference](docs/inference.md),
[agent usage](docs/agent.md), [external MCP](docs/mcp.md) and
[backend contract](docs/backend-contract.md) for current runnable instructions.
Version 0.1.1 adds [FX spot and basic IR lookups](docs/market-lookups.md), with
exact point matching, provider clarification and dated freshness information.
The [active implementation plan](docs/business-use-cases-plan.md) now focuses
on agent evaluations, an honest failing/blocked baseline and agent-only
improvements using existing APIs. Reporting/server architecture and command
capabilities are [separate future PR proposals](docs/architecture-future-prs.md)
for project-lead discussion; they do not block the evaluation work.
The [Stage 4 evaluation runner](docs/evaluation.md) provides offline replay,
real-model/fixture and opt-in live-read profiles via `npm run eval`. Reports
retain failures and blocked capabilities; normal mode does not require high scores.
The [Stage 0 baseline](docs/business-baseline.md) records live data readiness
and how to rerun the read-only operator inspection.
Version 0.1.2 adds [Stage 1 discovery](docs/discovery.md): trades, books,
portfolios, counterparties and scoped trade economics.
Version 0.1.3 adds [stored trade results and run health](docs/reporting.md),
with explicit handling of partial output. Its [clean live NPV gate](docs/valuation-validation.md) passed separately.
Version 0.1.4 adds [historical book/portfolio NPV summaries](docs/scope-reporting.md)
with complete coverage checks, explicit currency metadata and paginated contributors.
Version 0.1.5 adds [stored raw Greek buckets](docs/greeks.md): Delta, Gamma,
cash vega at its stated volatility bump, and signed ORE rate Delta at a 1bp
target bump. Exact factor/bump/scenario/currency buckets remain separate;
missing members withhold that bucket's total.

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
