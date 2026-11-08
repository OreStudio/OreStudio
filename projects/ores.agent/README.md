# OreStudio Agent

Current code: **0.1.9**. This is a local, read-only assistant. Chat and MCP share
one process. Business calls use NATS. A separate llama.cpp process runs the model.
The agent has its own build and deployment. It does not use the main CMake build.

## Start here

| Need | Guide |
| --- | --- |
| Use chat and see examples | [Agent](docs/agent.md) |
| Install, start, stop or roll back | [Deployment](docs/deployment.md) |
| Configure CUDA and the model | [Inference](docs/inference.md) |
| Connect an external MCP client | [MCP](docs/mcp.md) |
| Understand identity and NATS access | [Backend contract](docs/backend-contract.md) |
| Run evaluations | [Evaluation](docs/evaluation.md) |
| Compare results and inspect timing | [Repeatable evaluation](docs/reproducible-evaluation.md) |
| Review current scope and remaining work | [Business plan](docs/business-use-cases-plan.md) |
| Discuss future server changes | [Architecture proposals](docs/architecture-future-prs.md) |
| Check release evidence and known gaps | [Validation](docs/validation.md) |

Use Node 22.11 or later. From this directory:

```text
npm ci --ignore-scripts
npm run verify
npm run package
```

These commands test and package the agent. They do not deploy it, start services
or download a model. Keep private configuration, keys, reports and model files
outside Git.

Business guides: [market data](docs/market-lookups.md),
[discovery](docs/discovery.md), [trade values](docs/reporting.md),
[scope totals](docs/scope-reporting.md) and [Greeks](docs/greeks.md).
Design: [runtime layout](docs/plan.md), [intent](docs/intent-workflows.md) and
[typed requests](docs/request-plan.md).

Dated baseline and validation pages are historical evidence. They are not current
setup instructions. Their links are in [validation](docs/validation.md).
