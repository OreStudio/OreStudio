# OreStudio Agent

Version **0.1.9**. A local, read-only assistant with chat and MCP in one process.
Business calls use NATS. A separate llama.cpp process runs the model.

## Guides

| Need | Document |
| --- | --- |
| Chat, MCP, business tools and limits | [Usage](docs/agent.md) |
| Build, install, CPU/CUDA, access and rollback | [Deployment](docs/deployment.md) |
| Datasets, evaluation, comparison and timing | [Evaluation](docs/evaluation.md) |
| Discuss component ownership and future changes | [Architecture proposals](docs/architecture-future-prs.md) |
| Check tested releases, known gaps and historical evidence | [Validation](docs/validation.md) |

Use Node 22.11 or later. From this directory:

```text
npm ci --ignore-scripts
npm run verify
npm run package
```

These commands test and package the agent. They do not deploy it, run CMake,
start services or download models. Keep private config, keys and reports out of Git.

## Design and current scope

Chat and MCP call one internal tool executor. The model proposes calls; code
checks arguments and identity. The executor uses an independent NATS adapter,
not the moving main web/BFF package. No extra MCP gateway is needed.

Keep agent and inference releases outside the checkout. Use the fork
branch `fix/agent-review-comments` and the existing native build tree. Agent work does not
require a new main-server build.

Local agent/evaluation Stages 0–6 are delivered, with recorded gaps. Later releases
added typed requests and Markdown replies. Current work remains in the agent,
its UI, tests and evaluations. Do not change backend code, broker settings,
schemas, business records or the main UI to improve a score.

Next: use evaluation traces to select one agent improvement and compare it with
a fixed baseline. Preserve failures and checks. Server aggregation is optional;
future business actions need separate agreement. Open a PR only when requested.

`Agent CPU evaluation` is separate CI for real-model fixture tests on Windows/Linux.
See [CPU CI](docs/evaluation.md#github-cpu-ci) for triggers, results and failure rules.
