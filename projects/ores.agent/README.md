# ORE Studio Agent

Standalone TypeScript agent and MCP endpoint, deployed as one process independently of OreStudio. Backend access is NATS only; internal tools are shared functions, not loopback MCP requests. Inference is a separately deployed CUDA llama.cpp server. Selected model: LiquidAI/LFM3-1.7B (official QAD Q4_0 GGUF, approximately 1.05 GB, approved by the user; runtime pinning recorded in the implementation plan).

Requires Node 22.11 or newer. From this component directory:

```sh
npm ci
npm test
npm run package
```

Build/package commands do not start or modify deployments. Packaging creates a versioned, checksummed directory in `artifacts/`, containing a bundled executable module; no production dependency installation is needed on the deployment host.

```sh
node scripts/deploy.mjs install artifacts/<release> /absolute/path/to/agent-runtime
node /absolute/path/to/agent-runtime/releases/<version-hash>/agent.mjs --config /absolute/path/to/agent-runtime/config/agent.json
```

Copy `config.example.json` into that separate deployment configuration directory first. Do not copy OreStudio's `.env`. Windows accepts absolute Windows paths for the same commands. Models, credentials, logs and runtime state stay outside source control and release packages.

Stage 1 exposes `/healthz` (process liveness) and `/readyz` (dependency readiness). Readiness is intentionally unavailable until a backend is configured and connected. Remote deployments should use a TLS reverse proxy; the process binds loopback and rejects unconfigured Host/Origin headers.

See [implementation plan](docs/plan.md). Existing OreStudio processes and its CMake/service registry are not changed by this component.
