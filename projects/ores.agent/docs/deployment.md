# Deploy the agent separately

Use Node 22.11 or later. Run these commands from `projects/ores.agent`:

```text
npm ci --ignore-scripts
npm run verify
npm run package
```

The commands do not run CMake, download models or start services. The package
contains `agent.mjs`, `service.mjs` and a manifest with source revision and hashes.
A target host needs Node but does not need npm dependencies or the checkout.
For a source archive, set `ORES_AGENT_SOURCE_REVISION` to the full Git SHA before
packaging. Otherwise, packaging reads HEAD.

## Install and configure

```text
node scripts/deploy.mjs install <ARTIFACT_DIRECTORY> <APP_ROOT>
node scripts/deploy.mjs configure <APP_ROOT> <INSTALLED_RELEASE>
```

Put `<APP_ROOT>` outside the checkout and main server installation. Install
checks file hashes and refuses to overwrite a damaged release. Configure selects
a release in `service.json`. It preserves existing `config/agent.json`.

Set the NATS URL, prefix, CA, client certificate and key in that private config.
Set the inference URL and `apiKeyFile`. The agent does not load the main `.env`.
Protect configuration, keys and logs with file permissions. See the
[backend contract](backend-contract.md) for access limits.

## Operate the installed release

```text
node <INSTALLED_RELEASE>/service.mjs start <APP_ROOT>/service.json
node <INSTALLED_RELEASE>/service.mjs status <APP_ROOT>/service.json
node <INSTALLED_RELEASE>/service.mjs stop <APP_ROOT>/service.json
```

The supervisor controls only its own child. It authenticates control requests;
it does not stop a process using a saved PID alone. Startup allows 30 seconds
for health. Inspect `run/stdout.log` and `run/stderr.log` after failure.
There is no automatic restart policy or OS boot registration.

| Endpoint | Purpose |
| --- | --- |
| `/` | Chat UI |
| `/mcp` | Authenticated MCP |
| `/healthz` | Agent process health |
| `/readyz` | NATS connection readiness |

Readiness does not prove model readiness or business-data availability.
MCP read tools can work while inference is stopped.

## Upgrade or roll back

Install the selected artifact. Stop the current agent with its installed
controller. Configure the selected release, then start it. Keep the previous
release for rollback. Private config survives; login sessions do not.

This procedure does not control OreStudio, NATS or inference. Inference has its
own root and [lifecycle](inference.md). Agent code changes need a new package;
evaluation-only runs and document edits need no deployment.

The manual CI workflow covers Windows/Linux with Node 22/24. It has no PR trigger.
See [validation](validation.md) for actual runs; configured CI is not evidence
that CI or native Linux CUDA has run.
