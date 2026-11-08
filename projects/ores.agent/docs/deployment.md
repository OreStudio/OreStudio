# Build and deploy

Agent, inference and existing OreStudio services have separate lifecycles.
Keep runtime roots outside the checkout. Run checkout commands from
`projects/ores.agent`. Node 22.11+ is required.

## Agent package

```text
npm ci --ignore-scripts
npm run verify
npm run package
node scripts/deploy.mjs install <ARTIFACT_DIRECTORY> <APP_ROOT>
node scripts/deploy.mjs configure <APP_ROOT> <INSTALLED_RELEASE>
```

Build/package does not run CMake, download models or start services. The package
contains a bundle, controller and manifest with source revision and hashes.
A target needs Node, not npm dependencies or the checkout. Source-archive builds
must set `ORES_AGENT_SOURCE_REVISION` to a full Git SHA; otherwise packaging reads HEAD.

Install verifies hashes and refuses to overwrite damaged releases. Configure
selects a release in `service.json` and preserves `config/agent.json`.
Set the NATS URL/prefix, CA, client certificate/key, inference URL and `apiKeyFile`.
The main `.env` is not loaded. Protect config, keys and logs with file permissions.

```text
node <INSTALLED_RELEASE>/service.mjs start <APP_ROOT>/service.json
node <INSTALLED_RELEASE>/service.mjs status <APP_ROOT>/service.json
node <INSTALLED_RELEASE>/service.mjs stop <APP_ROOT>/service.json
```

The supervisor authenticates control requests and controls only its own child.
It does not stop by saved PID alone. Startup allows 30 seconds for health.
Inspect `run/stdout.log` and `run/stderr.log` after failure. There is no automatic
restart policy or OS boot registration.

| Endpoint | Purpose |
| --- | --- |
| `/` | Chat UI; local default `127.0.0.1:20100` |
| `/mcp` | Authenticated external tools |
| `/healthz` | Process health |
| `/readyz` | NATS readiness; not model/data readiness |

To upgrade or roll back, install/select the release, stop the current agent with
its installed controller, configure the selected release, then start. Keep the
old release. Private config survives; sessions do not. This does not control
main services, NATS or inference. Document/evaluation-only work needs no deployment.

## Inference

Selected model: official LiquidAI/LFM3-1.7B QAD Q4_0 GGUF, 1,049,615,680 bytes.
`inference-lock.json` pins model/license/runtime references and SHA-256 values.
Runtime: llama.cpp b12172. Keep binaries and weights outside Git.

CUDA installation needs Python 3.12+, Node and an enabled NVIDIA GPU. Windows x64
uses native CUDA 12.4 binaries. Linux x64 uses Ubuntu CUDA 12.8 binaries with
compatible glibc/drivers. CPU fallback is not a GPU test pass.

```text
python scripts/install-inference.py <INFERENCE_ROOT>
node scripts/configure-inference.mjs <INFERENCE_ROOT>
node <INFERENCE_ROOT>/service.mjs start <INFERENCE_ROOT>/service.json
node <INFERENCE_ROOT>/service.mjs status <INFERENCE_ROOT>/service.json
node <INFERENCE_ROOT>/service.mjs stop <INFERENCE_ROOT>/service.json
```

Install downloads/checks files. Configure creates the private key once and copies
the controller into the root. Neither starts inference. Set the agent's
`inference.apiKeyFile` to `<INFERENCE_ROOT>/config/api-key.txt`.

| Setting | Value |
| --- | --- |
| Address / alias | `127.0.0.1:20010` / `lfm3-1.7b` |
| Context / slots | 262144 tokens / 1 |
| GPU / CPU threads | CUDA0, all layers requested / 4 |
| Template | GGUF Jinja |
| Temperature / top_k / repeat penalty | 0.1 / 50 / 1.1 |
| Agent generated-output allowance | 2048 tokens |

Context includes input/output. Set private `inference.maxOutputTokens` to 128–8192
to change the output allowance. `finish_reason: length` is rejected. Payload limits
still apply. Full context is capacity, not proof of 256K-prompt speed or quality.

```text
node scripts/smoke-inference.mjs <PRIVATE_AGENT_CONFIG>
```

This checks generation, tool calls and fixture replay. Confirm layer offload and
GPU memory in logs; HTTP health alone is insufficient. Logs can contain prompts.
For an inference upgrade, stop, install/configure and start. Reuse keys/models.
See [validation](validation.md) for Windows evidence and the open Linux GPU check.

## NATS and access

Backend reference: `fc4f559dfb187a368bf2a8cd4053d8e833348835`.
`src/backend.ts` defines the independent operation allowlist. It uses MessagePack,
mTLS, user Bearer token, `Nats-Session-Id` and correlation IDs. Reads have an
eight-second deadline without retry. Cancellation discards pending results.
No HTTP/SQL fallback exists.

Agent tokens are short-lived and hashed in memory. Backend credentials remain
server-side. Tools check tenant/party and fields; services enforce access.
There is no automatic refresh or in-session party switch.

The local `CN=ores.agent` certificate does not have certificate-specific broker
subject permissions. Application allowlisting is not a broker boundary.
Restricted hosting needs verified broker grants, user access, authentication,
TLS proxy and Origin rules. A proxy alone does not complete that work.
Deployment commands do not rewrite broker config.

After building, check a test backend before upgrading it:

```text
node scripts/smoke-backend.mjs <PRIVATE_CONFIG> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID>
```

The party is optional if the account has a valid default. Output contains scoped
business data. Keep it private. The manual CI workflow covers Windows/Linux
Node 22/24; [validation](validation.md) states which checks actually ran.

## Separate CPU deployment

The [CPU CI workflow](evaluation.md#github-cpu-ci) uses this optional profile:

```text
python scripts/install-inference.py <CPU_ROOT> --cpu
node scripts/configure-inference.mjs <CPU_ROOT>
node <CPU_ROOT>/service.mjs start <CPU_ROOT>/service.json
node <CPU_ROOT>/service.mjs stop <CPU_ROOT>/service.json
```

Use a different root from CUDA. The installer refuses mixed profiles. CPU uses
pinned Windows/Linux archives, port 20110, `--device none` and zero GPU layers.
Context, model and sampling stay the same. It needs no NVIDIA driver or CUDA DLLs.
Downloads, including restored caches, must pass SHA-256 checks.

CI starts this profile only on its temporary runner. It does not install anything
on the local laptop or change the existing CUDA deployment.
