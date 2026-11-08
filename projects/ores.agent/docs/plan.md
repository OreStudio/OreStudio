# Agent runtime design

Current design for feature #4295. Initial setup is complete. This page describes
the implemented layout; use [deployment](deployment.md) for operating commands.

## Independent units

| Unit | Role | Build/deployment |
| --- | --- | --- |
| Existing OreStudio services | Business data, rules and access control | Existing native/web deployment |
| `ores.agent` | Chat, MCP, session state, tools and evaluations | Separate TypeScript package and lifecycle |
| llama.cpp | Model inference only | Separate pinned runtime, model store and lifecycle |

Chat and external MCP calls use the same tool executor. Chat does not call its
own MCP/HTTP endpoint. The executor calls existing business operations through
one NATS adapter. Model calls use HTTP. No separate MCP gateway is needed.

The model proposes calls. Code validates arguments, identity, deadlines and
output size. The agent has no arbitrary NATS, SQL, shell or write tool.
Financial summaries remain checked client-side analysis. Optional server changes
are explained in [architecture proposals](architecture-future-prs.md).

## Model and platform

Selected model: official LiquidAI LFM3-1.7B QAD Q4_0, about 1.05 GB.
Selected runtime: llama.cpp b12172 with CUDA. Full context is 262144 tokens;
generated output defaults to 2048. See [inference](inference.md) and the lockfile.
Old Qwen, 4096-context and 512-output choices are obsolete.

Application packages are tested on Windows and Linux. Windows CUDA is verified
on the RTX 4070 Laptop GPU. Native Linux NVIDIA execution remains unverified.
The local MCP bearer profile is not a completed remote-hosting profile.

## Workspace

Use the existing fork branch `feature/local-ai-assistant`; push remote is `fork`
(`alexhock/OreStudio`). Reuse the current checkout to limit disk use. Keep agent
and inference runtime roots outside it. Do not duplicate the native build tree.

The local agent URL is `http://127.0.0.1:20100/`; inference uses port 20010.
The existing main web app uses port 20002. These are local settings, not a
requirement for every deployment.

Build, install, configure and start remain separate operations. Agent packaging
does not run CMake or download models. Existing runtime files and private config
survive agent upgrades. OS boot registration and automatic restart are not provided.

## Status

Initial runtime checkpoints are complete. Business/evaluation Stages 0–6 are also
delivered, with failures and unsupported capabilities retained. See the
[business plan](business-use-cases-plan.md) and [validation](validation.md).

Historical database recreation, queue clearing and test-report preparation were
separately approved setup actions. They are not ongoing setup instructions or
permission to repeat them. No server/data change is needed for current agent work.
