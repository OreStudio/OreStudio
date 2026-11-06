# Feature #4295: local AI assistant

Planning date: 2026-11-04. Updated: 2026-11-06. This is the versioned implementation plan; a convenience copy is kept beside the checkout.
Source: https://github.com/OreStudio/OreStudio/issues/4295 and its three comments.

## Current implementation status (2026-11-06)

Six runnable checkpoints are committed and pushed to the fork. The combined
agent/MCP application is deployed independently at http://127.0.0.1:20100/.
llama.cpp b12172 serves the approved official LiquidAI LFM3-1.7B QAD Q4_0
model on port 20010 with all 25 layers offloaded to the RTX 4070. Builds and
deployments are separate; no main-server compilation or restart was required.

The first read-only deliverable is running: real NATS currencies, market-series
and observation tools, official external MCP client interoperability, a bounded
direct tool loop and a separate authenticated UI. Twelve tests pass on Windows
and Linux. Independent stop/restart checks leave all 25 native backend/broker
process IDs unchanged. Release source: `88fe926d56fa64653a70e4c009cab53eaa3cbda3`;
artifact identity: `512b6069701e` (identical Windows/Linux builds).

Remaining validation/hardening: native Linux NVIDIA GPU execution; restricted
broker subject ACLs for the dedicated certificate; a hosted OAuth/client
enrollment profile beyond the documented local bearer profile. The sampled
market series has no observations, so the assistant reports missing data.
No successful live FX quote, booking or two-way pricing implementation is
claimed. See the component's deployment, validation, MCP and inference guides
for current commands and evidence. Historical planning notes below record the
initial environment setup and broader follow-up scope.

## Local workspace

| Item | Setting |
| --- | --- |
| Checkout | `C:/Users/alexa/Documents/projects/orestudio/OreStudio` |
| Feature branch | `feature/local-ai-assistant` |
| Starting upstream commit | `d99da6fc86fda2f60c3162f2430d1930d9b1ad78` |
| Current upstream baseline (2026-11-05) | `fc4f559dfb187a368bf2a8cd4053d8e833348835` |
| Fork / push remote | `fork` = `https://github.com/alexhock/OreStudio.git` |
| Upstream / fetch remote | `origin` = `https://github.com/OreStudio/OreStudio.git` |
| Environment | Existing native Windows environment, reused to conserve disk |
| Base port | `20000` |
| Existing C++ HTTP release / web ports | `20001` / `20002` |
| NATS / monitoring | Existing configuration retained, NATS client `20005` |
| Development database | `ores_dev_orestudio`, recreated on current-main schema with explicit user authorization |
| Proposed inference port | `20010`, verify availability before launch |

The original checkout and its existing CMake build tree, vcpkg installation, web dependencies and ignored `.env` provide the running OreStudio backend reference deployment. The combined agent/MCP application and inference deployment will be built and operated separately, as specified below; they will not use that CMake build tree or `.env`. The existing branch-specific `pushRemote` is `fork`. The feature branch is now pushed to the fork in six runnable implementation checkpoints; see `docs/validation.md`.

On 2026-11-05, following the user's disk-space concern and request to reuse the same worktree, the clean duplicate `ores_dev_calm_turing` worktree was removed and `feature/local-ai-assistant` was checked out in `OreStudio`. The retired environment skeleton was retained outside Git as `retired-calm-turing.env`; no feature database had been created. The removed checkout contained about 1.64 GiB of files, including generated web dependencies. Git history and the prior Windows branches remain available.

`OreStudio-upstream-sync` remains as the submitted Windows PR checkout. Its completed login-audit CMake `build/output/login-upgrade-audit/projects` directory (about 26.72 GiB of generated compilation output) was removed. The audit's `runtime` directory, database/role backups, published executable, logs and reports were retained. The original checkout's approximately 45.92 GiB native CMake build tree was retained for reuse. No databases or running services were changed.

## Windows dependency and isolation

PR #4461 merged on 2026-11-04 at 21:44:34 UTC, with merge commit `ca336730cd7f04ce2b5f9509617ce50b55e87a6f`. On 2026-11-05 the clean feature branch was fast-forwarded to upstream main, `fc4f559dfb187a368bf2a8cd4053d8e833348835`; that baseline contains the Windows merge. Its push destination remains the fork. Native full-stack execution has since been built and verified as recorded below.

The previously proposed temporary Windows integration branch is no longer needed. Work directly in the original checkout using upstream's native Windows support. Keep the existing build directory at its existing path; reconfigure it when needed and rebuild required targets incrementally. Do not copy it to a different checkout or create a duplicate full build. The bundled Windows ORE package remains reusable.

The initial consolidation retained PostgreSQL data, credentials, IAM/NATS keys and service configuration. During deployment the old schema was confirmed incompatible, and the repository provides no incremental migration path. The user explicitly confirmed the old demo data had no value and authorized deleting/recreating `ores_dev_orestudio`. It has now been recreated at fingerprint `71153f931207e254`, matching source commit `fc4f559dfb`. The user separately approved clearing the 17 old local JetStream streams. Existing local passwords and keys were retained. The attempted old-data dump was cancelled after that authorization; no successful database backup is claimed.

Deployment completed: the web and all required native targets passed in the original build tree, resuming after the battery pause with six parallel jobs. All 25 application processes are running with `ORES_REQUIRE_SCHEMA_MATCH=1`. Acme setup completed with 1,000 current trades; system/tenant sign-in, party access, health and web HTML checks passed. The app is at `http://127.0.0.1:20002/`. Linux and Windows ORE packages were published; no new engine build or pricing batch was required. See `installation-status.md` for current logs and startup instructions.

## First deliverable

A user asks for supported market data; the standalone TypeScript agent/MCP application sends the prompt and tool schemas to a local LiquidAI LFM3-1.7B QAD Q4_0 model served by llama.cpp. The agent invokes its shared tool-execution layer directly in process; the NATS adapter obtains the answer from OreStudio under the user's authorization. The assistant returns the value with its observation date and source. External MCP clients invoke the same tools through the application's MCP endpoint. A missing or ambiguous quote produces a clear answer or clarification, never an invented value.

Start with one read-only capability, preferably an FX spot lookup. Confirm the actual backend query and seeded data first. A currency lookup can establish transport interoperability while that query is wired. Fixtures must be visibly identified as fixtures.

The issue also asks for external MCP clients. Prove an independent client can discover and call the same tool, without going through the assistant or running a model.

## Confirmed architecture — combined standalone agent/MCP application

Decision, 2026-11-06: run the agent and MCP endpoint inside one standalone TypeScript application and one process. Keep that application outside the main C++ HTTP server and existing web BFF. It has its own build, configuration and release, independent of OreStudio. This supersedes the earlier separate-agent/separate-gateway proposal. llama-server remains a separate inference process.

Confirmed backend transport decision: the combined application uses NATS exclusively for OreStudio operations, including supported authentication/session and caller-delegation operations. There is no HTTP backend adapter or HTTP fallback. External MCP clients use Streamable HTTP to reach the application's MCP endpoint; the built-in agent calls shared tool handlers directly, without an HTTP call to itself. llama-server retains its inference HTTP API. This does not introduce a custom MCP-over-NATS protocol or remove OreStudio's existing HTTP server.

```text
Agent UI                     External MCP clients
   |                               | MCP over HTTP
   v                               v
+-------------------------------------------------+
| Standalone agent/MCP application (one process)   |
|                                                 |
| Agent loop ---> shared tool execution <--- MCP   |
|     |           validation + authorization       |
|     |                    |                      |
|     |                NATS adapter               |
+-----|--------------------|----------------------+
      v                    v
 llama-server          NATS broker
 inference only            |
                           v
                  Existing OreStudio services
                  business rules and data ownership
```

- Run the combined application, OreStudio and inference independently. The agent and MCP endpoint share deployment, restart, rollback and process-failure boundaries; they are separate modules, not separate services. Existing OreStudio remains available when the combined application is stopped. Apply CPU/memory/concurrency limits: process separation alone does not eliminate shared-host resource contention, especially for inference.
- Use the official TypeScript MCP server SDK for external access. The built-in agent does not need an MCP client connection to its own endpoint. An MCP client SDK remains useful for external-client interoperability tests. Do not use the SDK's test/development in-memory transport as the production shortcut; call the shared execution layer directly.
- Keep the implementation in the user's existing `alexhock/OreStudio` fork, on `feature/local-ai-assistant`, as a standalone component proposed at `projects/ores.agent`. This supersedes the earlier separate-repository suggestion: repository ownership and build/deployment independence are separate choices. Give the component its own lockfile, CI, configuration and release artifact; do not add it to the main CMake build or existing fleet lifecycle. Suggested modules are `agent`, `mcp`, `tools`, `auth`, `ores-nats` and `inference`, all in one deployable application. The component is implemented in `projects/ores.agent`. It must not import or symlink the main application's source or require its CMake build. Keep the existing checkout/build available as the backend test deployment. The TypeScript MCP implementation differs from the original issue's C++ MCP proposal and should be documented when discussing it upstream.
- Start with a separate agent UI or client harness so prompt/model/tool-loop experiments require no OreStudio web changes. A later main-app integration can use one stable agent API. Do not treat the existing BFF's opaque session cookie as a portable credential for a separately hosted agent.
- Keep all OreStudio access inside one NATS adapter with domain operations such as `readFxSpot` and `readTradeResults`. Use existing authenticated request/reply operations for reads and commands, and explicitly selected subscriptions for updates. Confirm the actual subjects, schemas and backend capability for each tool. The agent never selects a raw subject or receives broker credentials.
- Give the combined application a dedicated service identity, restricted publish/subscribe permissions and supported caller delegation. Broker permissions do not replace backend user/tenant/party authorization. Resolve missing capabilities through a supported NATS domain operation or leave that tool unavailable; do not introduce an HTTP fallback.
- Contain wire-protocol coupling in the adapter. The current TypeScript wire package is private and versioned `0.0.1`; it is not an independently published stable SDK. Pin any reused generated client artifact to an exact source revision and test compatibility rather than importing the moving checkout. No MCP route or HTTP endpoint is needed in the main server for each agent tool.
- Use explicit deadlines and no-responder handling for request/reply; define reconnect and subscription cleanup behavior. A timeout does not establish that a write failed, so never blindly retry a booking or other mutation. Use a supported durable workflow and idempotency mechanism where required; core NATS request/reply is not itself a durable job queue.
- Do not access OreStudio tables directly or duplicate pricing/booking rules in the combined application. Backend services remain authoritative for validation and permissions.
- Evaluate the existing `llama-server` executable before building a custom C++ wrapper. It already provides HTTP inference, structured output and tool calling. Pin its revision, model, GGUF checksum, license, quantization, chat template and launch flags after a smoke test.
- The model proposes tools. Maintain one tool catalogue and one execution entry point, such as `executeTool(name, arguments, authenticatedContext)`, used by both the agent loop and MCP handlers. Both paths enforce the same schemas, tool visibility, user/tenant/party permissions, rate/concurrency limits, deadlines, cancellation, output bounds and audit logging. The authenticated context comes from trusted application/session or MCP authentication, never from model-supplied claims. Tools accept typed domain arguments; the model interprets customer shorthand. The tools module contains no model-specific prompts or inference runtime.
- Use Streamable HTTP with a pinned MCP revision. Determine streaming, cancellation and compatibility behavior against that revision, rather than implementing the issue's generic HTTP/SSE sketch as a new protocol.

## Inference server requirements and alternatives

Requirement confirmed 2026-11-06: the inference server must deploy on both Windows and Linux. Keep it as a separate process from the combined agent/MCP application and OreStudio. Native Windows support is the baseline; a Windows deployment must not depend on WSL. Linux may use a native process or a container. The selected GPU backend can differ by OS, but the agent-facing inference contract must remain the same.

The issue proposes llama.cpp. The selected initial runtime is its existing CUDA-enabled `llama-server`, independently deployed on native Windows and NVIDIA-equipped Linux; do not build a new C++ inference wrapper. Prefer a compatible pinned prebuilt CUDA release, including required runtime libraries, before considering a separate source build. The pinned CUDA runtime and approved model are installed in the separate local runtime; see `docs/inference.md`.

### Selected GPU test model

The user replaced Qwen with LiquidAI/LFM3-1.7B and explicitly approved its official QAD Q4_0 GGUF at approximately 1.05 GB, overriding the earlier strict 1 GB artifact limit. Use `LiquidAI/LFM3-1.7B-GGUF`, file `LFM3-1.7B-QAD-Q4_0.gguf`, not Qwen or NVFP4. The RTX 4070 is an Ada GPU; do not select a quantization for Blackwell-only acceleration. Pin the model revision/hash and an LFM3-capable CUDA llama.cpp release (official model documentation requires b11960/v0.8.0 or newer). Start with 4096 tokens and one slot; verify actual model-layer CUDA offload on the already confirmed RTX 4070 (8188 MiB VRAM, driver 577.02). Use the model's own current tool-capable chat template and recommended sampling (temperature 0.1, top_k 50, repeat penalty 1.1). Model/runtime deployment is independent of the application build; a successful CPU fallback is not a GPU test pass.

This small model is for integration testing; keep deterministic agent tests and measure tool selection/replay instead of assuming reasoning accuracy. The approved model is installed and CUDA-verified on Windows. Source: https://huggingface.co/LiquidAI/LFM3-1.7B-GGUF.

Candidates researched from primary documentation:

| Runtime | Windows / Linux deployment | Evaluation position |
| --- | --- | --- |
| llama.cpp `llama-server` | Native Windows executable and Linux executable; Linux server container images also available | Recommended first baseline: direct control over model files and runtime settings, matching the issue's proposal |
| Ollama | Native Windows background application/CLI and Linux daemon/service | Alternative when model installation/lifecycle convenience is the priority; pin runtime and model identity, validate API differences |
| Strata (`Niko1221/Strata`, assumed project intended by the user) | Documented Windows and Linux terminal/desktop installation and inference API | Candidate for its supported model/hardware combinations; verify unattended launch, tool-call behavior, resource use and disk requirements on both target deployments |
| LM Studio / llmster | Documented Windows and Linux headless daemon installation | Candidate for managed model lifecycle and desktop experimentation; evaluate headless operational setup and distribution/licensing requirements |
| vLLM | Official GPU installation requires Linux; Windows uses WSL or community alternatives | Exclude from the initial native Windows/Linux baseline; reconsider only if a Linux-only inference host becomes acceptable |

Use a small inference adapter in the combined application, configured by provider/base URL/model and required capabilities. An OpenAI-compatible chat endpoint is a useful common starting point, not a guarantee of identical behavior. Normalize streaming deltas, tool-call IDs/arguments, tool-result replay, usage and errors. Keep the model's reasoning output separate from tool execution. The inference server proposes calls; the application's shared tools layer authorizes and executes them.

Run the same task-oriented evaluation on Windows and Linux: health/model readiness, cold model load, first-token and full-turn latency, valid structured tool calls, a tool-result follow-up, repeated turns, cancellation, concurrent requests, restart recovery and idle behavior. Record GPU/CPU backend, model identity/checksum, chat template, quantization, context size, RAM/VRAM and disk footprint. Compare transport compatibility separately from model quality; different models or quantizations are not a controlled runtime-speed comparison.

For deployment, supply a Windows PowerShell launcher plus a background supervisor/service-wrapper or scheduled-task configuration, and a Linux launcher plus systemd unit or container configuration. Do not assume a console executable implements the Windows service-control protocol. Include logs, restart policy, readiness checks and graceful stop. Store model files outside source/build trees, reuse compatible files where supported and avoid duplicate large downloads. Do not enable an inference runtime's own agent/MCP execution features; retain the agreed tool and authorization boundaries in the combined application.

Research references: [llama-server](https://github.com/ggml-org/llama.cpp/blob/master/tools/server/README.md), [Ollama Windows](https://docs.ollama.com/windows), [Ollama Linux](https://docs.ollama.com/linux), [Strata](https://github.com/Niko1221/Strata), [LM Studio headless](https://lmstudio.ai/docs/developer/core/headless), [vLLM platform requirements](https://docs.vllm.ai/en/latest/getting_started/installation/gpu/).

## GitHub and staged commits

Verified 2026-11-06: the existing fork remote is `https://github.com/alexhock/OreStudio.git`; upstream remains `https://github.com/OreStudio/OreStudio.git`. The current local branch is `feature/local-ai-assistant`, its push destination is `fork`, and the working tree is clean. `git push --dry-run fork HEAD:refs/heads/feature/local-ai-assistant` succeeded and would create the remote branch. No remote branch, commit or push was created by that check.

Use one feature branch with reviewable stage commits. For each stage, implement the smallest runnable increment, run its relevant tests and smoke check, include reproducible build/run instructions, inspect the diff for accidental files/secrets, then commit and push the finished checkpoint to the fork. The first actual push can use `git push -u fork feature/local-ai-assistant`; subsequent pushes continue to that fork branch. Follow the repository's commit and agile traceability conventions. A later stage builds on the committed working checkpoint.

Stage 1 should bring the agreed architecture and staged implementation plan into the versioned component documentation; this current planning file is outside the repository and is not yet committed. Keep credentials, model weights, logs, generated build/deployment files and installed dependencies out of Git. Source, lockfiles, tests, deployment scripts and configuration examples belong in the stage commits. Component CI should exercise its independent build on Windows/Linux without invoking OreStudio's C++ build. Do not open a PR unless the user explicitly asks in that turn.

## Build and deployment separation

Requirement confirmed 2026-11-06: building and deploying the AI components are distinct operations, and neither operation owns the existing OreStudio deployment. There are three independently controlled deployment units:

| Unit | Build/release input | Deployment responsibility |
| --- | --- | --- |
| Existing OreStudio | Its existing native/web build and packages | Existing services, database and NATS broker; unchanged by AI deployment commands |
| Combined agent/MCP application | Its own TypeScript build and versioned production package/image | One agent/MCP process with its own configuration, service/task, logs, health checks and rollback |
| Inference server | Pinned upstream runtime binary/image, or a separately built runtime artifact | Its own process/service, model store, configuration, logs, health checks and rollback |

- Build commands produce and test artifacts only. They must not install/restart services, deploy files to a live directory, download models automatically or alter OreStudio configuration/data. The agent build must not invoke the main CMake build or require a main-server source checkout.
- Deployment commands consume an already built, versioned artifact and explicit environment configuration. They must not compile source or run an unpinned dependency installation in the live deployment. Include required production dependencies and declare/pin the Node runtime. Package/test Windows and Linux artifacts appropriately where native dependencies or platform launchers differ.
- Keep deployment directories outside source/build trees and outside the existing OreStudio installation. Use configurable agent-release, inference-runtime, config, logs, state and model directories. Model weights live in a persistent store separate from both application releases, so changing agent code does not copy/download weights again. Select inactive release directories before switching/restarting the relevant AI service; retain a known-good release for rollback.
- Give the combined application and inference distinct service/task/process names and ports. Provide independent install/update/start/stop/status/uninstall commands on Windows and Linux. A convenience AI start/stop command may orchestrate these two components only; it must not stop/start the main fleet, broker, PostgreSQL or unrelated deployments. Do not register them as children of the existing Windows fleet supervisor or make them mandatory services in the main application's service registry.
- Supply dedicated AI configuration and least-privilege credentials. Configure the existing broker address/subject namespace and the inference endpoint explicitly. Do not copy or rewrite OreStudio's `.env`, signing keys, service credentials, database or NATS server configuration as a deployment side effect. Any required broker permission provisioning is a separate explicit administration step, not an implicit startup mutation.
- Windows deployment uses native processes with independent supervisors/service wrappers or scheduled tasks; Linux uses independent systemd units or independently managed containers. Containers are optional, and native Windows operation must not require WSL or a second backend stack. AI components can run beside OreStudio or on separate hosts without changing the business integration contract.
- Independent operation does not remove runtime dependencies: tools need the configured OreStudio services/NATS, and chat inference needs the selected model server. Report unavailable dependencies and enforce deadlines/reconnect limits instead of changing or recreating the backend. External MCP read tools should remain usable when inference is stopped if their own dependencies are healthy. Apply resource limits when sharing a host.
- Acceptance: build without starting any deployment; install/start/update/rollback/stop each AI unit independently; verify the existing OreStudio fleet remains available and its service processes/configuration are unaffected; verify agent changes need no model reinstall, inference rebuild or OreStudio rebuild/restart. These deployment commands and artifacts are planned, not implemented yet.

## Compatibility and independent release rules

- Maintain two explicit contracts: the MCP tool schemas seen by agents, and the OreStudio NATS subjects/message schemas consumed by the adapter. An MCP protocol revision does not version an individual tool's business semantics.
- Start with a deliberately small tool catalogue. For example, one `read_fx_spot` tool maps an agent-friendly input to a backend lookup and maps its output to a stable value/date/source shape. Backend field renames should require changing that adapter, not the agent or tool schema.
- Keep tool changes backward compatible where possible; version a tool or endpoint when inputs, outputs or behavior must break. Record supported backend revisions/capabilities and fail clearly on unsupported ones. Never reinterpret malformed or missing backend data as a valid answer.
- Test adapter fixtures offline and run a small read-only contract suite against a pinned known-good backend and a candidate upstream version. Do not automatically upgrade the backend and agent together. Keep last-known-good deployable artifacts for each service and upgrade them independently.
- Make unsupported tools unavailable, or return an explicit capability error. Add request IDs and separate timing spans for agent/MCP entry, shared tool execution, authorization, NATS calls and inference. Set timeouts, cancellation and concurrency limits so a failed dependency does not monopolize the application. Test both entry paths against identical valid/invalid requests and allowed/denied identities to prevent an internal-agent authorization bypass.
- Separation limits rebuilds, releases and failures, but cannot guarantee compatibility with arbitrary breaking backend changes. Missing backend capabilities may still need normal domain API work; agent-specific prompts, tools and MCP logic should remain outside the server.

## Hosting and latency proof

Start with the combined agent/MCP application next to OreStudio, using persistent authenticated NATS connections. The combined application or llama-server can move to a separate host later. Configure the broker URL, subject namespace and TLS identity independently of MCP and inference URLs; no localhost assumptions in application logic. Keep broker access on the trusted network and give external clients access to the MCP endpoint rather than the broker. Connect remote clients using MCP Streamable HTTP; stdio is useful for local subprocess clients but is not the remote-hosting transport.

For one built-in agent tool call, the network path is application -> NATS broker -> OreStudio service: two one-way hops, four network legs for request and reply. The shared tool layer and NATS adapter add no network hops inside the process. An external MCP client adds its HTTP connection to the application: three one-way hops, six request/reply legs. These counts exclude the UI connection and separate inference requests; one user turn can involve multiple tool calls.

Do not choose C++ or embed MCP in the server on an unmeasured latency assumption. The added process/network boundary may be small compared with inference, but this has not been measured here. Backend execution time and the number of sequential tool calls also matter; keep discovery out of the per-turn hot path and avoid unnecessary repeated lookups.

First spike: benchmark the same authenticated NATS read through (1) the adapter alone, (2) the shared internal tool execution path, and (3) external MCP plus that same shared execution path, with inference disabled. Measure warm and cold paths, realistic payloads, 1/10/50 concurrent clients, failures, reconnects and cancellation. Report p50/p95/p99, errors and resource use for side-by-side and LAN deployment. Then measure end-to-end model/tool-loop latency separately.

A provisional engineering target is no more than 20 ms added p95 latency per small read for external MCP access in side-by-side deployment, relative to the direct NATS adapter baseline and excluding inference. Measure internal tool-layer overhead separately. These are acceptance measurements, not achieved results or promises for all workloads. Set a separate remote-host budget from measured RTT. If the target is missed, profile adapter calls, serialization, authentication and connection reuse before considering another language or moving code into OreStudio.

## Work breakdown and acceptance

1. **Protocol and security spike.** Select and pin the SDK/protocol pair; exercise a minimal client/server exchange and record required methods, metadata, error handling and transport behavior. The current official SDK documents v2 `@modelcontextprotocol/client` and the 2026-07-28 protocol; the issue names the older `@modelcontextprotocol/sdk`. Decide explicitly whether legacy external clients need compatibility. Do not copy an older initialize/session design without checking the selected revision.
2. **Small CUDA inference baseline.** GPU visibility is verified; check runtime compatibility with NVIDIA driver 577.02, then independently deploy a pinned CUDA `llama-server` with the selected official LFM3-1.7B QAD Q4_0 GGUF (approximately 1.05 GB, explicitly approved). Pin the model revision/hash, verify full model-layer GPU offload, and test health, a plain response, a structured tool call and a follow-up response containing a tool result. Start with 4096-token context and one slot; record latency and actual RAM/VRAM/disk use. Verify Windows and NVIDIA Linux deployments; a silent CPU fallback is a failed GPU acceptance check. Runtime/model installation remains a separate deployment operation from component builds.
3. **Combined application skeleton and first shared tool.** Build one standalone TypeScript application with agent, MCP, shared tools/auth and NATS-adapter modules. Expose one read-only tool through external MCP and invoke the exact same execution layer directly from an internal test harness. Prove caller authorization and restricted subject permissions on both paths; measure direct-NATS, internal-tool and external-MCP latency. Verify backend revision/capability compatibility, SDK interoperability, malformed JSON-RPC, unknown methods/tools, input limits, cancellation, reconnects and streaming. No MCP route or HTTP backend adapter is added to OreStudio.
4. **Real backend tool.** Resolve the supported FX/IR market-data query; return value, instrument, observation time/date and provenance. Check missing/stale data, ambiguous instruments, invalid arguments, pagination/size limits and permission failures. Add a resource only when it has a concrete consumer; do not expose arbitrary SQL or filesystem access.
5. **Agent loop within the combined application.** Add the inference adapter, directly invoke shared tool execution, and enforce cancellation, deadlines, maximum turns and output limits. Do not add loopback HTTP/MCP calls between the built-in agent and its own tools. Test no-tool answers, successful calls, malformed or unauthorized calls, repeated calls, inference failure and tool failure. Use deterministic fake inference in automated tests; keep real-model evaluation separate. Prompt or tool-loop changes require only an agent/MCP application deployment, with no OreStudio rebuild or restart.
6. **User interaction and independent deployment.** Start with a separate assistant UI/harness, including loading/cancel/error behavior. Implement separate build/package and deployment/lifecycle commands as specified above: one release/service for the combined application and an independently deployed inference runtime/model store. Provide an external MCP-client example. Verify OreStudio availability when either AI unit is installed, updated, stopped or restarted, and verify recovery of agent/MCP access. Validate native Windows and Linux deployment, side-by-side and LAN latency, compatibility tests and independent rollback. Keep large model files and generated credentials outside version control. Integrate a stable agent API into the existing OreStudio UI only as a later step.
7. **Follow-up use cases.** Add reporting/results reads when their backend flows are stable. Then extract a proposed trade into the existing booking screen for explicit user review and submission. Do not book from model output directly. Two-way pricing, unsupported volatility queries and business workflows remain separate work, consistent with the issue comments.

## Authorization decisions to settle in the spike

- Bind local inference and development MCP to loopback; validate allowed origins and enforce authentication. Keep inference and MCP URLs in operator configuration, never in a user-supplied URL field.
- Enforce tenant, party and permission checks on every tool invocation, including discovery visibility. Test two users/tenants and expired credentials. Never accept tenant identity from the model as authority.
- Reconcile existing IAM login/JWT behavior with MCP's HTTP authorization/discovery requirements. Validate tokens for their intended resource; do not blindly forward a token issued for the combined application's MCP endpoint to OreStudio. Normalize internal-agent sessions and external MCP identities into trusted execution contexts, then use supported identity delegation/token exchange or a deliberate backend credential mapping while retaining the user's tenant/party permissions. No unrestricted shared administrator account is permitted. An internal authenticated prototype does not establish external-client OAuth interoperability.
- A client name or user-agent string cannot prove a harness is sanctioned. Approved client enrollment, scopes/audience and credential issuance/revocation need a defined policy; credentials copied to another harness remain usable unless stronger controls are introduced.
- Treat prompt text and tool results as untrusted data. Log correlation IDs, tool names and outcomes; redact tokens and sensitive trade/account content. Future writes require explicit confirmation and idempotency protection.

## Ready now / remaining setup

- Done: feature branch updated to merged upstream main and moved into the original checkout; fork push destination retained; duplicate feature worktree removed; completed audit compilation output removed; original native build and local runtime data preserved.
- Installed host tools: Node 24.16.0, Python 3.13.16, CMake 4.4.4, Git for Windows. Existing native toolchain is described in `installation-status.md`; a new C++ build was not attempted.
- Compass orientation ran through the Python entry point. The shell provisioning step has a Windows path issue; direct skills/settings generation then found Emacs missing from PATH. No generated Claude configuration is claimed ready. This does not prevent editing or TypeScript work.
- Web dependencies and build verification: see the validation update below.
- Base runtime is now ready. Remaining feature work begins with protocol/authentication selection and local model hardware selection/inference installation, followed by the first read-only MCP tool and TypeScript orchestration. Neither a temporary integration branch nor another full build tree is required.

When implementation begins, scaffold the story/tasks with `compass add`, link issue #4295 and the existing deferred MCP capture, and record the chosen protocol and first tool. This local plan does not create or start sprint tasks.

## Validation update

The web build results below are from the initial 2026-11-04 baseline. The 2026-11-05 fast-forward was verified for clean Git state and Windows merge ancestry; no new C++ or web build was run. The root web package manifest and lockfile are unchanged across that update.

- `npm ci --ignore-scripts --no-audit --no-fund` installed 208 locked web packages. The restricted run failed inside npm; the network-enabled retry succeeded. No dependency manifests or lockfiles changed.
- `npm run build` passed for all web workspaces, including BFF, contracts, org, browser and wire-protocol. Vite reported bundle-size and ineffective dynamic-import warnings; they did not fail the build.
- Feature checkout is clean, its HEAD equals the fetched upstream main, and its branch push destination is the fork. The `.env` is ignored and uses LF. Both existing worktrees remain on their original branches with clean working trees.
- No automated feature tests or C++ build were run: this turn created the workspace and plan, with no feature implementation. No runtime, database or inference smoke-test success is claimed.

## References

- Feature and scope discussion: https://github.com/OreStudio/OreStudio/issues/4295
- Merged Windows support: https://github.com/OreStudio/OreStudio/pull/4461
- SDK and version compatibility: https://github.com/modelcontextprotocol/typescript-sdk
- MCP transports: https://modelcontextprotocol.io/specification/2026-07-28/basic/transports
- MCP HTTP authorization: https://modelcontextprotocol.io/specification/2026-07-28/basic/authorization
- llama.cpp HTTP server: https://github.com/ggml-org/llama.cpp/blob/master/tools/server/README.md
- llama.cpp tool calling: https://github.com/ggml-org/llama.cpp/blob/master/docs/function-calling.md
- Local repository guidance: `doc/recipes/compass/how_do_i_provision_an_environment_with_compass.org`, `doc/recipes/git/how_do_i_start_work_on_a_story.org`, and `doc/agile/product_backlog/deferred/consider_adding_mcp_support.org`.
