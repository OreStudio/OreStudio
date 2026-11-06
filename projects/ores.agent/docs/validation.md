# Implementation validation

Checkpoints pushed to `alexhock/OreStudio`, branch `feature/local-ai-assistant`:

| Stage | Commit | Evidence |
| --- | --- | --- |
| Foundation | 7edd50b00a | Packaged release starts outside checkout |
| NATS/session/tools | ad58eefcf4 | Real tenant sign-in/party selection, currencies/series/observations |
| MCP | 6520bca6ed | Official external client discovers and calls the same tools |
| CUDA inference | da3599e08b | Pinned LFM3 QAD model, full 25/25-layer RTX 4070 offload |
| Agent/UI | a3cc72d40b | Real-data answers with evidence, bounded loop and private UI session |
| Deployment/lifecycle | 88fe926d56 | Windows/Linux immutable packages and authenticated independent lifecycle |

## Running local release

Release source: `88fe926d56fa64653a70e4c009cab53eaa3cbda3`.
Manifest identity: `512b6069701eaf88c6c446c4947289addea47da8e3998c97cef267f6067e09ab`.
The exact committed source produced **the same artifact identity on Windows
and Linux**. Agent bundle SHA-256:
`6bf5d361d914560d6fd29713c3b3b22db3c324dcbcebc0ae0b1c71a65914ca3d`.

Local roots are `../agent-runtime/app` and `../agent-runtime/inference`, outside
the source checkout. UI: http://127.0.0.1:20100/; MCP: the same origin's `/mcp`;
inference: loopback port 20010. Both processes are running under independent
supervisors. The existing OreStudio app remains at http://127.0.0.1:20002/.

The installed release passed external MCP discovery/read and GPU-backed chat
again: real currency answer 1,921 ms, empty-observation answer 1,536 ms.
`scripts/smoke-isolation.mjs` stopped/restarted each AI component separately:
inference stayed healthy while the agent stopped; NATS tools remained available
while inference stopped; chat returned HTTP 503 during inference downtime.
Both components were restored. A PowerShell before/after comparison confirmed
all 25 native backend/broker process IDs were unchanged, and the main web app
continued returning HTTP 200. No backend rebuild, restart or data mutation was
part of these checks.

After verified installation, 680,171,207 bytes of temporary CUDA ZIP downloads
were removed. The independent local runtime (model, native libraries, app,
private config and logs) occupies approximately 2.15 GiB. The model is stored
once and is shared across inference runtime releases. The main CMake tree was
neither duplicated nor rebuilt.

Windows and Linux component verification: 12 tests pass, including immutable
package installation, independently authenticated lifecycle control, two-user
identity isolation, revoked credentials, denied/malformed backend replies,
gzip/output bounds, MCP SDK interoperability, inference timeouts and bounded
agent calls. Linux additionally builds and packages in a disposable container:
`node@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9`.
The manual CI matrix is supplied but no remote GitHub Actions run is claimed.

Live GPU/backend smoke: official LFM3 QAD Q4_0 on native Windows, selected
Acme tenant/party; currency answer 2,237 ms, empty-observation answer 1,546 ms.
The model generated a real tool call in each case and consumed its result.
These are smoke samples, not benchmarks. See inference.md for VRAM placement
and the exact model/runtime lock. No main-server compilation was required.

Remaining deployment limits: Linux NVIDIA execution needs a hardware host;
the local broker has mTLS but no certificate-specific subject ACLs; external
MCP authentication uses explicit short-lived bearer headers, not OAuth client
enrollment. Automatic OS boot registration, conversation persistence, trading
writes/two-way pricing and broad model accuracy evaluation are outside this
first local read-only deliverable. The sampled market series has no seeded
observations, so no successful live FX-price lookup is claimed.
