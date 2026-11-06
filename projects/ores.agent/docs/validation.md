# Implementation validation

Checkpoints pushed to `alexhock/OreStudio`, branch `feature/local-ai-assistant`:

| Stage | Commit | Evidence |
| --- | --- | --- |
| Foundation | 7edd50b00a | Packaged release starts outside checkout |
| NATS/session/tools | ad58eefcf4 | Real tenant sign-in/party selection, currencies/series/observations |
| MCP | 6520bca6ed | Official external client discovers and calls the same tools |
| CUDA inference | da3599e08b | Pinned LFM3 QAD model, full 25/25-layer RTX 4070 offload |
| Agent/UI | a3cc72d40b | Real-data answers with evidence, bounded loop and private UI session |

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
