# Implementation validation

## Stage 4: evaluation tooling and initial baseline

Implementation `ebb7fe7849d2089da0444f250bbe32edddfbf405` passes all 85 tests
on Windows and Linux. The 26-case corpus runs as replay, local-model/fixtures
and opt-in existing live reads. Handling checks passed 26/26, 14/21 and 13/18
respectively. Failed checks, missing-data/capability outcomes and a known
booking-refusal oracle false negative remain visible. See the
[complete baseline evidence](evaluation-baseline.md) and [runner usage](evaluation.md).
This tooling changes no installed application or backend deployment; the
0.1.5 release and its historical validation below remain unchanged.

## Current release: 0.1.5 stored raw Greek buckets

Validated code: `753a54b4550b9b73a602fc19a7b42b9393e773ad`. Installed artifact:
`0.1.5-cf24e13db443`, identical Windows/Linux manifests. All **77 tests pass**
on both platforms, using the same pinned Node Linux image below. Evidence is
`build/output/agent-greeks-windows-check.log` and `agent-greeks-linux-check.log`.
No native Linux NVIDIA execution or remote CI is claimed.

The clean historical run `134b921d-eb6a-4277-964f-13d4b33429f5` priced at
2026-11-06 and records zero errors/one warning. A separate Python `Decimal`
reference verified **all 510 stored Delta/Gamma figures against all 255 actual
engine sensitivity.csv rows**, including source line, trade, measure, currency,
factor and target shifts, then independently reconstructed historical scopes.
No agent arithmetic is imported by that reference. See
[operator setup, failed attempts and execution limitations](greek-validation.md).

`scripts/smoke-greeks.mjs` passed on the installed release with the independent
official MCP client. It enumerated all 16 FX-book buckets and all 16 parent
portfolio buckets, then reconciled every paged contributor against that
CSV-backed reference. The FX book has two complete 63-trade buckets:

| Raw factor | Target shift | Stored measure | Full-scope cash finite difference |
| --- | ---: | --- | ---: |
| DiscountCurve/GBP/0/1Y | 0.0001 | Delta | GBP -6502.44 |
| DiscountCurve/GBP/0/1Y | 0.0001 | Gamma | GBP 0.14 |

Other buckets have sparse coverage and therefore no full-scope total. The
loans book has no stored sensitivity rows; its missing contributions are not
filled with zero to produce a parent total. Vega has no live volatility rows
in this dataset and correctly reports unavailable. Positive volatility-Delta
selection and cross-gamma behavior have fixture coverage, not a claimed live
option/vega demonstration. Raw/par, factor/tenor, exact decimal shift, scenario,
currency and duplicate/missing-member guards pass meaningful fixture tests.

Actual GPU chat returned the verified Delta, Gamma and signed 1bp rate Delta
(DV01 selector) in 5794, 10017 and 7657 ms respectively. Run clarification,
same-session UUID continuation, cross-party denial, differently requested
currency withholding and errored-run withholding passed. The comprehensive
MCP contributor reconciliations took 85488 ms for the book and 84690 ms for
the parent across many separate bounded calls; these are full smoke durations,
not per-question latency. Final evidence is
`build/output/agent-greeks-final-smoke.log`; the preceding full pass remains
in `agent-greeks-smoke.log`. The final guard also prevents model-supplied empty
selectors from silently dropping stress or cross-gamma buckets.

Existing NPV scope, market, discovery and trade-reporting regressions passed
during rollout; logs are `build/output/agent-greeks-npv-regression.log` and
`agent-greeks-{market,discovery,reporting}-regression.log`. The new clean run
also contains NPV output, so the trade-reporting smoke now verifies default
multi-run ambiguity and exact clean-run selection instead of assuming only
one clean run exists. No product behavior was changed to hide that ambiguity.

Only the standalone agent release changed. The 19 inspected native service,
broker and inference PIDs and private agent config hash remained unchanged
across its switch. Main app, agent health and readiness returned HTTP 200.
No new subject, native rebuild, temporary reporting instance, broker change,
database reset or queue purge was ultimately needed. Operator validation did
create fresh unscheduled definitions/runs and prepared input artifacts through
existing authenticated NATS APIs; the agent itself remains read-only.

Available raw Greek reads complete the declared read-only Stage 3 scope with
these data/coverage limits. Per-unit Greeks, inferred par-risk conversion,
zero filling and automatic report execution are not implemented. The normal
packager's missing simulation/sensitivity export support and the existing
prepared-input pending-to-running bug remain **execution** prerequisites;
operator setup explicitly started fresh runs with version checks, never
relabelled failed/errored runs. Hosted auth, broker hardening, role/cross-tenant
proof and native Linux NVIDIA checks remain separate gates.

## Previous release: 0.1.4 book/portfolio NPV

Validated code: `558452611601852436ccb3820efc1b10e6b8d7a0` (NPV implementation
begins at `fbded53408`). Installed artifact: `0.1.4-754c262be033`, identical on
Windows and Linux. All **68 tests pass** on both platforms. Local evidence is
in `build/output/agent-scope-windows-check.log` and
`build/output/agent-scope-linux-check.log`; Linux used the pinned Node image
`node@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9`.
No Linux GPU execution or remote CI is claimed.

The read-only `scripts/smoke-scope-reporting.mjs` passed with the official
independent MCP client. Every contributor across all pages and each full-scope
total matched a separate Python `Decimal` reference from historical NATS rows
and typed CSV metrics. The agent's arithmetic is not imported by that reference:

| Scope | Expected/valued trades | Base NPV, GBP | MCP reconciliation sample |
| --- | ---: | ---: | ---: |
| Group FX Hedges | 63/63 | 206318954.123122 | 6097 ms |
| Group Intercompany Loans | 10/10 | 50969977.080832 | 2017 ms |
| Group Treasury Portfolio | 73/73 | 257288931.203954 | 6062 ms |

Run `884d0bc9-932d-4c4c-9b66-ee97d2b810bb` priced at 2026-11-06. Booking
membership uses its exact start `2026-11-06 21:42:03.608662Z`, preserving all
microseconds. The canonical source is `npv.csv`; explicit GBP base units were
joined from each matching typed metric's `BaseCurrency`, not inferred from
current book configuration. Both books reconcile to their parent portfolio.

Actual LFM3 GPU book and portfolio questions passed (4924 ms and 5030 ms
samples). Run clarification and same-session UUID continuation passed. A
second party could not access this run; a partial run supplied no aggregate;
a requested different currency supplied no filtered partial total. Both MCP
and GPU chat returned unsupported for Greek requests without substituting NPV.
Samples are smoke timings, not benchmarks. See [scope semantics](scope-reporting.md).
The previously intermittent book and portfolio requests then passed three
additional repetitions each without a generation-budget failure (4870–5903 ms
samples), recorded in `build/output/agent-scope-chat-repeat.log`. This is a
bounded regression check, not a guarantee that arbitrary requests cannot reach
the configured output limit.

The existing market, discovery and stored trade-reporting smoke suites also
passed on the installed release. They cover provider and book clarification,
historical FX/no-observation behavior, trade economics, clean default NPV,
explicit partial inspection, run status and second-party isolation. Logs are
`build/output/agent-scope-{market,discovery,reporting}-regression.log`.

The original 512-token generated-output allowance intermittently exhausted
before a complete call. It now defaults to 2048, optionally configured by
`inference.maxOutputTokens`; truncated replies still fail closed. The launcher
was also corrected from 4096 to 262144 context tokens. Only llama.cpp was
restarted for that separate correction: 25/25 layers remain on CUDA, total
GPU usage was 4548 MiB, and a 7225-token prompt completed successfully.
This does not prove quality/latency at a fully occupied 256K context.

Only the standalone agent was switched during the application rollout. The
19 inspected native service/broker/inference PIDs and private agent config hash
were unchanged across that switch; the main app, agent health and readiness
returned HTTP 200. This release adds two existing read subjects to the agent
allowlist; no new C++ handler, native build, broker ACL/configuration, database
reset, queue purge or report execution was required.

Stage 3's NPV slice is delivered. **Greek aggregation remains open** pending
verified units, bump conventions and factor/scenario dimensions. Hosted auth,
restricted broker ACLs, role/cross-tenant proof and native Linux NVIDIA checks
remain separate open gates. No command/write business use case is enabled.

## Previous release: 0.1.3 stored reporting

Validated code: `bf08fba92c6cfe2da67ec172f6c207272501b4ff` (implementation begins
at `5ff7160a7b`). Installed artifact: `0.1.3-5b23d543cb79`, identical on Windows
and Linux. All **48 tests pass** on both platforms. Linux output is retained
locally in `build/output/agent-reporting-linux-check.log`; the pinned container
is the same image used below. No Linux GPU or remote CI run is claimed.

The installed release passed `scripts/smoke-reporting.mjs` with the official
independent MCP client and actual Acme data. Run discovery and health succeeded;
default NPV requests withheld the errored runs' figures; explicit run-scoped
partial inspection returned a labelled `npv.csv` row priced at 2026-11-06.
A separate login selecting Acme HK could not read the Plc trade's results.
No report was generated, retried or repaired during these checks.

GPU-backed NPV and run-status questions passed (3,671 ms and 1,279 ms samples).
The exact explicit two-UUID partial command uses validated direct execution,
not a model round trip (1,071 ms sample). This distinction is deliberate and
recorded in reporting.md. All numbers are smoke samples, not benchmarks.
Discovery/book clarification/trade economics and market/provider/currency
regressions passed. Native backend, NATS and inference PID sets were unchanged;
the main app remained HTTP 200. Only the standalone agent release changed.

The separately declared live NPV gate **passed on 2026-11-07**. An isolated
NPV-only report completed clean for 73 actual trades across two books. Run
`884d0bc9-932d-4c4c-9b66-ee97d2b810bb` gives the example trade's NPV of
GBP -7,998,696.427339, priced at 2026-11-06, source `npv.csv`. The new clean
smoke reconciles direct authenticated NATS, independent MCP and GPU-backed chat
(5,327 ms sample); the updated existing reporting smoke passes clean-default,
partial-output and second-party regressions.

Preparing that dataset required a declared backend scope-copy addition, a
focused Windows reporting DLL build and a temporary validation instance. The
temporary instance was stopped before the corrected run; the main fleet was
not restarted or replaced. No broker configuration/ACL changes were required.
The new backend subject is not permanently deployed in the original fleet;
native Linux validation of that addition remains open. See the complete
[setup, failed first attempt, API observations and evidence](valuation-validation.md).
The installed agent artifact and its read contracts remain unchanged.

## Previous release: 0.1.2 discovery

Validated code: `26217842d462e4f5b193b55d4e02ce366adb728d` (Stage 1 implementation
starts at `065e5dc857`). Installed artifact: `0.1.2-adeb734cfeb9`, identical on
Windows and Linux. All **37 tests pass** on both platforms; the pinned Linux
image and local `build/output/agent-discovery-linux-check.log` reproduce the
cross-platform check. No Linux GPU or remote CI execution is claimed.

The installed release passed `scripts/smoke-discovery.mjs` using an independent
MCP client, actual selected-party data and CUDA LFM3 chat. It verified book,
portfolio and counterparty discovery; 63 trades in Group FX Hedges; precise
FX-forward economics and booking hierarchy; ambiguous-book clarification; and
an actual separate login selecting Acme HK that could not read the Plc trade.
This is second-party coverage, not a claim of least-privilege/cross-tenant proof.
Final sample times: books 1,009 ms; ambiguous book 903 ms; exact clarification
328 ms; trade detail 697 ms; counterparty code 484 ms. These are smoke samples.

FX/IR/provider/currency regressions also passed during rollout. Native backend,
NATS and inference PID sets remained unchanged across agent deployments, and
the main app returned HTTP 200. Only the agent release changed; no server source,
CMake build, broker configuration or database migration was required. Existing
services had been restored during Stage 0; GPU inference was restored for model
validation. Historical market-data and clean-NPV data gaps remain unchanged.

See [discovery](discovery.md) for scope, limits and examples. Counterparty name
search is explicitly per-page; Bond economics are issue-reference/notional only.
Stage 2 reporting is not implemented by this discovery release.

## Previous release: 0.1.1 market lookups

Committed/pushed source: `7cc8511292a2416da54c25fe8bd54530326b37c7`.
Installed release: `../agent-runtime/app/releases/0.1.1-86a407144a11`.
Windows and Linux produced the same artifact identity prefix `86a407144a11`.
All **24 tests pass** on both platforms; Linux output is retained locally in
`build/output/agent-market-linux-check.log`. The checks use the same pinned
Linux Node container recorded below. No remote CI or Linux GPU execution is
claimed.

The deployed application passed `scripts/smoke-market.mjs` using the official
external MCP client and actual CUDA-backed chat:

| Request | Result | Sample application time |
| --- | --- | --- |
| EURUSD spot, no provider | Asks ore vs bloomberg | 1,012 ms |
| Provider clarification: ore | Historical/stale 1.0552, observed 2016-12-30 | 45 ms |
| EURUSD spot from ore | Same dated result with stale warning | 723 ms |
| EURUSD from ore on 2016-12-30 | Historical as-of result | 1,782 ms |
| USD 5Y swap from bloomberg | No matching observation; no rate supplied | 1,792 ms |

External MCP historical FX and the existing currency tool-loop regression also
passed. These timings are individual smoke samples, not benchmarks. Successful
populated IR cases use labelled fixtures; live-populated IR is not claimed.
The market facts are rendered from validated tool output, not rewritten by the
model. See [market lookups](market-lookups.md) for boundaries and examples.

Only the standalone agent process was restarted. Before/after process-ID sets
for the native backend, NATS and llama.cpp matched exactly; the main web app
continued returning HTTP 200. No OreStudio server source/build, NATS subjects,
broker configuration or database changes were needed. The reusable local
lifecycle CLI now exits gracefully to avoid the native Windows Node handle
shutdown crash observed during tests.

## Initial 0.1.0 validation history

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
observations; that initial smoke did not demonstrate a populated FX lookup. Version 0.1.1 now verifies imported historical EURUSD observations as recorded above.
