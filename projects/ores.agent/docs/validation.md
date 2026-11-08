# Validation index and known limits

Last installed release checked: **0.1.9-fa435c800dd7**, 2026-11-08.
Code: `aa547d496a`. Later document-only commits do not change that artifact.
Check `/healthz` and `/readyz` for current process status; dated records below
are not a live health monitor.

## Latest check: Markdown chat

112 tests passed on Windows and pinned Linux. All three replay handling gates
passed. Release manifests matched. Edge checks passed at desktop/phone sizes:
headings, tables, lists, code, literal evidence, unsafe markup, errors, cancel
and sign-out. Installed authenticated NPV/Greek replies retained exact values.

Only the standalone agent restarted. Private config and all 19 inspected native,
broker and inference PIDs stayed unchanged. Main web/agent health returned 200.
No broad model/live evaluation rerun was performed for this UI update.
Private evidence: `build/output/agent-markdown-*`, `check-agent-markdown-*.mjs`
and `snapshot-agent-markdown-scope.ps1`.

## Current limits

- The agent reads data; it does not book, run reports or issue executable quotes.
- Live trade base-NPV checks still fail `invalid_result_units`; values are withheld.
- Positive live IR and vega remain unverified. Missing/sparse metrics are not zero.
- Scope totals are agent-derived, with bounded reads rather than an atomic server snapshot.
- Original fixtures can conceal native/base errors. The review corpus is separate.
- Local bearer authentication is not hosted OAuth enrollment. Restricted broker
  subjects, low-permission and cross-tenant coverage still need proof.
- Native Linux NVIDIA, local Node 22 and remote CI execution are not established
  by the recorded Linux Node tests. There is no OS boot or automatic restart policy.

Latest broad evaluation: [0.1.8 review](review-validation.md). Handling passed
42/42 original model, 34/36 live and 22/22 review/model cases. The datasets and
repetitions differ; these are not general model-accuracy scores.

## Historical releases

All test counts and installed IDs in this table belong to their dated checkpoint.
They are not today's test count or service status.

| Release/checkpoint | Source | Installed artifact | Tests per Windows/Linux | Record |
| --- | --- | --- | ---: | --- |
| 0.1.8 typed requests | `055af005e05a` | `0.1.8-c1371bb8918e` | 107 | [Review](review-validation.md) |
| 0.1.7 comparison/timing | `5004b5e7c8e3` | `0.1.7-0a1d2f580ce4` | 101 | [Stage 6](stage6-validation.md) |
| 0.1.6 intent | `17cb7bc4a07c` | `0.1.6-4317a6cd1870` | 94 | [Stage 5](stage5-validation.md) |
| Stage 4 evaluator | `ebb7fe7849d2` | Kept 0.1.5 | 85 | [Baseline](evaluation-baseline.md) |
| 0.1.5 Greeks | `753a54b4550b` | `0.1.5-cf24e13db443` | 77 | [Greek data](greek-validation.md) |
| 0.1.4 scope NPV | `558452611601` | `0.1.4-754c262be033` | 68 | Scope check below |
| 0.1.3 trade values | `bf08fba92c6c` | `0.1.3-5b23d543cb79` | 48 | [NPV data](valuation-validation.md) |
| 0.1.2 discovery | `26217842d462` | `0.1.2-adeb734cfeb9` | 37 | Discovery check below |
| 0.1.1 market points | `7cc8511292a2` | `0.1.1-86a407144a11` | 24 | Market check below |
| 0.1.0 initial runtime | `88fe926d56fa` | `0.1.0-512b6069701e` | 12 | Initial checks below |

Pinned Linux test image:
`node@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9`.
It tests Node application behavior, not native CUDA execution.

## Historical scope check

Run `884d0bc9-932d-4c4c-9b66-ee97d2b810bb`, priced 2026-11-06.
Membership time: `2026-11-06 21:42:03.608662Z`. Independent Python `Decimal`
matched all contributors and totals from NATS/CSV metadata. Explicit per-row
`BaseCurrency` supplied GBP units; current book currency was not inferred.

| Scope | Expected/valued trades | Base NPV, GBP | MCP reconciliation sample |
| --- | ---: | ---: | ---: |
| Group FX Hedges | 63/63 | 206318954.123122 | 6097 ms |
| Group Intercompany Loans | 10/10 | 50969977.080832 | 2017 ms |
| Group Treasury Portfolio | 73/73 | 257288931.203954 | 6062 ms |

The reconciliation times are smoke samples, not benchmarks. Both books reconciled
to the parent. Native FX-book NPV was withheld for mixed GBP/USD. Partial runs,
foreign-party access and a different requested currency supplied no total.
0.1.4 did not implement Greeks; 0.1.5 later did, with its recorded limits.

The separate inference correction increased context from 4096 to 262144 and
output allowance from 512 to 2048. All 25 layers remained on CUDA. It tested a
7225-token prompt, not a full context window. See [inference](inference.md).

## Historical Greek check

510 metrics matched 255 real CSV rows. The 63-trade FX book had complete
`DiscountCurve/GBP/0/1Y` groups at target shift 0.0001:

| Raw factor | Target shift | Stored measure | Full-scope cash finite difference |
| --- | ---: | --- | ---: |
| DiscountCurve/GBP/0/1Y | 0.0001 | Delta | GBP -6502.44 |
| DiscountCurve/GBP/0/1Y | 0.0001 | Gamma | GBP 0.14 |

All 16 FX-book and 16 parent groups were inspected with independent MCP checks.
Sparse groups and missing loan-book rows had no full total. Live vega stayed
unavailable. Existing NPV, market, discovery and reporting regressions passed.
The operator created separate validation runs; the agent remained read-only.
See [failed attempts and preparation](greek-validation.md).

## Historical trade, discovery and market checks

0.1.3 verified explicit partial output and separate-party denial. A later clean
NPV check matched **GBP -7,998,696.427339** across NATS, MCP and GPU chat.
Its temporary scope-copy addition was later reverted. It is not required at
runtime. See the [preparation/removal record](valuation-validation.md).

0.1.2 verified two books, three portfolios and 73 Plc trades, including 63 FX
forwards. Book hierarchy and direct portfolio links stayed distinct. Counterparty
name search was per-page; bond terms were issue-reference/notional only.

0.1.1 verified historical EUR/USD 1.0552 from `ore`, observed 2016-12-30,
provider clarification and missing USD 5Y swap observations. Positive rate tests
used fixtures. Individual smoke timings were not benchmarks.

## Initial runtime checks

| Stage | Commit | Evidence |
| --- | --- | --- |
| Foundation | 7edd50b00a | Packaged release starts outside checkout |
| NATS/session/tools | ad58eefcf4 | Real tenant sign-in/party selection, currencies/series/observations |
| MCP | 6520bca6ed | Official external client discovers and calls the same tools |
| CUDA inference | da3599e08b | Pinned LFM3 QAD model, full 25/25-layer RTX 4070 offload |
| Agent/UI | a3cc72d40b | Real-data answers with evidence, bounded loop and private UI session |
| Deployment/lifecycle | 88fe926d56 | Windows/Linux immutable packages and authenticated independent lifecycle |

Initial manifest ID:
`512b6069701eaf88c6c446c4947289addea47da8e3998c97cef267f6067e09ab`.
Bundle SHA-256: `6bf5d361d914560d6fd29713c3b3b22db3c324dcbcebc0ae0b1c71a65914ca3d`.
Windows/Linux packages matched. Agent and inference stopped/restarted independently;
MCP reads worked with inference down, while chat returned 503. All 25 then-inspected
native/broker PIDs stayed unchanged. The main native build was not duplicated.

For current commands, use [deployment](deployment.md), [evaluation](evaluation.md)
and [README](../README.md). Historical process counts, timings and local disk
measurements are not current capacity or performance claims.
