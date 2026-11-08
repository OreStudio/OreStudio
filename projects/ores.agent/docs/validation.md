# Validation and known gaps

Last installed release checked: **0.1.9-fa435c800dd7**, 2026-11-08.
Code: `aa547d496a`. Later document-only commits do not change that artifact.
Use `/healthz` and `/readyz` for current status; these are dated results.

## Latest release

112 tests passed on Windows and pinned Linux. All three replay handling suites
passed and release manifests matched. Desktop/phone Edge checks covered Markdown,
unsafe markup, literal evidence, errors, cancel and sign-out. Installed NPV/Greek
replies retained exact values. This UI release did not rerun broad model/live evaluations.

Only the agent restarted. Private config and all 19 inspected native/broker/inference
PIDs stayed unchanged. Main/agent health returned 200. Evidence is in ignored
`build/output/agent-markdown-*` and `check-agent-markdown-*.mjs` files.

## Known gaps

- No booking, report execution or executable quote tools.
- Live trade base-NPV fails `invalid_result_units`; the value stays withheld.
- Positive live IR/vega is unverified. Missing/sparse metrics are not zero.
- Scope totals use bounded agent reads, not an atomic server summary.
- Original fixtures can conceal native/base errors; review fixtures are separate.
- Hosted authentication, restricted broker subjects, low-permission and cross-tenant
  proof remain open. There is no OS boot or automatic restart policy.
- Native Linux NVIDIA and local Node 22 execution remain unverified.

Latest broad handling scores: original model 42/42, live 34/36, review/model 22/22.
These use different selections and include deterministic agent paths. They are
not general model-accuracy scores.

## Historical evidence

Keep these reviewed JSON records unchanged. They retain scores, failures and
source/model identifiers; complete private reports remain outside Git.

| Checkpoint | Tests per Windows/Linux | Evidence |
| --- | ---: | --- |
| Stage 4 initial baseline | 85 | [Aggregate](../evals/baselines/stage4-2026-11-07.json) |
| Stage 5 / 0.1.6 | 94 | [Aggregate](../evals/baselines/stage5-2026-11-07.json) |
| Stage 6 / 0.1.7 | 101 | [Aggregate](../evals/baselines/stage6-2026-11-07.json) |
| Review / 0.1.8 | 107 | [Aggregate](../evals/baselines/review-2026-11-08.json) |

Detailed Markdown reports and earlier release checks are retained in the
[frozen documentation snapshot](https://github.com/alexhock/OreStudio/tree/f2dd9f89c88c45f270cf42cea43e99cc09cf5bc1/projects/ores.agent/docs).
The current tree keeps only the five guides and README.

Reference records:

- [NPV preparation](https://github.com/alexhock/OreStudio/blob/f2dd9f89c88c45f270cf42cea43e99cc09cf5bc1/projects/ores.agent/docs/valuation-validation.md): run
  `884d0bc9-932d-4c4c-9b66-ee97d2b810bb`, priced 2026-11-06. FX-book base NPV
  GBP 206318954.123122; parent GBP 257288931.203954. The temporary scope-copy
  addition was reverted. Existing results remain; no current tool requires it.
- [Greek preparation](https://github.com/alexhock/OreStudio/blob/f2dd9f89c88c45f270cf42cea43e99cc09cf5bc1/projects/ores.agent/docs/greek-validation.md): run
  `134b921d-eb6a-4277-964f-13d4b33429f5`. 510 metrics matched 255 engine CSV rows.
  The 63-trade FX book had GBP 1Y Delta -6502.44 and Gamma 0.14 at shift 0.0001.
  Other groups were sparse; no live vega was established.

Historical operator actions are not setup instructions or approval to repeat writes.
Windows CUDA verified all 25 layers and context 262144 on the RTX 4070 Laptop GPU.
A 7225-token prompt passed; a full 256K prompt was not tested. Linux Node tests
used `node@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9`.
They did not test native Linux CUDA. Smoke timings are not benchmarks.

## CPU evaluation CI

The separate [CPU workflow run](https://github.com/alexhock/OreStudio/actions/runs/42495394844)
uses Node 24, llama.cpp b12172 and the pinned LFM3 QAD Q4_0 model. Context is
262144 with four CPU threads and no GPU offload. It uses fixture data, not NATS.

114 agent tests, five installer tests and replay checks pass locally.
The laptop model run passed 40/40 handling checks; six business cases remained
blocked. It generated 4,256 tokens at 26.7 tokens/second. Linux CI also passed
40/40 handling checks, generating 4,053 tokens at 33.5 tokens/second.
Windows CI passed 40/40 handling checks, generating 4,270 tokens at 22.2
tokens/second. Each platform retained six expected blocked business cases in total.
Both jobs stopped inference and uploaded reports. Rates exclude prompt processing;
these are samples, not a controlled hardware benchmark.

The temporary laptop CPU deployment was stopped and removed. Existing app,
agent and GPU inference health checks passed. No runtime application was upgraded.
