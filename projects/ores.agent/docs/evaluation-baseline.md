# Stage 4 evaluation baseline — 2026-11-07

Stage 4 is delivered: a runnable evaluator, versioned corpus and complete
reports, including failures. This is an initial measurement, not completion of
every business use case. Implementation: `ebb7fe7849d2089da0444f250bbe32edddfbf405`;
corpus 1.0.0. See [commands and scoring](evaluation.md) and the reviewed
[machine-readable aggregate](../evals/baselines/stage4-2026-11-07.json).

| Profile | Cases | Handling passed | Overall passed | Failed checks | Overall blocked data | Errors |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Scripted replay / fixtures | 26 | 26 | 22 | 0 | 2 | 0 |
| Local GPU model / fixtures | 21 | 14 | 12 | 7 | 2 | 0 |
| Local GPU model / existing live reads | 18 | 13 | 11 | 5 | 2 | 0 |

Replay also has two overall `blocked_backend` outcomes. In both real-model
profiles, the report/booking capability gaps are still recorded separately;
their failed handling checks take precedence in the overall outcome. Missing
IR observations and vega remain blocked data. All selected cases stay in the
denominator. Replay is scripted inference, not model-quality evidence.
One repetition per real-model profile does not establish general accuracy or
latency. Handling correctness and business fulfillment remain separate scores.

## Findings for Stage 5

The fixture-model profile failed automated checks for scope NPV/Delta date
arguments, the Gamma factor argument, report and booking refusal, a natural-date
FX paraphrase and a scope-value paraphrase. The live profile failed checks for
trade NPV selection, report and booking refusal, natural-date FX (turn budget)
and the scope-value paraphrase. These traces identify work to inspect; they do
not automatically identify the agent or backend as the cause.

**Known oracle false negative:** the booking answer correctly refuses creation
using wording such as “unable to place” and “cannot create”, which the bounded
keyword matcher does not recognise. The frozen baseline retains that failed
check. Review and version any oracle correction explicitly in Stage 5, then
compare results without hiding this original result. Other failures include
strict request-argument checks even where returned figures can be correct.

No unavailable positive IR/vega or command capability is claimed. Future server
contracts remain [separate proposals](architecture-future-prs.md).

## Validation and provenance

All **85 component/harness tests pass on Windows and Linux**. Linux additionally
reproduced all 26 replay outcomes, with matching source fingerprints. Its source
archive was created from the implementation revision above; the extracted
archive report itself records Git revision `unknown`. Linux Node was v24.22.2;
Windows Node v24.16.0. Pinned Linux image:
`node@sha256:01d566e8d94d8c619591d78652babfccd3c6992a3dffa9ed5ab39e11cf6c70e9`.
This is Linux test/replay evidence, not native Linux NVIDIA validation.

The Windows baseline reports record a clean component at that revision. Local
model/live runs used existing llama.cpp b12172 and official LFM3-1.7B QAD Q4_0,
with matching GGUF hash, configured context 262144 and CUDA offload. Model
requests allowed 2048 output tokens with temperature 0.1. Full hashes/settings
are in the reviewed aggregate. These identify configured artifacts and do not
attest which bytes an already-running process loaded.

Normal completed evaluations returned 0 despite failures. Strict replay returned
the expected 2 because blocked capabilities lower the all-case pass rate;
deterministic CLI tests also cover setup errors and refusal to overwrite reports.

Private evidence is retained under ignored `build/output/`:

- `agent-evals-stage4-{replay,model,live}/`: `results.json`, `summary.md`, progress
  and reservation metadata; raw traces may contain business data.
- `agent-evals-stage4-strict/`: strict-mode report.
- `agent-evals-stage4-linux/replay/`: Linux replay report.
- `agent-evals-stage4-windows-test.log` and
  `agent-evals-stage4-linux-check.log`: test evidence.

Only reviewed aggregates are committed. No production agent source, reporting/
main-server/ORE source, broker configuration or business records changed in
this stage. Live checks used existing reads and normal login/logout only.
The installed agent remains `0.1.5-cf24e13db443`; no deployment was needed.
All 19 inspected native/broker/inference PIDs, the agent PID and private config
hash stayed unchanged. Main app, agent health and readiness returned HTTP 200.

Next: Stage 5 agent/evaluation improvements against this frozen evidence,
retaining failures, blocked capabilities and explicit oracle revisions.
