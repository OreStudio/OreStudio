# Historical Stage 4 evaluation baseline

Recorded 2026-11-07. Code: `ebb7fe7849d2089da0444f250bbe32edddfbf405`.
Corpus: 1.0.0. This is the frozen initial measurement, not the current score.
See [current validation](validation.md), [run commands](evaluation.md) and the
[reviewed aggregate](../evals/baselines/stage4-2026-11-07.json).

## Results

| Profile | Cases | Handling passed | Overall passed | Failed checks | Overall blocked data | Errors |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Scripted replay / fixtures | 26 | 26 | 22 | 0 | 2 | 0 |
| Local GPU model / fixtures | 21 | 14 | 12 | 7 | 2 | 0 |
| Local GPU model / existing live reads | 18 | 13 | 11 | 5 | 2 | 0 |

Replay also had two `blocked_backend` outcomes. In real-model profiles, failed
handling took priority over report/booking capability gaps. Missing IR and vega
remained blocked data. All selected cases stayed in the denominator.
Each real-model profile ran once. Replay was not model-quality evidence.

## Retained failures

Model/fixture failures covered scope dates, Gamma factor, refusals and natural
phrasing. Live failures covered trade selection, refusals, natural-date FX and
scope phrasing. A trace needs review before assigning the cause.

The booking-refusal oracle had a known false negative: correct “unable to place”
or “cannot create” wording did not match its keywords. The original failed
check remains recorded. Later results must not overwrite this baseline.

## Validation and source records

85 tests passed on Windows and Linux. All 26 Linux replay outcomes and source
fingerprints matched. Windows used Node v24.16.0; Linux used v24.22.2 in the
pinned image recorded in [validation](validation.md). The extracted archive's
Git revision was `unknown`; the archive revision/fingerprints identify its source.

The model/live runs used LFM3 QAD Q4_0, llama.cpp b12172, configured context
262144, output allowance 2048 and temperature 0.1. These identify configured
files, not the running process's loaded bytes.

Normal runs returned 0 despite failures. Strict replay returned 2 because blocked
capabilities reduced the overall rate. The installed agent stayed at 0.1.5.
No production/backend code, business records or broker configuration changed.
The 19 inspected native/broker/inference PIDs, agent PID and private config stayed
unchanged. Health/readiness checks returned 200.

Private evidence: `build/output/agent-evals-stage4-{replay,model,live}/`,
`agent-evals-stage4-strict/`, `agent-evals-stage4-linux/replay/` and
`agent-evals-stage4-{windows-test,linux-check}.log`.
