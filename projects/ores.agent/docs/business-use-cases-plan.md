# Agent work: status and next steps

Updated: 2026-11-08. Local agent/evaluation Stages 0–6 are delivered.
This does not mean all business use cases work. Current code is 0.1.9.
See [validation](validation.md) for release evidence and failures.

## Current scope

Continue work in the agent, its UI, tests, evaluations and separate deployment.
Use existing authenticated NATS reads. Keep the current model/runtime as the
baseline unless a model experiment is explicitly selected.

Do not change main/reporting/ORE code, schemas, business records, broker settings
or main UI as part of this scope. Do not prepare data or start reports to improve
scores. Existing login, party selection and logout are allowed session operations.
Historical setup actions are not permission to repeat them.

Keep financial units, coverage, identity and source checks. Missing data is not
zero. Failed validation withholds the value. Keep current agent aggregation;
server migration is optional, not a requirement to use the agent.

## Delivered work

| Stage | Result | Limits/evidence |
| --- | --- | --- |
| 0 | Inspect existing business data and APIs | [Dated baseline](business-baseline.md); not current inventory |
| 1 | Find trades, books, portfolios and counterparties | [Discovery](discovery.md); partial catalogue and product limits |
| 2 | Read stored trade values and run health | [Reporting](reporting.md); current base-NPV unit case still fails |
| 3 | Historical scope NPV and raw Greek groups | [Scope](scope-reporting.md), [Greeks](greeks.md); incomplete groups have no total |
| 4 | Evaluation runner and initial baseline | [Baseline](evaluation-baseline.md); failures and oracle limits retained |
| 5 | Intent and conversation improvements | [Stage 5 evidence](stage5-validation.md); not broad accuracy proof |
| 6 | Repeatable checks, comparison and private timing | [Stage 6 evidence](stage6-validation.md); data/command gaps remain |

Later updates added typed requests and stronger review fixtures in 0.1.8, then
Markdown chat in 0.1.9. The run-continuation regression recorded in Stage 6 passed
both live repetitions during 0.1.8 review. Historical results remain unchanged.

## Next agent work

Use evaluation traces to select one bounded improvement. Candidates include
clearer questions, better tool descriptions, wider paraphrase coverage and
stronger response types. Keep one assistant and the shared chat/MCP executor.
Do not add a framework or move a calculation merely to change the architecture.

Before each change, record the baseline corpus, fixtures, grader, model settings
and data IDs. Compare the same case selection. Keep failed/blocked cases in the
denominator. Add versioned tests without rewriting the original baseline.
Model/profile changes need separate comparisons. See [evaluation](evaluation.md).

Validate each runnable checkpoint, then commit and push to the fork. Document
changes need link/content checks, not an application rebuild. Open a PR only
when the user explicitly requests one.

## Work for lead review

Use [architecture proposals](architecture-future-prs.md) for component ownership
and natural-language examples. F1–F3 are optional shared queries. Other proposals
start with existing APIs and confirmed business gaps. A failed evaluation does
not approve server work.

Bookings, report execution, executable quotes, hosted authentication and durable
business commands remain unavailable through the agent. Positive live IR/vega,
restricted broker access, low-permission/cross-tenant proof and native Linux GPU
execution remain separate checks.
