// SPDX-License-Identifier: GPL-3.0-or-later
function markdown(report) {
  const s = report.summary, rows = report.results.map((r) => `| ${r.id} | ${r.repetition} | ${r.outcome} | ${r.handling} | ${r.capability} | ${Math.round(r.durationMs ?? 0)} |`);
  const findings = report.results.filter((r) => r.outcome !== "pass").map((r) => `- ${r.id} (${r.repetition}): ${r.outcome}. ${r.capabilityReason ?? ""} ${(r.turns ?? []).flatMap((t) => t.failures ?? []).join(" ")} ${r.error?.code ?? ""}`);
  return `# Agent evaluation report

Profile: ${report.profile}. Data: ${report.metadata.dataMode}.
Inference: ${report.metadata.inferenceMode}
Corpus: ${report.corpusVersion}; revision: ${report.metadata.agentRevision}.

Runner completion does not imply model accuracy or business capability fulfillment.

Total: ${s.total}. Outcomes: ${JSON.stringify(s.outcomes)}.
Handling: ${JSON.stringify(s.handling)}. Capability: ${JSON.stringify(s.capability)}.
Overall pass rate: ${(100 * s.overallPassRate).toFixed(1)}%; handling pass rate: ${(100 * s.handlingPassRate).toFixed(1)}%.
Denominator: ${s.denominator}

| Case | Repetition | Overall | Handling | Business capability | Milliseconds |
| --- | ---: | --- | --- | --- | ---: |
${rows.join("\n")}

## Findings

${findings.join("\n") || "No failed or blocked cases in this selection."}

Metrics: ${JSON.stringify(report.metrics.scores)}.
Truncation turns: ${report.metrics.truncationTurns}. Timing sample counts and percentile eligibility: see results.json.

See results.json for expected/actual evidence, redacted traces, failures and provenance.
Lexical answer checks are bounded oracles, not comprehensive semantic or financial certification.
`;
}
export {
  markdown
};
