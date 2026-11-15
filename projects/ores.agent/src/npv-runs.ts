// SPDX-License-Identifier: GPL-3.0-or-later
import { createHash, randomUUID } from 'node:crypto';
import { mkdir, open, readFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { z } from 'zod';
import type { Session } from './auth.js';
import { AppError, type Backend, type Operation } from './backend.js';
import { markdownText } from './markdown.js';
import { marketId } from './market.js';
import { readPages } from './read-pages.js';
import { formatReporting } from './reporting.js';
import { reportRunRequest, type RunRequest, type ScopeSelector } from './report-run-request.js';
import { ToolExecutor } from './tools.js';
import { present } from './presentation.js';

const owned = { id: marketId, party_id: marketId, tenant_id: marketId };
const definition = z.object({
    ...owned,
    name: z.string(),
    version: z.number().int(),
    report_type: z.string(),
    pre_processing: z.string(),
    post_processing: z.string(),
    skipped_families: z.string(),
    cut_kind_code: z.string().nullable(),
    cut_id: marketId.nullable(),
});
const runDocument = z.object({
    success: z.boolean(),
    party_id: marketId,
    document: z.object({
        settings: z.object({
            tenant_id: marketId,
            party_id: marketId,
            market_data_type: z.string(),
            market_data_date: z.iso.date(),
            base_currency: z.string().regex(/^[A-Z]{3}$/),
        }),
        analytics: z.array(
            z.object({
                analytic: z.object({ analytic_type_code: z.string(), active: z.string() }),
            }),
        ),
    }),
});
const result = z.object({ outcome: z.string(), code: z.string().optional() });
const admission = z.object({ result, report_instance_id: z.union([marketId, z.literal('')]) });
const receipt = z.object({ baseline: z.array(marketId).max(2000) });
type Outcome = { answer: string; evidence: unknown[]; reportFollow?: string };
type ReportProgress = Outcome & { watching: boolean };
type ResultScope = { type: ScopeSelector['type']; id: string; name: string };
type RunPlan = {
    definitionId: string;
    date: string;
    npvOnly: boolean;
    storesNpv: boolean;
    resultScope?: ResultScope;
    follow: boolean;
};
type Draft = {
    owner: string;
    confirmationId: string;
    plan: RunPlan;
    fingerprint: string;
    expires: number;
    consumed: boolean;
    outcome?: Outcome;
};
const owner = (s: Session) => JSON.stringify([s.sessionId, s.tenantId, s.partyId]);
const belongsTo = (row: { party_id: string; tenant_id: string }, s: Session) =>
    row.party_id === s.partyId && row.tenant_id === s.tenantId;
const guidance =
    'Use “Run NPV report using definition <UUID>” or put the exact definition name in double quotes. You can add “on YYYY-MM-DD” to check the configured valuation date.';
const scopeGuidance =
    'To run the configured scope and request a subset of the stored results, say “Run NPV report using definition <UUID> on YYYY-MM-DD, then show results for book <name>” (or use a portfolio or trade UUID). Ask “Run a new NPV report” to list candidate definitions.';
const progressState = z.object({
    status: z.literal('ok'),
    health: z.string(),
    state_name: z.string(),
    run: z.object({
        id: marketId,
        definition_id: marketId,
        completed_at: z.string().nullable(),
        priced_at: z.string().nullable(),
    }),
});

export class NpvRuns {
    private drafts = new WeakMap<Session, Draft>();
    private tracked = new WeakMap<Session, Map<string, RunPlan>>();
    private executor: Pick<ToolExecutor, 'execute'>;

    constructor(
        private backend: Backend,
        private directory: string,
        executor?: Pick<ToolExecutor, 'execute'>,
    ) {
        this.executor = executor ?? new ToolExecutor(backend, () => {});
    }

    reset(session: Session) {
        this.drafts.delete(session);
    }

    async handle(
        message: string,
        session: Session,
        signal: AbortSignal,
    ): Promise<Outcome | undefined> {
        const confirm = /^confirm (?:npv|report) run ([a-f\d-]{36})[.!]?$/i.exec(message);
        const reconcile = /^check npv submission for definition ([a-f\d-]{36})[.!]?$/i.exec(
            message,
        );
        if (confirm) return this.confirm(marketId.parse(confirm[1]), session, signal);
        if (reconcile) return this.reconcile(marketId.parse(reconcile[1]), session, signal);
        const followRun = /^(?:check|follow|watch) (?:npv |report )?run ([a-f\d-]{36})[.!]?$/i.exec(
            message,
        );
        if (followRun) {
            const id = marketId.parse(followRun[1]).toLowerCase();
            if (this.tracked.get(session)?.has(id)) {
                const outcome = await this.progress(id, session, signal);
                return { ...outcome, ...(outcome.watching ? { reportFollow: id } : {}) };
            }
            const outcome = await this.existingRun(id, session, signal);
            const state = progressState.safeParse(outcome.evidence[0]);
            if (
                state.success &&
                state.data.run.id === id &&
                ['pending', 'queued', 'running'].includes(state.data.state_name)
            ) {
                this.track(id, session, {
                    definitionId: state.data.run.definition_id,
                    date: '',
                    npvOnly: false,
                    storesNpv: false,
                    follow: true,
                });
                return { ...outcome, reportFollow: id };
            }
            return outcome;
        }
        const request = reportRunRequest(message);
        if (!request) return;
        this.reset(session);
        if (request === 'clarify') return { answer: guidance + ' ' + scopeGuidance, evidence: [] };
        if (request.requestedExecutionScope) {
            const resolved = await this.resolveScope(
                request.requestedExecutionScope,
                session,
                signal,
            );
            return {
                answer: resolved
                    ? `I found ${resolved.type} **${markdownText(resolved.name)}**. OreStudio uses the report definition’s configured execution scope, and its report API does not expose the configured book list. ${scopeGuidance}`
                    : `Choose one exact ${request.requestedExecutionScope.type} name or UUID from the selected party. ${scopeGuidance}`,
                evidence: [
                    {
                        tool: 'review_npv_run',
                        status: resolved ? 'execution_scope_unverified' : 'scope_required',
                        scope: resolved,
                    },
                ],
            };
        }
        if (request.selector) return this.review(request, session, signal);
        const choices = await this.definitions(session, signal);
        const candidates = choices
            .filter((d) => d.report_type === 'risk')
            .sort(
                (a, b) =>
                    Number(/\bnpv\b/i.test(b.name)) - Number(/\bnpv\b/i.test(a.name)) ||
                    a.name.localeCompare(b.name),
            )
            .slice(0, 10);
        return {
            answer: [
                'Choose an existing report definition with the scope and valuation settings you want.',
                guidance,
                ...(request.date
                    ? [
                          `Requested valuation date **${request.date}**. Include it when selecting the definition.`,
                      ]
                    : []),
                ...(request.follow ? [scopeGuidance] : []),
                ...candidates.map((d) => `- ${markdownText(d.name)} — \`${d.id}\``),
                ...(!candidates.length
                    ? ['No risk report definitions were found for your selected party.']
                    : []),
            ].join('\n\n'),
            evidence: [
                {
                    tool: 'review_npv_run',
                    status: 'definition_required',
                    candidates: candidates.map(({ id, name }) => ({ id, name })),
                },
            ],
        };
    }

    private check(session: Session, signal: AbortSignal) {
        signal.throwIfAborted();
        if (!session.active || session.expires <= Date.now())
            throw new AppError('session_expired', 'Sign in again.', 401);
    }

    private async read(op: Operation, payload: unknown, session: Session, signal: AbortSignal) {
        this.check(session, signal);
        const raw = await this.backend.request(op, payload, session, signal);
        this.check(session, signal);
        return raw;
    }

    private pages<T extends z.ZodType<{ id: string }>>(
        op: 'reportDefinitions' | 'reportInstances',
        key: string,
        schema: T,
        filter: unknown,
        session: Session,
        signal: AbortSignal,
    ): Promise<z.output<T>[]> {
        const as_of = new Date().toISOString();
        return readPages<z.output<T>>(
            async (offset, limit) => {
                const raw = await this.read(
                    op,
                    { offset, limit, order: { field: '', descending: false }, filter, as_of },
                    session,
                    signal,
                );
                const page = z.object({ result, total: z.number().int().nonnegative() }).parse(raw);
                if (page.result.outcome !== 'ok')
                    throw new AppError(
                        'report_read_refused',
                        'OreStudio did not authorize or complete the report catalogue read.',
                        403,
                    );
                const rows = z.object({ [key]: z.array(schema).max(limit) }).parse(raw)[key]!;
                return { total: page.total, rows };
            },
            (row) => row.id,
            {
                pageSize: 100,
                code: 'incomplete_reporting',
                message: 'Could not verify a complete report catalogue. Retry the review.',
            },
        );
    }

    private async definitions(session: Session, signal: AbortSignal, id?: string) {
        const rows = await this.pages(
            'reportDefinitions',
            'definitions',
            definition,
            {
                party_id: session.partyId,
                id_one_of: id ? [id] : null,
                cut_kind_code: null,
                party_id_one_of: null,
                cut_kind_code_one_of: null,
                search: null,
            },
            session,
            signal,
        );
        return rows.filter((row) => belongsTo(row, session) && (!id || row.id === id));
    }

    private async configuration(
        def: z.infer<typeof definition>,
        session: Session,
        signal: AbortSignal,
        npvOnly: boolean,
    ) {
        if (
            def.report_type !== 'risk' ||
            def.pre_processing !== 'execute' ||
            def.post_processing !== 'execute' ||
            def.cut_kind_code ||
            def.cut_id ||
            (npvOnly &&
                def.skipped_families.split(',').some((family) => family.trim() === 'npv_report'))
        )
            throw new AppError(
                'unsupported_npv_definition',
                'Choose a risk report configured to generate inputs and store results using a fixed valuation date.',
                422,
            );
        const raw = await this.read(
            'runDocument',
            { report_definition_id: def.id },
            session,
            signal,
        );
        const parsed = runDocument.safeParse(raw);
        if (
            !parsed.success ||
            !parsed.data.success ||
            parsed.data.party_id !== session.partyId ||
            !belongsTo(parsed.data.document.settings, session)
        )
            throw new AppError(
                'npv_configuration_unavailable',
                'Could not verify the run settings for this report definition.',
                422,
            );
        const { settings, analytics } = parsed.data.document;
        const active = analytics
            .filter((entry) => entry.analytic.active === 'Y')
            .map((entry) => entry.analytic.analytic_type_code);
        if (
            settings.market_data_type !== 'date' ||
            settings.market_data_date > new Date().toISOString().slice(0, 10) ||
            active.length === 0 ||
            (npvOnly && (active.length !== 1 || active[0] !== 'npv'))
        )
            throw new AppError(
                'unsupported_npv_configuration',
                npvOnly
                    ? 'Choose a report definition with only NPV enabled and an explicit valuation date that has already begun.'
                    : 'Choose a report definition with an enabled analytic and an explicit valuation date that has already begun.',
                422,
            );
        return {
            valuationDate: settings.market_data_date,
            baseCurrency: settings.base_currency,
            analytics: active,
            fingerprint: createHash('sha256')
                .update(JSON.stringify([def, raw]))
                .digest('hex'),
        };
    }

    private async resolveScope(
        scope: ScopeSelector,
        session: Session,
        signal: AbortSignal,
    ): Promise<ResultScope | undefined> {
        if (scope.type === 'trade') {
            const id = marketId.parse(scope.selector).toLowerCase();
            const raw = await this.read('trade', { key: { id } }, session, signal);
            const response = z.object({ result, trade: z.object(owned).nullish() }).parse(raw);
            if (
                response.result.outcome === 'ok' &&
                response.trade &&
                belongsTo(response.trade, session) &&
                response.trade.id === id
            )
                return { type: scope.type, id, name: id };
            return;
        }
        const found = z
            .object({
                searchComplete: z.boolean(),
                totalMatches: z.number().int().nonnegative(),
                rows: z.array(z.object({ id: marketId, name: z.string() })),
            })
            .parse(
                await this.executor.execute(
                    scope.type === 'book' ? 'find_books' : 'find_portfolios',
                    { query: scope.selector, limit: 25 },
                    session,
                    signal,
                ),
            );
        if (!found.searchComplete || found.totalMatches !== found.rows.length) return;
        const match = found.rows.filter(
            (row) =>
                row.id.toLowerCase() === scope.selector.toLowerCase() ||
                row.name.toLowerCase() === scope.selector.toLowerCase(),
        );
        if (match.length === 1) return { type: scope.type, ...match[0]! };
    }

    private async review(
        request: RunRequest,
        session: Session,
        signal: AbortSignal,
    ): Promise<Outcome> {
        const { date, npvOnly } = request;
        const selector = request.selector!;
        const id = marketId.safeParse(selector);
        const matches = (
            await this.definitions(session, signal, id.success ? id.data.toLowerCase() : undefined)
        ).filter((d) => id.success || d.name.toLowerCase() === selector.toLowerCase());
        if (matches.length !== 1)
            return {
                answer: 'Choose one exact report definition from your selected party. ' + guidance,
                evidence: [],
            };
        const def = matches[0]!;
        const config = await this.configuration(def, session, signal, npvOnly);
        if (date && date !== config.valuationDate)
            return {
                answer: `This definition uses ${config.valuationDate}. Select a definition configured for the requested date.`,
                evidence: [],
            };
        if (await this.pendingReceipt(def.id, session))
            return this.reconcile(def.id, session, signal);
        const resultScope = request.resultScope
            ? await this.resolveScope(request.resultScope, session, signal)
            : undefined;
        if (request.resultScope && !resultScope)
            return {
                answer: `Choose one exact ${request.resultScope.type} name or UUID from the selected party for the results.`,
                evidence: [{ tool: 'review_npv_run', status: 'scope_required' }],
            };
        const storesNpv =
            config.analytics.includes('npv') &&
            !def.skipped_families.split(',').some((family) => family.trim() === 'npv_report');
        if (resultScope && !storesNpv)
            return {
                answer: 'This definition does not store the NPV results needed for the requested scope. Choose a definition that stores NPV.',
                evidence: [{ tool: 'review_npv_run', status: 'result_family_unavailable' }],
            };
        this.check(session, signal);
        const draft: Draft = {
            owner: owner(session),
            plan: {
                definitionId: def.id,
                date: config.valuationDate,
                npvOnly,
                storesNpv,
                resultScope,
                follow: request.follow,
            },
            confirmationId: randomUUID(),
            fingerprint: config.fingerprint,
            expires: Date.now() + 300000,
            consumed: false,
        };
        this.drafts.set(session, draft);
        const label = npvOnly ? 'NPV report' : 'report';
        return {
            answer: [
                `Review ${label} **${markdownText(def.name)}**`,
                `Definition \`${def.id}\` (version ${def.version})\nValuation date ${config.valuationDate}\nBase currency ${config.baseCurrency}\nAnalytics ${config.analytics.map(markdownText).join(', ')}`,
                'OreStudio runs the definition’s configured scope. The report API does not expose its configured book list.',
                ...(resultScope
                    ? [
                          `After clean completion, show stored base NPV for ${resultScope.type} **${markdownText(resultScope.name)}** (\`${resultScope.id}\`).`,
                      ]
                    : []),
                `To submit this review within five minutes, reply \`Confirm ${npvOnly ? 'NPV' : 'report'} run ${draft.confirmationId}\`.`,
            ].join('\n\n'),
            evidence: [
                {
                    tool: 'review_npv_run',
                    status: 'confirmation_required',
                    definitionId: def.id,
                    valuationDate: config.valuationDate,
                    baseCurrency: config.baseCurrency,
                    analytics: config.analytics,
                    resultScope,
                },
            ],
        };
    }

    private lockPath(id: string, session: Session) {
        return join(
            this.directory,
            `${marketId.parse(session.tenantId)}-${marketId.parse(session.partyId)}-${marketId.parse(id)}.json`,
        );
    }

    private async pendingReceipt(id: string, session: Session) {
        try {
            return receipt.parse(JSON.parse(await readFile(this.lockPath(id, session), 'utf8')));
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
            throw new AppError(
                'npv_receipt_unavailable',
                'The submission record could not be verified. Check existing report runs before attempting another submission.',
                409,
            );
        }
    }

    private async runs(id: string, session: Session, signal: AbortSignal) {
        const rows = await this.pages(
            'reportInstances',
            'instances',
            z.object({
                ...owned,
                definition_id: marketId,
                name: z.string(),
                recorded_at: z.string().optional(),
            }),
            {
                definition_id: id,
                kind: 'production',
                cut_id: null,
                id_one_of: null,
                definition_id_one_of: null,
                kind_one_of: null,
                cut_id_one_of: null,
            },
            session,
            signal,
        );
        return rows.filter((r) => belongsTo(r, session) && r.definition_id === id);
    }

    private async reconcile(id: string, session: Session, signal: AbortSignal): Promise<Outcome> {
        const pending = await this.pendingReceipt(id, session);
        if (!pending)
            return {
                answer: 'There is no unresolved submission for that definition in this agent.',
                evidence: [],
            };
        const previous = new Set(pending.baseline);
        const candidates = (await this.runs(id, session, signal)).filter(
            (r) => !previous.has(r.id),
        );
        return {
            answer: [
                'The earlier submission outcome is unknown. Check the candidate runs before attempting another submission. A scheduled or independently triggered run can also appear here.',
                ...candidates.slice(0, 10).map((r) => `- ${markdownText(r.name)} — \`${r.id}\``),
                ...(!candidates.length ? ['No new candidate run is visible yet.'] : []),
            ].join('\n\n'),
            evidence: [
                {
                    status: 'submission_unknown',
                    definitionId: id,
                    candidateCount: candidates.length,
                    candidates: candidates.slice(0, 10),
                },
            ],
        };
    }

    private async confirm(id: string, session: Session, signal: AbortSignal): Promise<Outcome> {
        const draft = this.drafts.get(session);
        if (!draft || draft.owner !== owner(session) || draft.confirmationId !== id.toLowerCase())
            return {
                answer: 'No matching report review exists in this session. Request a new review.',
                evidence: [],
            };
        if (draft.outcome) return draft.outcome;
        if (draft.consumed || draft.expires <= Date.now())
            return {
                answer: 'That confirmation is in progress or has expired. Check the report review before submitting again.',
                evidence: [],
            };
        draft.consumed = true;
        const plan = draft.plan;
        const def = (await this.definitions(session, signal, plan.definitionId))[0];
        if (
            !def ||
            (await this.configuration(def, session, signal, plan.npvOnly)).fingerprint !==
                draft.fingerprint
        )
            return {
                answer: 'The report configuration changed after review. Request a new review before submitting.',
                evidence: [],
            };
        const baseline = (await this.runs(def.id, session, signal)).map((r) => r.id);
        await mkdir(this.directory, { recursive: true, mode: 0o700 });
        const path = this.lockPath(def.id, session);
        try {
            const file = await open(path, 'wx', 0o600);
            try {
                await file.writeFile(JSON.stringify({ baseline }) + '\n');
                await file.sync();
            } finally {
                await file.close();
            }
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code === 'EEXIST')
                return this.reconcile(def.id, session, signal);
            throw error;
        }
        let response: z.infer<typeof admission>;
        let sent = false;
        try {
            this.check(session, signal);
            if (draft.expires <= Date.now())
                throw new AppError('confirmation_expired', 'Request a new NPV report review.', 409);
            sent = true;
            response = admission.parse(
                await this.backend.request(
                    'triggerReport',
                    {
                        report_definition_id: def.id,
                        tenant_id: session.tenantId,
                        job_instance_id: 0,
                        kind: 'production',
                        covered_date: '',
                        scope_code: '',
                        business_centre_code: '',
                    },
                    session,
                    signal,
                ),
            );
        } catch (error) {
            if (!sent) {
                await unlink(path);
                throw error;
            }
            if (
                error instanceof AppError &&
                [
                    'backend_denied',
                    'unauthorized',
                    'session_expired',
                    'backend_bad_request',
                ].includes(error.code)
            ) {
                await unlink(path).catch(() => {});
                return (draft.outcome = {
                    answer: markdownText(error.message),
                    evidence: [{ status: 'rejected', definitionId: def.id, code: error.code }],
                });
            }
            return (draft.outcome = this.unknown(def.id));
        }
        if (
            !response.report_instance_id &&
            !['invalid', 'missing', 'denied', 'forbidden'].includes(response.result.outcome) &&
            response.result.code !== 'no_run_grant'
        )
            return (draft.outcome = this.unknown(def.id));
        // Keep the known result even if the local lock cannot be removed.
        const released = await unlink(path).then(
            () => true,
            () => false,
        );
        const code = /^[a-z0-9_]{1,80}$/.test(response.result.code ?? '')
            ? response.result.code
            : undefined;
        const runId = response.report_instance_id;
        if (!runId)
            return (draft.outcome = {
                answer: `OreStudio declined the report request${code ? ` (\`${code}\`)` : ''}.`,
                evidence: [{ status: 'rejected', definitionId: def.id, code }],
            });
        this.track(runId, session, plan);
        const evidence: unknown[] = [
            {
                status: 'recorded',
                runId,
                definitionId: def.id,
                code,
                outcome: response.result.outcome,
            },
        ];
        const answer = [`OreStudio recorded run \`${runId}\`${code ? ` (\`${code}\`)` : ''}.`];
        if (response.result.outcome !== 'ok')
            answer.push('OreStudio reported an error after recording the run.');
        else if (code === 'not_dispatched')
            answer.push(
                'Check the run state for the outcome of the definition’s concurrency policy.',
            );
        if (!released)
            answer.push(
                'The submission record still needs reconciliation before another submission.',
            );
        const initial = await this.progress(runId, session, signal).catch(() => undefined);
        if (initial) {
            evidence.push(...initial.evidence);
            answer.push(initial.answer);
        }
        if (!initial || initial.watching)
            answer.push(
                `Say \`Follow run ${runId}\` to check progress. Stopping chat or watching stops status checks only.`,
            );
        return (draft.outcome = {
            answer: answer.join('\n\n'),
            evidence,
            ...(plan.follow && (!initial || initial.watching) ? { reportFollow: runId } : {}),
        });
    }

    private async existingRun(
        runId: string,
        session: Session,
        signal: AbortSignal,
    ): Promise<Outcome> {
        const state = await this.executor.execute('read_run_status', { runId }, session, signal);
        return {
            answer: formatReporting(state) ?? 'Run status is unavailable.',
            evidence: [state],
        };
    }

    private track(runId: string, session: Session, plan: RunPlan) {
        let tracked = this.tracked.get(session);
        if (!tracked) this.tracked.set(session, (tracked = new Map()));
        tracked.set(runId, plan);
    }

    async progress(runId: string, session: Session, signal: AbortSignal): Promise<ReportProgress> {
        this.check(session, signal);
        const plan = this.tracked.get(session)?.get(runId);
        if (!plan)
            throw new AppError(
                'run_not_tracked',
                'This run is not being followed in this session. Ask for its status by run ID.',
                404,
            );
        let report: Outcome;
        try {
            report = await this.existingRun(runId, session, signal);
            this.check(session, signal);
        } catch (error) {
            this.check(session, signal);
            if (error instanceof AppError && error.status === 401) throw error;
            return {
                watching: true,
                answer: `Status for run \`${runId}\` is temporarily unavailable. OreStudio may still be processing it.`,
                evidence: [],
            };
        }
        const parsed = progressState.safeParse(report.evidence[0]);
        if (!parsed.success)
            return {
                watching: true,
                answer: `OreStudio has recorded run \`${runId}\`. Its state is not yet verified for the selected party.`,
                evidence: [],
            };
        const state = parsed.data;
        if (state.run.id !== runId || state.run.definition_id !== plan.definitionId)
            return {
                watching: false,
                answer: 'OreStudio returned a different run or definition. No results are shown.',
                evidence: [],
            };
        const terminal = ['completed', 'failed', 'cancelled', 'skipped'].includes(state.state_name);
        if (!terminal) return { ...report, watching: true };
        if (state.state_name !== 'completed' || state.health !== 'completed clean')
            return {
                ...report,
                watching: false,
                answer:
                    report.answer +
                    '\n\nThe run did not complete cleanly. Stored totals are unavailable.',
            };
        if (!plan.resultScope)
            return {
                ...report,
                watching: false,
                answer:
                    report.answer +
                    (plan.storesNpv
                        ? `\n\nAsk for the stored NPV of a trade, book or portfolio using run \`${runId}\`.`
                        : '\n\nThe current results tools do not display this report’s output automatically.'),
            };
        if (
            !state.run.priced_at ||
            !state.run.completed_at ||
            state.run.priced_at.slice(0, 10) !== plan.date
        )
            return {
                ...report,
                watching: false,
                answer:
                    report.answer +
                    `\n\nThe run has no verified completion and pricing date matching the reviewed date ${plan.date}. NPV is unavailable for this request.`,
            };
        const scope = plan.resultScope;
        try {
            const args = { runId, valuationDate: plan.date, measure: 'npv_base' };
            const result =
                scope.type === 'trade'
                    ? await this.executor.execute(
                          'read_trade_results',
                          { ...args, tradeId: scope.id },
                          session,
                          signal,
                      )
                    : await this.executor.execute(
                          'summarize_scope_results',
                          { ...args, scopeType: scope.type, scope: scope.id },
                          session,
                          signal,
                      );
            this.check(session, signal);
            return {
                watching: false,
                answer:
                    report.answer +
                    '\n\n' +
                    (present(result).answer ??
                        'Stored NPV is unavailable for the requested scope.'),
                evidence: [...report.evidence, result],
            };
        } catch (error) {
            this.check(session, signal);
            if (error instanceof AppError && error.status === 401) throw error;
            const cause =
                error instanceof AppError && /^[a-z_]+$/.test(error.code)
                    ? ` (\`${error.code}\`)`
                    : '';
            return {
                ...report,
                watching: false,
                answer:
                    report.answer +
                    `\n\nThe stored NPV or historical scope could not be verified${cause}. Say \`Follow run ${runId}\` to check again.`,
            };
        }
    }

    private unknown(id: string): Outcome {
        return {
            answer: `OreStudio did not confirm the submission outcome. Reply \`Check NPV submission for definition ${id}\` to look for candidate runs. This definition is held for reconciliation.`,
            evidence: [{ status: 'submission_unknown', definitionId: id }],
        };
    }
}
