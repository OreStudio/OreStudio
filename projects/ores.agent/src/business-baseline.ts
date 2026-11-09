// SPDX-License-Identifier: GPL-3.0-or-later
// Operator inspection only. No new agent/MCP tools or mutation operations.
import { z } from 'zod';
import { AppError, backendRevision, subjects, type Backend } from './backend.js';
import type { Session } from './auth.js';
import { marketId, marketSeries, observationTime } from './market.js';
const page = {
    offset: 0,
    limit: 50,
    order: { field: '', descending: false },
    filter: null,
    as_of: null,
};
const identity = { id: marketId, tenant_id: marketId, party_id: marketId };
const specs = {
    series: { key: 'market_series', schema: marketSeries },
    trades: { key: 'trades', schema: z.object({ ...identity, trade_type: z.string() }) },
    books: {
        key: 'books',
        schema: z.object({ ...identity, name: z.string(), parent_portfolio_id: marketId }),
    },
    portfolios: { key: 'portfolios', schema: z.object({ ...identity, name: z.string() }) },
    reportDefinitions: {
        key: 'definitions',
        schema: z.object({ ...identity, name: z.string(), report_type: z.string() }),
    },
    reportInstances: {
        key: 'instances',
        schema: z.object({
            ...identity,
            name: z.string(),
            definition_id: marketId,
            priced_at: z.string().nullable(),
            completed_at: z.string().nullable(),
            price_note: z.string(),
        }),
    },
    runResults: {
        key: 'results',
        schema: z.object({
            ...identity,
            report_instance_id: marketId,
            subject_type: z.string(),
            subject_id: marketId,
            measure: z.string(),
            currency: z.string(),
            as_of: z.string(),
            figure: z.number().finite(),
            source: z.string(),
        }),
    },
} as const;
type Read = keyof typeof specs;
type Finding = {
    status: string;
    subject: string;
    totalVisible?: number;
    selectedPartyCount?: number;
    rows?: Record<string, unknown>[];
    code?: string;
};
export async function inspectBusiness(backend: Backend, session: Session) {
    const signal = AbortSignal.timeout(120000);
    const findings: Partial<Record<Read, Finding>> = {};
    const resultFilter = Object.fromEntries(
        [
            'party_id',
            'report_instance_id',
            'report_definition_id',
            'subject_type',
            'subject_id',
            'measure',
            'currency',
            'counterparty_id',
            'id_one_of',
            'party_id_one_of',
            'report_instance_id_one_of',
            'report_definition_id_one_of',
            'subject_type_one_of',
            'subject_id_one_of',
            'measure_one_of',
            'currency_one_of',
            'counterparty_id_one_of',
        ].map((k) => [k, k === 'party_id' ? session.partyId : null]),
    );
    const check = () => {
        signal.throwIfAborted();
        if (!session.active || session.expires <= Date.now())
            throw new AppError('session_expired', 'Inspection session expired.', 401);
    };
    for (const operation of Object.keys(specs) as Read[]) {
        const spec = specs[operation],
            rows: Record<string, unknown>[] = [],
            seen = new Set<string>();
        let total: number | undefined;
        try {
            for (let offset = 0; offset < 2000; offset += 50) {
                check();
                const raw = await backend.request(
                    operation,
                    {
                        ...page,
                        offset,
                        ...(operation === 'runResults' ? { filter: resultFilter } : {}),
                    },
                    session,
                    signal,
                );
                check();
                const result = z
                    .object({
                        result: z.object({ outcome: z.string(), code: z.string().optional() }),
                        total: z.number().int().nonnegative(),
                    })
                    .parse(raw);
                if (result.result.outcome !== 'ok') {
                    findings[operation] = {
                        status: result.result.outcome === 'denied' ? 'denied' : 'backend_refused',
                        subject: subjects[operation],
                        code: result.result.code,
                    };
                    break;
                }
                const batch = z.object({ [spec.key]: z.array(spec.schema).max(50) }).parse(raw)[
                    spec.key
                ]!;
                if (
                    (total !== undefined && total !== result.total) ||
                    batch.length !== Math.min(50, Math.max(0, result.total - offset))
                )
                    throw new AppError('incomplete', 'Read changed or page was incomplete.');
                total = result.total;
                for (const row of batch) {
                    if (seen.has(row.id)) throw new AppError('incomplete', 'Duplicate page.');
                    seen.add(row.id);
                    if (row.tenant_id === session.tenantId && row.party_id === session.partyId)
                        rows.push(row);
                }
                findings[operation] = {
                    status: offset + batch.length >= total ? 'ok' : 'incomplete',
                    subject: subjects[operation],
                    totalVisible: total,
                    selectedPartyCount: rows.length,
                    rows,
                };
                if (offset + batch.length >= total) break;
            }
        } catch (e) {
            findings[operation] = {
                status:
                    e instanceof AppError && e.status === 403
                        ? 'denied'
                        : e instanceof AppError
                          ? e.code
                          : 'invalid_reply',
                subject: subjects[operation],
            };
        }
    }
    const health: { runId: string; status: string; health?: string; state?: string }[] = [];
    for (const run of (findings.reportInstances?.rows ?? []).slice(0, 100)) {
        try {
            check();
            const r = z
                .object({
                    result: z.object({ outcome: z.string() }),
                    health: z.string(),
                    state_name: z.string(),
                })
                .parse(
                    await backend.request(
                        'runHealth',
                        { report_instance_id: run.id },
                        session,
                        signal,
                    ),
                );
            check();
            health.push({
                runId: String(run.id),
                status: r.result.outcome,
                health: r.health,
                state: r.state_name,
            });
        } catch (e) {
            health.push({
                runId: String(run.id),
                status: e instanceof AppError ? e.code : 'invalid_reply',
            });
        }
    }
    const trades = findings.trades?.rows ?? [],
        results = findings.runResults?.rows ?? [];
    const usableRunIds = new Set(
        health
            .filter((r) => r.status === 'ok' && r.health === 'completed clean')
            .map((r) => r.runId),
    );
    const example = results.find(
        (r) =>
            r.subject_type === 'trade' &&
            String(r.measure).toLowerCase() === 'npv' &&
            usableRunIds.has(String(r.report_instance_id)) &&
            trades.some((t) => t.id === r.subject_id) &&
            findings.reportInstances?.rows?.some(
                (run) =>
                    run.id === r.report_instance_id &&
                    run.completed_at &&
                    run.priced_at &&
                    String(run.priced_at).slice(0, 10) === String(r.as_of).slice(0, 10),
            ),
    );
    const irSeries = (findings.series?.rows ?? []).filter((r) =>
        String(r.oresmd_uri).startsWith('oresmd://ir/'),
    );
    const irChecks: Record<string, unknown>[] = [];
    for (const series of irSeries.slice(0, 50)) {
        try {
            check();
            const r = z
                .object({
                    result: z.object({ outcome: z.string() }),
                    total: z.number().int().nonnegative(),
                    market_observations: z
                        .array(
                            z.object({
                                series_id: marketId,
                                tenant_id: marketId,
                                party_id: marketId,
                                oresmd_uri: z.string(),
                                value: z.string(),
                                observation_datetime: z.string(),
                                source: z.string(),
                            }),
                        )
                        .max(1),
                })
                .parse(
                    await backend.request(
                        'observations',
                        { series_id: series.id, scope: 'direct', ...page, limit: 1 },
                        session,
                        signal,
                    ),
                );
            check();
            const o = r.market_observations[0];
            if (r.result.outcome !== 'ok') {
                irChecks.push({ seriesId: series.id, status: 'backend_refused' });
                continue;
            }
            if (
                o &&
                (o.tenant_id !== session.tenantId ||
                    o.party_id !== session.partyId ||
                    o.series_id !== series.id ||
                    !Number.isFinite(Number(o.value)) ||
                    !o.source ||
                    !Number.isFinite(observationTime(o.observation_datetime)))
            )
                throw new AppError('invalid_reply', 'IR observation identity or value invalid.');
            irChecks.push({
                seriesId: series.id,
                provider: series.provider_code,
                instrument: series.oresmd_uri,
                status: o ? 'populated' : 'empty',
                total: r.total,
                ...(o ? { observation: o } : {}),
            });
        } catch (e) {
            irChecks.push({
                seriesId: series.id,
                status: e instanceof AppError ? e.code : 'invalid_reply',
            });
        }
    }
    const complete = Object.values(findings).every((f) => f.status === 'ok');
    return {
        inspectedAt: new Date().toISOString(),
        backendContractRevision: backendRevision,
        tenantId: session.tenantId,
        partyId: session.partyId,
        mode: 'read_only',
        findings,
        runHealth: health,
        availableTradeTypes: [...new Set(trades.map((t) => t.trade_type))].sort(),
        availableMeasures: [...new Set(results.map((r) => r.measure))].sort(),
        concreteTradeId: trades[0]?.id ?? null,
        npvExample: example ?? null,
        reportingReady: complete && Boolean(example),
        gaps: [
            ...(!complete
                ? ['One or more reads were denied, invalid, unavailable or incomplete.']
                : []),
            ...(!example
                ? [
                      'No verified clean completed run with a matching selected-party trade NPV was found; do not fabricate a reporting demonstration.',
                  ]
                : []),
        ],
        irBaseline: {
            selectedSeriesCount: irSeries.length,
            checked: irChecks,
            complete: findings.series?.status === 'ok' && irSeries.length <= 50,
            populatedExample: irChecks.find((r) => r.status === 'populated') ?? null,
        },
        limits: { maxRowsPerResource: 2000, maxRunHealthChecks: 100, maxIrChecks: 50 },
    };
}
