// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { marketId, observationTime } from './market.js';
export const reportingTools = {
    find_report_runs: { description: 'Find selected-party stored runs. With tradeId, shows only runs linked by stored result rows, not every attempted run. Shows health and actual pricing dates, not values. No execution.', schema: z.object({ tradeId: marketId.optional(), query: z.string().max(120).default(''), offset: z.number().int().min(0).max(1000).default(0), limit: z.number().int().min(1).max(10).default(5) }).strict() },
    read_run_status: { description: 'Read health and provenance of an exact selected-party report run UUID. Completed with errors is partial, not clean.', schema: z.object({ runId: marketId }).strict() },
    read_trade_results: { description: 'Read stored trade NPV or base NPV with exact run/date/currency/source. Defaults to clean completed runs only. A partial value requires explicit runId and allowPartial=true; failed/running values are withheld. No pricing or report execution.', schema: z.object({ tradeId: marketId, runId: marketId.optional(), measure: z.enum(['npv', 'npv_base']).default('npv'), currency: z.string().regex(/^[A-Za-z]{3}$/).transform(s => s.toUpperCase()).optional(), valuationDate: z.iso.date().optional(), allowPartial: z.boolean().default(false), maxAgeDays: z.number().int().min(0).max(365).default(3) }).strict() },
} as const;
const scope = { id: marketId, tenant_id: marketId, party_id: marketId };
const runSchema = z.object({ ...scope, name: z.string(), definition_id: marketId, priced_at: z.string().nullable(), price_note: z.string(), completed_at: z.string().nullable(),
    cut_id: marketId.nullable(), cut_label: z.string().nullable(), cut_timepoint: z.string().nullable(), engine_settings_id: marketId.nullable(), engine_settings_version: z.number().nullable() });
const resultSchema = z.object({ ...scope, report_instance_id: marketId, report_definition_id: marketId, as_of: z.string(), subject_type: z.string(), subject_id: marketId, measure: z.string(), currency: z.string().regex(/^[A-Z]{3}$/), figure: z.number().finite(), source: z.string().min(1) });
function dateOf(s: string): string { const d = z.iso.date().parse(s.slice(0, 10)); if (s.length > 10) observationTime(s); return d; }
function accepted(raw: unknown) { if (z.object({ result: z.object({ outcome: z.string() }) }).parse(raw).result.outcome !== 'ok') throw new AppError('report_read_refused', 'Backend did not complete or authorize the reporting read.', 403); }
export class Reporting {
    constructor(private backend: Backend, private now: () => number = Date.now) {}
    async execute(name: string, raw: unknown, session: Session, signal?: AbortSignal): Promise<unknown> {
        const args = reportingTools[name as keyof typeof reportingTools].schema.parse(raw) as Record<string, any>;
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]), snapshot = new Date(this.now()).toISOString();
        const check = () => { deadline.throwIfAborted(); if (!session.active || session.expires <= Date.now()) throw new AppError('session_expired', 'Sign in again.', 401); };
        const request = async (op: Operation, data: unknown) => { check(); const r = await this.backend.request(op, data, session, deadline); check(); return r; };
        const own = (r: { tenant_id: string; party_id: string }) => r.tenant_id === session.tenantId && r.party_id === session.partyId;
        const scan = async <T extends z.ZodType>(op: 'reportInstances' | 'runResults', key: string, schema: T, filter: unknown): Promise<z.infer<T>[]> => {
            const rows: z.infer<T>[] = [], seen = new Set<string>(); let total: number | undefined;
            for (let offset = 0; offset < 2000; offset += 50) {
                const r = await request(op, { offset, limit: 50, order: { field: '', descending: false }, filter, ...(op === 'reportInstances' ? { as_of: snapshot } : {}) }); accepted(r);
                const n = z.object({ total: z.number().int().nonnegative() }).parse(r).total;
                const batch = z.object({ [key]: z.array(schema).max(50) }).parse(r)[key]!;
                if (total !== undefined && total !== n || batch.length !== Math.min(50, Math.max(0, n - offset))) throw new AppError('incomplete_reporting', 'Reporting page changed or was incomplete.', 422);
                total = n;
                for (const row of batch) { const id = String((row as any).id); if (seen.has(id)) throw new AppError('incomplete_reporting', 'Duplicate reporting page.', 422); seen.add(id); rows.push(row); }
                if (offset + batch.length >= n) return rows;
            }
            throw new AppError('incomplete_reporting', 'Reporting scan exceeds its 2,000-row limit; narrow the query.', 422);
        };
        const runs = async (ids?: string[]) => (await scan('reportInstances', 'instances', runSchema, ids ? { definition_id: null, kind: null, cut_id: null, id_one_of: ids, definition_id_one_of: null, kind_one_of: null, cut_id_one_of: null } : null)).filter(own);
        const health = async (run: z.infer<typeof runSchema>) => {
            const r = await request('runHealth', { report_instance_id: run.id }); accepted(r);
            return z.object({ health: z.string(), state_name: z.string() }).parse(r);
        };
        const ensureTrade = async (id: string) => {
            const r = await request('trade', { key: { id } });
            if (z.object({ result: z.object({ outcome: z.string() }) }).parse(r).result.outcome === 'missing') return false;
            accepted(r); const trade = z.object({ trade: z.object(scope).nullable() }).parse(r).trade;
            if (trade && trade.id.toLowerCase() !== id.toLowerCase()) throw new AppError('invalid_reply', 'Different trade returned for exact identity.');
            return Boolean(trade && own(trade));
        };
        const results = async () => {
            const filter = Object.fromEntries(['party_id', 'report_instance_id', 'report_definition_id', 'subject_type', 'subject_id', 'measure', 'currency', 'counterparty_id', 'id_one_of', 'party_id_one_of', 'report_instance_id_one_of', 'report_definition_id_one_of', 'subject_type_one_of', 'subject_id_one_of', 'measure_one_of', 'currency_one_of', 'counterparty_id_one_of'].map(k => [k, null])) as Record<string, unknown>;
            Object.assign(filter, { party_id: session.partyId, subject_type: 'trade', subject_id: args.tradeId, report_instance_id: args.runId ?? null, measure: args.measure ?? null, currency: args.currency ?? null });
            const rows = await scan('runResults', 'results', resultSchema, filter);
            if (rows.some(r => !own(r) || r.subject_type !== 'trade' || r.subject_id.toLowerCase() !== args.tradeId.toLowerCase() || args.runId && r.report_instance_id.toLowerCase() !== args.runId.toLowerCase() || args.measure && r.measure !== args.measure || args.currency && r.currency !== args.currency)) throw new AppError('scope_mismatch', 'Result does not match the requested identity.', 403);
            return rows;
        };
        const base = { tool: name, request: args, source: 'OreStudio', partyId: session.partyId, retrievedAt: snapshot };
        if (args.valuationDate && args.valuationDate > snapshot.slice(0, 10)) throw new AppError('future_date', 'Future valuation dates are not supported.', 400);
        if (args.tradeId && !await ensureTrade(args.tradeId)) return { ...base, status: 'not_found', reason: 'Trade is not available in the selected party.' };
        if (name === 'read_run_status') {
            const run = (await runs([args.runId])).find(r => r.id.toLowerCase() === args.runId.toLowerCase());
            return run ? { ...base, status: 'ok', run, ...await health(run) } : { ...base, status: 'not_found', reason: 'Run is not available in the selected party.' };
        }
        const stored = args.tradeId ? await results() : [];
        const ids = [...new Set(stored.map(r => r.report_instance_id))];
        if (name === 'find_report_runs') {
            const selected = (args.tradeId ? ids.length ? await runs(ids) : [] : await runs()).filter(r => r.name.toLowerCase().includes(args.query.toLowerCase())).sort((a, b) => String(b.priced_at).localeCompare(String(a.priced_at)) || b.id.localeCompare(a.id));
            const page = selected.slice(args.offset, args.offset + args.limit), rows = [];
            for (const run of page) rows.push({ ...run, ...await health(run) });
            return { ...base, status: rows.length ? 'ok' : 'not_found', totalMatches: selected.length, rows, offset: args.offset, limit: args.limit, reason: rows.length ? null : 'No matching stored run; no report was started.' };
        }
        if (ids.length > 20) return { ...base, status: 'incomplete', reason: 'More than 20 candidate runs; specify a run ID or valuation date. No value selected.' };
        if (args.allowPartial && !args.runId) return { ...base, status: 'selection_required', reason: 'Inspecting partial values requires an explicit run ID.' };
        const metadata = ids.length ? await runs(ids) : [], evaluated = [];
        for (const run of metadata) evaluated.push({ run, ...await health(run) });
        const eligible = evaluated.filter(r => (r.health === 'completed clean' || args.allowPartial && r.health === 'completed with errors') && r.run.completed_at && observationTime(r.run.completed_at) <= this.now() && r.run.priced_at && dateOf(r.run.priced_at) <= snapshot.slice(0, 10) && (!args.valuationDate || dateOf(r.run.priced_at) === args.valuationDate));
        if (!eligible.length) return { ...base, status: 'unavailable', reason: 'No eligible completed run for this request. Failed/running values and unrequested partial values are withheld.', candidates: evaluated.map(r => ({ id: r.run.id, name: r.run.name, health: r.health, pricedAt: r.run.priced_at, priceNote: r.run.price_note })) };
        // Only compare runs with the same pricing date and configuration. Never silently combine reports.
        eligible.sort((a, b) => dateOf(b.run.priced_at!).localeCompare(dateOf(a.run.priced_at!)) || observationTime(b.run.completed_at!) - observationTime(a.run.completed_at!));
        const newestDate = dateOf(eligible[0]!.run.priced_at!);
        const atDate = eligible.filter(r => dateOf(r.run.priced_at!) === newestDate);
        const configs = new Set(atDate.map(r => JSON.stringify([r.run.definition_id, r.run.cut_id, r.run.engine_settings_id, r.run.engine_settings_version])));
        if (configs.size > 1 || atDate.length > 1 && observationTime(atDate[0]!.run.completed_at!) === observationTime(atDate[1]!.run.completed_at!)) return { ...base, status: 'ambiguous', reason: 'Multiple configurations or equally recent runs; specify a run ID.', candidates: atDate.map(r => ({ id: r.run.id, name: r.run.name, health: r.health })) };
        const chosen = atDate[0]!, rows = stored.filter(r => r.report_instance_id === chosen.run.id);
        if (rows.some(r => r.report_definition_id !== chosen.run.definition_id || dateOf(r.as_of) !== newestDate)) throw new AppError('inconsistent_provenance', 'Result and run pricing provenance disagree.');
        if (rows.length !== 1) return { ...base, status: rows.length ? 'ambiguous' : 'unavailable', reason: 'Expected exactly one value; specify currency or inspect the duplicate result. No value selected.', currencies: [...new Set(rows.map(r => r.currency))] };
        const finalHealth = await health(chosen.run);
        if (finalHealth.health !== chosen.health) return { ...base, status: 'incomplete', reason: 'Run health changed during reading; retry.' };
        const stale = !args.valuationDate && (Date.parse(snapshot.slice(0, 10)) - Date.parse(newestDate)) / 86400000 > args.maxAgeDays;
        return { ...base, status: chosen.health === 'completed with errors' ? 'partial' : stale ? 'stale' : 'ok', health: chosen.health, run: chosen.run, result: rows[0], historicalRequest: Boolean(args.valuationDate), stale,
            warning: chosen.health === 'completed with errors' ? 'Incomplete/errored report output. This is not a clean valuation.' : stale ? 'Stored historical result, not a current valuation.' : null };
    }
}
export function formatReporting(raw: unknown): string | undefined {
    if (!raw || typeof raw !== 'object' || !Object.hasOwn(reportingTools, (raw as any).tool)) return;
    const r = raw as any;
    if (r.result) return `${r.warning ? r.warning + '\n' : ''}${r.result.measure}: ${r.result.figure} ${r.result.currency}\nTrade: ${r.result.subject_id}\nRun: ${r.run.name} [${r.run.id}]\nHealth: ${r.health}\nActual pricing date: ${r.run.priced_at}\nResult as-of: ${r.result.as_of}\nCompleted: ${r.run.completed_at}\nMarket cut: ${r.run.cut_label ?? 'not supplied'} (${r.run.cut_timepoint ?? 'time not supplied'})\nPricing-date note: ${r.run.price_note || 'none'}\nSource: ${r.result.source}\nStored result only; no new pricing or report execution.`;
    if (r.tool === 'read_run_status' && r.run) return `${r.run.name} [${r.run.id}]\nHealth: ${r.health}; state: ${r.state_name}\nActual pricing date: ${r.run.priced_at ?? 'not supplied'}\nCompleted: ${r.run.completed_at ?? 'not completed'}\nPricing-date note: ${r.run.price_note || 'none'}`;
    if (r.rows) return `${r.totalMatches} stored runs; showing ${r.rows.length} from offset ${r.offset}.\n${r.rows.map((x: any) => `${x.name} [${x.id}] — ${x.health}; priced ${x.priced_at ?? 'unknown'}`).join('\n')}`;
    return `${r.reason}\n${(r.candidates ?? []).map((c: any) => `${c.name} [${c.id}] — ${c.health}${c.pricedAt ? '; priced ' + c.pricedAt : ''}${c.priceNote ? '; ' + c.priceNote : ''}`).join('\n')}${r.currencies ? '\nCurrencies: ' + r.currencies.join(', ') : ''}`;
}
