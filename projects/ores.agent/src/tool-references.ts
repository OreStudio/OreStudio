// SPDX-License-Identifier: GPL-3.0-or-later
import { marketId } from './market.js';

type Row = Record<string, unknown>;
const object = (value: unknown): Row =>
    value && typeof value === 'object' && !Array.isArray(value) ? (value as Row) : {};

// Read only typed identity fields from successful, party-scoped adapter output.
// Never promote echoed requests, free text, ambiguous candidates or Python output.
export class ToolReferences {
    private values = new Map<string, Set<string>>();

    has(key: string, value: string) {
        return this.values.get(key)?.has(value.toLowerCase()) ?? false;
    }

    observe(raw: unknown) {
        const result = object(raw);
        if (result.status !== undefined && !['ok', 'stale'].includes(String(result.status))) return;
        const add = (key: string, value: unknown) => {
            if (typeof value !== 'string' || !marketId.safeParse(value).success) return;
            const values = this.values.get(key) ?? new Set<string>();
            values.add(value.toLowerCase());
            this.values.set(key, values);
        };
        const rows = Array.isArray(result.rows) ? result.rows.map(object) : [];
        switch (result.tool) {
            case 'find_trades':
                for (const row of rows) add('tradeId', row.id);
                break;
            case 'read_trade':
                add('tradeId', object(result.trade).id);
                break;
            case 'find_report_runs':
                for (const row of rows) add('runId', row.id);
                break;
            case 'read_run_status':
                add('runId', object(result.run).id);
                break;
            case 'list_market_series':
                for (const row of rows) add('seriesId', row.id);
                break;
            case 'read_fx_spot':
            case 'read_ir_rate':
                add('seriesId', object(result.series).seriesId);
                break;
        }
    }
}
