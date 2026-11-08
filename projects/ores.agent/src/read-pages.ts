// SPDX-License-Identifier: GPL-3.0-or-later
import { AppError } from './backend.js';

export interface Page<T> { total: number; rows: T[] }
export interface PagePolicy { code: string; message: string; pageSize?: number; maxRows?: number }

// Own-scope filtering, snapshot dates, wire validation and session/deadline
// checks remain with each adapter. Only bounded page mechanics are shared.
export async function readPages<T>(fetch: (offset: number, limit: number) => Promise<Page<T>>,
    identity: (row: T) => string, policy: PagePolicy): Promise<T[]> {
    const size = policy.pageSize ?? 50;
    const maximum = policy.maxRows ?? 2000;
    const rows: T[] = [];
    const seen = new Set<string>();
    let total: number | undefined;
    const fail = () => new AppError(policy.code, policy.message, 422);
    for (let offset = 0; offset < maximum; offset += size) {
        const page = await fetch(offset, size);
        if (!Number.isInteger(page.total) || page.total < 0 ||
            total !== undefined && page.total !== total ||
            page.rows.length !== Math.min(size, Math.max(0, page.total - offset))) throw fail();
        total = page.total;
        for (const row of page.rows) {
            const id = identity(row);
            if (!id || seen.has(id)) throw fail();
            seen.add(id);
            rows.push(row);
        }
        if (offset + page.rows.length >= total) return rows;
    }
    throw fail();
}
