/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { gunzipSync } from 'node:zlib';
import { z } from 'zod';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { storagePath } from './grid-downloads.js';
import { readRun } from './grid-report-runs.js';
import type { LiveSession } from './sessions.js';

/**
 * The per-trade results of a run.
 *
 * The engine writes them into the run's output archive as npv.csv, and nothing
 * else reads that file. The BFF reads it here, once, so the screen receives rows
 * rather than an archive and the engine's column names stay in one place.
 */

const runParamsSchema = z.object({ id: z.uuid() });

/** One trade's row, as the engine wrote it. The figures stay text. */
export interface RunResultRow {
    readonly trade_id: string;
    readonly trade_type: string;
    readonly npv: string;
    readonly npv_base: string;
    readonly notional: string;
    readonly counterparty: string;
}

interface ResultsAnswer {
    readonly rows: readonly RunResultRow[];
    readonly message: string;
}

/** The entries of a tar archive, by name. */
function entriesOf(archive: Buffer): Map<string, Buffer> {
    const entries = new Map<string, Buffer>();
    let offset = 0;
    while (offset + 512 <= archive.length) {
        const header = archive.subarray(offset, offset + 512);
        if (header.every((byte) => byte === 0)) {
            break;
        }
        const name = header.subarray(0, 100).toString('utf8').replace(/\0.*$/, '');
        const sizeText = header.subarray(124, 136).toString('utf8').replace(/\0.*$/, '').trim();
        const size = Number.parseInt(sizeText, 8);
        const type = header.subarray(156, 157).toString('utf8');
        const start = offset + 512;
        if (!Number.isFinite(size) || size < 0 || start + size > archive.length) {
            break;
        }
        if (type === '0' || type === '') {
            entries.set(name, archive.subarray(start, start + size));
        }
        offset = start + Math.ceil(size / 512) * 512;
    }
    return entries;
}

/** One CSV line, honouring the quotes the engine writes around a name with a comma. */
function splitCsv(line: string): string[] {
    const fields: string[] = [];
    let field = '';
    let quoted = false;
    for (let i = 0; i < line.length; i += 1) {
        const c = line[i];
        if (quoted) {
            if (c === '"' && line[i + 1] === '"') {
                field += '"';
                i += 1;
            } else if (c === '"') {
                quoted = false;
            } else {
                field += c;
            }
            continue;
        }
        if (c === '"') {
            quoted = true;
        } else if (c === ',') {
            fields.push(field);
            field = '';
        } else if (c !== '\r') {
            field += c;
        }
    }
    fields.push(field);
    return fields;
}

const COLUMNS = {
    trade_id: 'TradeId',
    trade_type: 'TradeType',
    npv: 'NPV',
    npv_base: 'NPV(Base)',
    notional: 'Notional',
    counterparty: 'CounterParty',
} as const;

function rowsOf(csv: string): readonly RunResultRow[] | null {
    const lines = csv.split('\n').filter((line) => line.trim() !== '');
    if (lines.length === 0) {
        return [];
    }
    const header = splitCsv(lines[0] ?? '');
    const index = (name: string): number => header.indexOf(name);
    if (index(COLUMNS.trade_id) < 0 || index(COLUMNS.npv) < 0) {
        return null;
    }
    return lines
        .slice(1)
        .map((line) => {
            const fields = splitCsv(line);
            const at = (name: string): string => fields[index(name)] ?? '';
            return {
                trade_id: at(COLUMNS.trade_id),
                trade_type: at(COLUMNS.trade_type),
                npv: at(COLUMNS.npv),
                npv_base: at(COLUMNS.npv_base),
                notional: at(COLUMNS.notional),
                counterparty: at(COLUMNS.counterparty),
            };
        })
        .filter((row) => row.trade_id !== '');
}

async function storedBytes(session: LiveSession, key: string): Promise<Buffer> {
    const path = storagePath(key);
    if (path === null) {
        throw notFound('The run names no stored output.');
    }
    const base = await session.client.discoverHttpBaseUrl();
    if (base === null) {
        throw notFound('The deployment has no file service.');
    }
    const response = await fetch(new URL(path, base), {
        headers: { Authorization: `Bearer ${session.client.token}` },
        redirect: 'error',
    });
    if (response.status === 404) {
        throw notFound('The stored output is no longer there.');
    }
    if (response.status === 401 || response.status === 403) {
        throw notPermitted('The file service refused the stored output.');
    }
    if (!response.ok) {
        throw new HttpFailure(502, {
            code: 'upstream-unavailable',
            message: 'The stored output could not be read.',
        });
    }
    const body = Buffer.from(await response.arrayBuffer());
    return body[0] === 0x1f && body[1] === 0x8b ? gunzipSync(body) : body;
}

export function registerGridReportResultsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get(
        '/api/operations/grid/report-runs/:id/results',
        async (request): Promise<ResultsAnswer> => {
            const session = requireSession(request);
            const parsed = runParamsSchema.safeParse(request.params);
            if (!parsed.success) {
                throw invalidRequest('The address names an id that is not a uuid.');
            }
            const run = await readRun(session, parsed.data.id);
            if (run === null) {
                throw notFound('No report run has this id.');
            }
            const key =
                typeof run['output_storage_key'] === 'string' ? run['output_storage_key'] : '';
            if (key === '') {
                return { rows: [], message: 'This run produced no output.' };
            }

            const files = entriesOf(await storedBytes(session, key));
            const results = [...files.entries()].find(
                ([name]) => name.split('/').pop()?.toLowerCase() === 'npv.csv',
            );
            if (results === undefined) {
                return { rows: [], message: "The run's output holds no npv.csv." };
            }
            const rows = rowsOf(results[1].toString('utf8'));
            if (rows === null) {
                return {
                    rows: [],
                    message: "The run's npv.csv carries no TradeId and NPV columns.",
                };
            }
            return { rows, message: '' };
        },
    );
}
