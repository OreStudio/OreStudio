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
import { invalidRequest, notFound } from './errors.js';
import { resultSchema, row, text } from './refdata-calls.js';
import { fetchStored } from './grid-downloads.js';
import { readRun, readRunStates } from './grid-report-runs.js';
import type { LiveSession } from './sessions.js';

/*
 * The per-trade results a run wrote.
 *
 * The engine writes them into the run's output archive as npv.csv, and nothing
 * else reads that file. The BFF reads it here so the screen receives rows rather
 * than an archive, and the engine's column names stay in one place. The answer
 * carries a reason code rather than a sentence: the screen words it, in the
 * reader's language.
 */

const runParamsSchema = z.object({ id: z.uuid() });

/** One trade's row, as the engine wrote it. The figures stay text. */
export interface RunResultRow {
    readonly trade_id: string;
    readonly trade_type: string;
    readonly npv: string;
    readonly npv_currency: string;
    readonly npv_base: string;
    readonly notional: string;
    readonly counterparty: string;
}

/**
 * Why a report has no rows to show.
 *
 * The empty answer is a fact about the run rather than an error, so it is a code
 * the screen words rather than a failure the caller must catch.
 */
export type ResultReason = '' | 'no-run' | 'no-output' | 'no-result-file' | 'no-figures';

export interface ResultsAnswer {
    readonly rows: readonly RunResultRow[];
    readonly reason: ResultReason;
}

const TAR_BLOCK_BYTES = 512;
const TAR_NAME_OFFSET = 0;
const TAR_NAME_LENGTH = 100;
const TAR_SIZE_OFFSET = 124;
const TAR_SIZE_LENGTH = 12;
const TAR_TYPE_OFFSET = 156;
const TAR_TYPE_LENGTH = 1;
const TAR_BASE_OCTAL = 8;

/** The entries of a tar archive, by name. */
export function entriesOf(archive: Buffer): Map<string, Buffer> {
    const entries = new Map<string, Buffer>();
    let offset = 0;
    while (offset + TAR_BLOCK_BYTES <= archive.length) {
        const header = archive.subarray(offset, offset + TAR_BLOCK_BYTES);
        if (header.every((byte) => byte === 0)) {
            break;
        }
        const name = header
            .subarray(TAR_NAME_OFFSET, TAR_NAME_OFFSET + TAR_NAME_LENGTH)
            .toString('utf8')
            .replace(/\0.*$/, '');
        const sizeText = header
            .subarray(TAR_SIZE_OFFSET, TAR_SIZE_OFFSET + TAR_SIZE_LENGTH)
            .toString('utf8')
            .replace(/\0.*$/, '')
            .trim();
        const size = Number.parseInt(sizeText, TAR_BASE_OCTAL);
        const type = header
            .subarray(TAR_TYPE_OFFSET, TAR_TYPE_OFFSET + TAR_TYPE_LENGTH)
            .toString('utf8');
        const start = offset + TAR_BLOCK_BYTES;
        if (!Number.isFinite(size) || size < 0 || start + size > archive.length) {
            break;
        }
        if (type === '0' || type === '') {
            entries.set(name, archive.subarray(start, start + size));
        }
        offset = start + Math.ceil(size / TAR_BLOCK_BYTES) * TAR_BLOCK_BYTES;
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
    npv_currency: 'NpvCurrency',
    npv_base: 'NPV(Base)',
    notional: 'Notional',
    counterparty: 'CounterParty',
} as const;

/**
 * The rows of npv.csv, or nothing when the file names no trade and no figure.
 *
 * The header the engine writes starts with a hash, and a file can start with a
 * byte-order mark, so both are taken off before the columns are read by name.
 */
export function rowsOf(rawCsv: string): readonly RunResultRow[] | null {
    const csv = rawCsv.replace(/^\uFEFF/, '');
    const lines = csv.split('\n').filter((line) => line.trim() !== '');
    if (lines.length === 0) {
        return [];
    }
    const header = splitCsv((lines[0] ?? '').replace(/^#/, ''));
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
                npv_currency: at(COLUMNS.npv_currency),
                npv_base: at(COLUMNS.npv_base),
                notional: at(COLUMNS.notional),
                counterparty: at(COLUMNS.counterparty),
            };
        })
        .filter((row) => row.trade_id !== '');
}

/** The rows of a run's result, or the reason there are none. */
async function resultsOfKey(session: LiveSession, key: string): Promise<ResultsAnswer> {
    if (key === '') {
        return { rows: [], reason: 'no-output' };
    }
    const archive = await fetchStored(session, key);
    const tar = archive[0] === 0x1f && archive[1] === 0x8b ? gunzipSync(archive) : archive;
    const resultFile = [...entriesOf(tar).entries()].find(
        ([name]) => name.split('/').pop()?.toLowerCase() === 'npv.csv',
    );
    if (resultFile === undefined) {
        return { rows: [], reason: 'no-result-file' };
    }
    const rows = rowsOf(resultFile[1].toString('utf8'));
    if (rows === null) {
        return { rows: [], reason: 'no-figures' };
    }
    return { rows, reason: '' };
}

export function registerGridReportResultsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** One reporting run, with the state it reached. */
    server.get('/api/reporting/runs/:id', async (request) => {
        const session = requireSession(request);
        const parsed = runParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address names an id that is not a uuid.');
        }
        const run = await readRun(session, parsed.data.id);
        if (run === null) {
            throw notFound('No report run has this id.');
        }
        const states = await readRunStates(session, [parsed.data.id]);
        return {
            id: text(run['id']),
            name: text(run['name']),
            party_id: text(run['party_id']),
            definition_id: text(run['definition_id']),
            kind: text(run['kind']),
            state_name: states.get(parsed.data.id) ?? '',
            started_at: text(run['started_at']),
            completed_at: text(run['completed_at']),
            priced_at: text(run['priced_at']),
            price_note: text(run['price_note']),
            output_message: text(run['output_message']),
            output_storage_key: text(run['output_storage_key']),
        };
    });

    /** The per-trade results of one run. */
    server.get('/api/reporting/runs/:id/results', async (request): Promise<ResultsAnswer> => {
        const session = requireSession(request);
        const parsed = runParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address names an id that is not a uuid.');
        }
        const run = await readRun(session, parsed.data.id);
        if (run === null) {
            return { rows: [], reason: 'no-run' };
        }
        return resultsOfKey(session, text(run['output_storage_key']));
    });

    /** The output archive of one run, as the run stored it. */
    server.get('/api/reporting/runs/:id/output', async (request, reply) => {
        const session = requireSession(request);
        const parsed = runParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address names an id that is not a uuid.');
        }
        const run = await readRun(session, parsed.data.id);
        if (run === null) {
            throw notFound('No report run has this id.');
        }
        const key = text(run['output_storage_key']);
        if (key === '') {
            throw notFound('This run produced no output.');
        }
        const archive = await fetchStored(session, key);
        return reply
            .header('Content-Type', 'application/gzip')
            .header('Content-Disposition', 'attachment; filename="output.tar.gz"')
            .header('Cache-Control', 'private, no-store')
            .send(archive);
    });
}
