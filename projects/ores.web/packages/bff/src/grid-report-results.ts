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
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as bookSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { subjects as bookingSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_booking_protocol';
import { invalidRequest, notFound } from './errors.js';
import { readAll, resultSchema, row, text } from './refdata-calls.js';
import { fetchStored } from './grid-downloads.js';
import { readRun, readRunStates as readStates } from './grid-report-runs.js';
import type { LiveSession } from './sessions.js';

/*
 * The results a run wrote, at the level a reader asks for.
 *
 * The engine writes one row per trade into npv.csv, and nothing else reads that
 * file. The BFF reads it here, joins the trades to their books and names their
 * counterparties, and rolls the figures up when the reader asks for a book or a
 * counterparty view. The browser receives rows rather than four lists to join.
 *
 * The answer carries a reason code rather than a sentence: the screen words it,
 * in the reader's language.
 */

const runParamsSchema = z.object({ id: z.uuid() });
const resultsQuerySchema = z.object({
    by: z.enum(['trade', 'book', 'counterparty']).default('trade'),
    baseline: z.uuid().optional(),
});

/** The levels a result can be read at. */
export type ResultLevel = 'trade' | 'book' | 'counterparty';

/** One figure in the currency it is stated in. */
export interface Money {
    readonly currency: string;
    readonly total: string;
}

/** One trade's row, as the engine wrote it, with the records it names. */
export interface RunResultRow {
    readonly trade_id: string;
    readonly trade_type: string;
    readonly npv: string;
    readonly npv_currency: string;
    readonly npv_base: string;
    readonly notional: string;
    readonly notional_currency: string;
    readonly counterparty: string;
    readonly counterparty_name: string;
    readonly book_id: string;
    readonly book_name: string;
}

/** One book's or counterparty's share of a run. */
export interface AggregateRow {
    readonly key: string;
    readonly name: string;
    readonly trades: number;
    readonly totals: readonly Money[];
    readonly notional: readonly Money[];
    /** The counterparty group a member belongs to, empty when it is top level. */
    readonly group: string;
    /** Whether the row is a group's roll-up rather than a member of one. */
    readonly is_group: boolean;
    /** What this row was in the run it is compared with. */
    readonly previous: readonly Money[];
    /** The change against that run, each currency apart. */
    readonly change: readonly Money[];
    /** The change as a share of the earlier figure, empty when it had none. */
    readonly change_pct: readonly Money[];
    /** Whether the row is only in the earlier run. */
    readonly is_removed: boolean;
}

/** Why a report has no rows to show. */
export type ResultReason = '' | 'no-run' | 'no-output' | 'no-result-file' | 'no-figures';

interface TradeAnswer {
    readonly level: 'trade';
    readonly rows: readonly RunResultRow[];
    readonly totals: readonly Money[];
    readonly reason: ResultReason;
    readonly recordsComplete: boolean;
}

interface AggregateAnswer {
    readonly level: 'book' | 'counterparty';
    readonly rows: readonly AggregateRow[];
    readonly totals: readonly Money[];
    readonly reason: ResultReason;
    readonly recordsComplete: boolean;
    /** The run this answer is compared with, empty when it is compared with none. */
    readonly baseline: BaselineSummary | null;
}

/** The run a comparison is against. */
export interface BaselineSummary {
    readonly id: string;
    readonly totals: readonly Money[];
    readonly change: readonly Money[];
    readonly change_pct: readonly Money[];
}

export type ResultsAnswer = TradeAnswer | AggregateAnswer;

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
    notional_currency: 'NotionalCurrency',
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
                notional_currency: at(COLUMNS.notional_currency),
                counterparty: at(COLUMNS.counterparty),
                counterparty_name: '',
                book_id: '',
                book_name: '',
            };
        })
        .filter((row) => row.trade_id !== '');
}

/** What the records say about the trades in a run. */
interface TradeRecords {
    readonly counterpartyName: ReadonlyMap<string, string>;
    readonly counterpartyParent: ReadonlyMap<string, string>;
    readonly bookOf: ReadonlyMap<string, string>;
    readonly bookName: ReadonlyMap<string, string>;
    readonly complete: boolean;
}

/**
 * The names and books behind the ids npv.csv writes.
 *
 * A read that fails leaves the names empty rather than failing the figures. A
 * trade with no booking keeps an empty book, which the book level shows as
 * unbooked rather than dropping.
 */
async function recordsOf(session: LiveSession, tradeIds: readonly string[]): Promise<TradeRecords> {
    const empty = {
        counterpartyName: new Map<string, string>(),
        counterpartyParent: new Map<string, string>(),
        bookOf: new Map<string, string>(),
        bookName: new Map<string, string>(),
        complete: false,
    };
    try {
        const [counterparties, bookings, books] = await Promise.all([
            readAll(session, counterpartySubjects.list_counterparties_request, 'counterparties', {
                filter: null,
                as_of: null,
            }),
            readAll(session, bookingSubjects.list_trade_bookings_request, 'trade_bookings', {
                filter: { trade_id_one_of: [...tradeIds] },
            }),
            readAll(session, bookSubjects.list_books_request, 'books', { filter: null }),
        ]);
        const counterpartyName = new Map(
            counterparties.map((counterparty) => [
                text(counterparty['id']),
                text(counterparty['full_name']),
            ]),
        );
        return {
            counterpartyName,
            counterpartyParent: new Map(
                counterparties
                    .map(
                        (counterparty) =>
                            [
                                text(counterparty['id']),
                                text(counterparty['parent_counterparty_id']),
                            ] as const,
                    )
                    .filter(([, parent]) => parent !== ''),
            ),
            bookOf: new Map(
                bookings.map((booking) => [text(booking['trade_id']), text(booking['book_id'])]),
            ),
            bookName: new Map(books.map((book) => [text(book['id']), text(book['name'])])),
            complete: true,
        };
    } catch {
        return empty;
    }
}

/** The rows with the names and books they point at. */
async function withRecords(
    session: LiveSession,
    rows: readonly RunResultRow[],
): Promise<{
    rows: readonly RunResultRow[];
    complete: boolean;
    counterpartyParent: ReadonlyMap<string, string>;
    counterpartyName: ReadonlyMap<string, string>;
}> {
    const records = await recordsOf(
        session,
        rows.map((row) => row.trade_id),
    );
    const resolved = rows.map((row) => {
        const bookId = records.bookOf.get(row.trade_id) ?? '';
        return {
            ...row,
            counterparty_name: records.counterpartyName.get(row.counterparty) ?? '',
            book_id: bookId,
            book_name: records.bookName.get(bookId) ?? '',
        };
    });
    return {
        rows: resolved,
        complete: records.complete,
        counterpartyParent: records.counterpartyParent,
        counterpartyName: records.counterpartyName,
    };
}

/** The per-currency total of a figure the rows carry. */
function totalsOf(
    rows: readonly RunResultRow[],
    figureOf: (row: RunResultRow) => { readonly amount: string; readonly currency: string },
): readonly Money[] {
    const totals = new Map<string, number>();
    for (const row of rows) {
        const { amount, currency } = figureOf(row);
        const figure = Number.parseFloat(amount);
        if (!Number.isFinite(figure)) {
            continue;
        }
        totals.set(currency, (totals.get(currency) ?? 0) + figure);
    }
    return [...totals.entries()].map(([currency, total]) => ({
        currency,
        total: total.toFixed(2),
    }));
}

/** The change between two sets of figures, each currency apart. */
function changeBetween(
    current: readonly Money[],
    previous: readonly Money[],
): { change: readonly Money[]; pct: readonly Money[] } {
    const currencies = [
        ...new Set([
            ...current.map((money) => money.currency),
            ...previous.map((money) => money.currency),
        ]),
    ];
    const figureIn = (money: readonly Money[], currency: string): number => {
        const found = money.find((entry) => entry.currency === currency);
        const figure = found === undefined ? Number.NaN : Number.parseFloat(found.total);
        return Number.isFinite(figure) ? figure : 0;
    };
    const change: Money[] = [];
    const pct: Money[] = [];
    for (const currency of currencies) {
        const now = figureIn(current, currency);
        const before = figureIn(previous, currency);
        change.push({ currency, total: (now - before).toFixed(2) });
        pct.push({
            currency,
            total: before === 0 ? '' : (((now - before) / Math.abs(before)) * 100).toFixed(2),
        });
    }
    return { change, pct };
}

/**
 * The rows stated against an earlier run of the same report.
 *
 * A row the earlier run did not have carries no earlier figure, and a row only
 * the earlier run had is kept and marked, so a book or a name that stopped
 * appearing is read as gone rather than silently dropped.
 */
export function comparedWith(
    current: readonly AggregateRow[],
    earlier: readonly AggregateRow[],
): readonly AggregateRow[] {
    const before = new Map(earlier.map((row) => [row.key, row]));
    const rows = current.map((row) => {
        const was = before.get(row.key);
        const previous = was?.totals ?? [];
        const { change, pct } = changeBetween(row.totals, previous);
        return { ...row, previous, change, change_pct: pct };
    });
    const keys = new Set(current.map((row) => row.key));
    for (const row of earlier) {
        if (keys.has(row.key)) {
            continue;
        }
        const { change, pct } = changeBetween([], row.totals);
        rows.push({ ...row, previous: row.totals, change, change_pct: pct, is_removed: true });
    }
    return rows;
}

/** One figure of a set of rows, each currency added apart. */
function moneyOf(
    rows: readonly AggregateRow[],
    figureOfRow: (row: AggregateRow) => readonly Money[],
): readonly Money[] {
    const totals = new Map<string, number>();
    for (const row of rows) {
        for (const money of figureOfRow(row)) {
            const figure = Number.parseFloat(money.total);
            if (Number.isFinite(figure)) {
                totals.set(money.currency, (totals.get(money.currency) ?? 0) + figure);
            }
        }
    }
    return [...totals.entries()].map(([currency, total]) => ({
        currency,
        total: total.toFixed(2),
    }));
}

/**
 * The counterparties with a group row above the members that share a parent.
 *
 * A group's figure is the sum of its members, so a reader can check one against
 * the other. A counterparty that is itself a parent and trades in the run is one
 * of its own group's members, so its trades are counted once and its row is
 * stated under the group it leads. A counterparty with no parent stands on its
 * own, and a parent whose name was not read is left standing rather than grouped
 * under a guess.
 */
export function withCounterpartyGroups(
    rows: readonly AggregateRow[],
    parents: ReadonlyMap<string, string>,
    names: ReadonlyMap<string, string>,
): readonly AggregateRow[] {
    // Only a parent that has at least one child row in this run leads a group;
    // a parent whose children did not trade would otherwise repeat its own row.
    const leading = new Set(
        rows.map((row) => parents.get(row.key) ?? '').filter((parent) => parent !== ''),
    );
    const members = new Map<string, AggregateRow[]>();
    const standing: AggregateRow[] = [];
    for (const row of rows) {
        const parent = parents.get(row.key) ?? '';
        const group = parent !== '' ? parent : leading.has(row.key) ? row.key : '';
        const groupName = names.get(group) ?? '';
        if (group === '' || groupName === '') {
            standing.push(row);
            continue;
        }
        members.set(group, [...(members.get(group) ?? []), { ...row, group: groupName }]);
    }
    const groups = [...members.entries()]
        .map(([parent, children]) => {
            const ordered = [...children].sort((left, right) => figureOf(right) - figureOf(left));
            const group: AggregateRow = {
                key: parent,
                name: names.get(parent) ?? '',
                trades: children.reduce((sum, child) => sum + child.trades, 0),
                totals: moneyOf(children, (row) => row.totals),
                notional: moneyOf(children, (row) => row.notional),
                group: '',
                is_group: true,
                previous: [],
                change: [],
                change_pct: [],
                is_removed: false,
            };
            return { group, ordered };
        })
        .sort((left, right) => figureOf(right.group) - figureOf(left.group));

    const ordered: AggregateRow[] = [];
    for (const entry of groups) {
        ordered.push(entry.group, ...entry.ordered);
    }
    ordered.push(...[...standing].sort((left, right) => figureOf(right) - figureOf(left)));
    return ordered;
}

/** The rows rolled up by the key they share. */
export function aggregateRows(
    rows: readonly RunResultRow[],
    keyOf: (row: RunResultRow) => string,
    names: ReadonlyMap<string, string>,
): readonly AggregateRow[] {
    const groups = new Map<string, RunResultRow[]>();
    for (const row of rows) {
        const key = keyOf(row);
        groups.set(key, [...(groups.get(key) ?? []), row]);
    }
    return [...groups.entries()]
        .map(([key, members]) => ({
            key,
            name: names.get(key) ?? '',
            trades: members.length,
            group: '',
            is_group: false,
            previous: [],
            change: [],
            change_pct: [],
            is_removed: false,
            totals: totalsOf(members, (row) => ({
                amount: row.npv,
                currency: row.npv_currency,
            })),
            notional: totalsOf(members, (row) => ({
                amount: row.notional,
                currency: row.notional_currency,
            })),
        }))
        .sort((left, right) => figureOf(right) - figureOf(left));
}

/** A row's figure, so the largest reads first and a large loss does not. */
function figureOf(row: AggregateRow): number {
    return row.totals.reduce((sum, total) => {
        const figure = Number.parseFloat(total.total);
        return sum + (Number.isFinite(figure) ? figure : 0);
    }, 0);
}

/** The rows of a run's result, or the reason there are none. */
async function tradeRowsOfKey(
    session: LiveSession,
    key: string,
): Promise<{
    rows: readonly RunResultRow[];
    reason: ResultReason;
    recordsComplete: boolean;
    counterpartyParent: ReadonlyMap<string, string>;
    counterpartyName: ReadonlyMap<string, string>;
}> {
    if (key === '') {
        return {
            rows: [],
            reason: 'no-output',
            recordsComplete: true,
            counterpartyParent: new Map(),
            counterpartyName: new Map(),
        };
    }
    const archive = await fetchStored(session, key);
    const tar = archive[0] === 0x1f && archive[1] === 0x8b ? gunzipSync(archive) : archive;
    const resultFile = [...entriesOf(tar).entries()].find(
        ([name]) => name.split('/').pop()?.toLowerCase() === 'npv.csv',
    );
    if (resultFile === undefined) {
        return {
            rows: [],
            reason: 'no-result-file',
            recordsComplete: true,
            counterpartyParent: new Map(),
            counterpartyName: new Map(),
        };
    }
    const rows = rowsOf(resultFile[1].toString('utf8'));
    if (rows === null) {
        return {
            rows: [],
            reason: 'no-figures',
            recordsComplete: true,
            counterpartyParent: new Map(),
            counterpartyName: new Map(),
        };
    }
    const resolved = await withRecords(session, rows);
    return {
        rows: resolved.rows,
        reason: '',
        recordsComplete: resolved.complete,
        counterpartyParent: resolved.counterpartyParent,
        counterpartyName: resolved.counterpartyName,
    };
}

/** The answer for the level the reader asked for. */
async function resultsOfKey(
    session: LiveSession,
    key: string,
    level: ResultLevel,
): Promise<ResultsAnswer> {
    const read = await tradeRowsOfKey(session, key);
    const totals = totalsOf(read.rows, (row) => ({
        amount: row.npv,
        currency: row.npv_currency,
    }));
    if (level === 'trade') {
        return {
            level,
            rows: read.rows,
            totals,
            reason: read.reason,
            recordsComplete: read.recordsComplete,
        };
    }
    const keyOf =
        level === 'book'
            ? (row: RunResultRow): string => row.book_id
            : (row: RunResultRow): string => row.counterparty;
    const names = new Map(
        read.rows.map((row) => [
            keyOf(row),
            level === 'book' ? row.book_name : row.counterparty_name,
        ]),
    );
    const rows = aggregateRows(read.rows, keyOf, names);
    return {
        level,
        rows:
            level === 'counterparty'
                ? withCounterpartyGroups(rows, read.counterpartyParent, read.counterpartyName)
                : rows,
        totals,
        reason: read.reason,
        recordsComplete: read.recordsComplete,
        baseline: null,
    };
}

/**
 * The answer for a level, stated against an earlier run of the same report.
 *
 * The two runs are read at the same level and compared row by row. A level that
 * is not an aggregate is not compared, because a trade is read on its own.
 */
async function comparedOfKey(
    session: LiveSession,
    key: string,
    baselineKey: string,
    level: 'book' | 'counterparty',
    baselineId: string,
): Promise<AggregateAnswer> {
    const [current, earlier] = await Promise.all([
        resultsOfKey(session, key, level),
        resultsOfKey(session, baselineKey, level),
    ]);
    if (current.level === 'trade' || earlier.level === 'trade') {
        throw new Error('a trade level is not compared');
    }
    const { change, pct } = changeBetween(current.totals, earlier.totals);
    return {
        level,
        rows: comparedWith(current.rows, earlier.rows),
        totals: current.totals,
        reason: current.reason,
        recordsComplete: current.recordsComplete,
        baseline: {
            id: baselineId,
            totals: earlier.totals,
            change,
            change_pct: pct,
        },
    };
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
        const states = await readStates(session, [parsed.data.id]);
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

    /** The results of one run, at the level the reader asked for. */
    server.get('/api/reporting/runs/:id/results', async (request): Promise<ResultsAnswer> => {
        const session = requireSession(request);
        const parsed = runParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address names an id that is not a uuid.');
        }
        const query = resultsQuerySchema.safeParse(request.query);
        if (!query.success) {
            throw invalidRequest('The level asked for is not one a result is read at.');
        }
        const run = await readRun(session, parsed.data.id);
        if (run === null) {
            return {
                level: query.data.by,
                rows: [],
                totals: [],
                reason: 'no-run',
                recordsComplete: true,
                baseline: null,
            };
        }
        if (query.data.baseline !== undefined && query.data.by !== 'trade') {
            const earlierRun = await readRun(session, query.data.baseline);
            if (earlierRun === null) {
                throw invalidRequest('The run compared with does not exist.');
            }
            if (text(earlierRun['definition_id']) !== text(run['definition_id'])) {
                throw invalidRequest('The run compared with is a run of another report.');
            }
            const compared = await comparedOfKey(
                session,
                text(run['output_storage_key']),
                text(earlierRun['output_storage_key']),
                query.data.by,
                query.data.baseline,
            );
            request.log.info(
                'reporting results: run %s compared with %s at the %s level',
                parsed.data.id,
                query.data.baseline,
                query.data.by,
            );
            return compared;
        }
        const answer = await resultsOfKey(session, text(run['output_storage_key']), query.data.by);
        if (!answer.recordsComplete) {
            request.log.warn(
                'reporting results: the books or counterparties behind run %s were not read',
                parsed.data.id,
            );
        }
        return answer;
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
