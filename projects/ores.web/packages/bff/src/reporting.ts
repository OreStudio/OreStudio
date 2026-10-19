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
import { z } from 'zod';
import { readMyParties } from '@ores/wire-protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_definition_protocol';
import { subjects as instanceSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_instance_protocol';
import { subjects as errorSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_error_event_protocol';
import { subjects as operationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_operations_protocol';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import { intentSchema, readAll, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The reporting routes: the reports a party runs, one row per definition, and
 * one definition read and edited.
 *
 * The server answers the definitions, each definition's latest run, the health
 * word of that run, its problem counts, its next fire and the party's name and
 * short code as six separate reads. Composing them here rather than in the
 * browser is what lets the list degrade one cell at a time: the row is a
 * definition, and everything else on it is a decoration that can be absent and
 * say why.
 *
 * A definition is named by its id throughout. Its name is not a key a session
 * can read by: the table is unique on (tenant, name, party), and a session that
 * sees several parties therefore sees several definitions of one name. The
 * id is the primary key, so nothing in the model has to move for the screen to
 * be right.
 */

/** The page a list opens on, as the record screen standard sets it. */
const DEFAULT_PAGE_SIZE = 15;

/** The largest page a caller may ask for, as the generated read allows. */
const MAX_PAGE_SIZE = 1000;

/** The words a row's health cell can hold. The server derives them; this only names them. */
const HEALTH_WORDS = [
    'failed',
    'completed with errors',
    'running',
    'never run',
    'completed clean',
] as const;

type HealthWord = (typeof HEALTH_WORDS)[number];

/**
 * What a person is told when an address names a record their parties do not
 * have. It is the standard's sentence for nothing found, and it names no entity
 * the interface does not otherwise speak of.
 */
const ABSENT = 'There is no such record, or you may not see it.';

/** The parts of a row that a separate read fills, and so the parts that can be unread. */
type Cell = 'parties' | 'latestRun' | 'health' | 'errors' | 'nextFire';

/** A read that failed, and the cell it would have filled. */
interface Unread {
    readonly cell: Cell;
    readonly message: string;
}

/** The latest run of a definition, as the list draws it. */
interface RunView {
    readonly id: string;
    readonly startedAt: string | null;
    readonly completedAt: string | null;
    readonly errors: number | null;
    readonly warnings: number | null;
}

/**
 * One row, as this module composes it.
 *
 * The browser reads the same shape through `reportsPageSchema` in
 * `wire-protocol/src/reporting.ts`, which brands the two instants; the module
 * states them as plain strings because this side only passes them through and
 * never parses them.
 */
interface ReportRow {
    readonly id: string;
    readonly name: string;
    readonly reportType: string;
    readonly partyId: string;
    readonly partyName: string;
    readonly partyCode: string | null;
    readonly scheduleExpression: string;
    readonly health: HealthWord | null;
    readonly stateName: string;
    readonly latestRun: RunView | null;
    readonly nextFireAt: string | null;
}

/** A party as the session's own account states it: the name a person reads, and the code. */
interface PartyName {
    readonly name: string;
    readonly shortCode: string;
}

/**
 * One page of definitions and the server's total, which is what the pager is
 * built from. A page is what the list shows, so the decorations are composed for
 * its rows only.
 */
interface DefinitionPage {
    readonly definitions: readonly Record<string, unknown>[];
    readonly total: number;
}

const pageQuerySchema = z.object({
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(MAX_PAGE_SIZE)).default(DEFAULT_PAGE_SIZE),
    sort: z.string().max(100).default(''),
    descending: z
        .enum(['true', 'false'])
        .default('false')
        .transform((value) => value === 'true'),
    /** The party the list is narrowed to, from a filter of the page. */
    party_id: z.uuid().optional(),
});

const idSchema = z.uuid();

/** The name an address may carry instead of an id, as long as the definition's own field. */
const nameSchema = z.string().trim().min(1).max(200);

/**
 * The generated definition filter, with every member stated.
 *
 * The wire decoder reads the filter as a whole record, so a member left out is a
 * filter that does not decode; a member that is not set is stated as null.
 */
interface DefinitionFilter {
    readonly party_id: string | null;
    readonly id_one_of: readonly string[] | null;
    readonly party_id_one_of: readonly string[] | null;
}

/** The filter that narrows to one party, or to one definition by its id. */
function definitionFilter(members: {
    readonly partyId?: string;
    readonly idOneOf?: readonly string[];
}): DefinitionFilter {
    return {
        party_id: members.partyId ?? null,
        id_one_of: members.idOneOf ?? null,
        party_id_one_of: null,
    };
}

const idParam = (request: FastifyRequest): string =>
    parseInput(idSchema, (request.params as { id: string }).id, 'A definition is named by its id.');

const nullableText = (value: unknown): string | null => {
    const stated = text(value);
    return stated === '' ? null : stated;
};

const countOf = (value: unknown): number => (typeof value === 'number' ? value : 0);

const healthOf = (value: unknown): HealthWord | null => {
    const word = text(value);
    return HEALTH_WORDS.find((candidate) => candidate === word) ?? null;
};

/**
 * A request's input read against its schema, or a 400 that names the task.
 *
 * The shared `input` states the schema's own complaint, which names a field and
 * not what the person was doing, so these routes say what the request was.
 */
function parseInput<Schema extends z.ZodType>(
    schema: Schema,
    value: unknown,
    message: string,
): z.infer<Schema> {
    const parsed = schema.safeParse(value);
    if (!parsed.success) {
        throw invalidRequest(message);
    }
    return parsed.data;
}

/**
 * Runs one decoration, or records that it could not be read.
 *
 * The list is the definition read and nothing else, so a decoration that fails
 * costs its own cell and not the screen. The message is the server's own, which
 * names the permission or the fault that stopped it.
 */
async function decorate<T>(
    cell: Cell,
    unread: Unread[],
    read: () => Promise<T>,
): Promise<T | undefined> {
    try {
        return await read();
    } catch (error) {
        unread.push({ cell, message: error instanceof Error ? error.message : String(error) });
        return undefined;
    }
}

/** One page of the definitions the session's parties own, with the server's total. */
async function readDefinitionsPage(
    session: LiveSession,
    page: {
        readonly offset: number;
        readonly limit: number;
        readonly sort: string;
        readonly descending: boolean;
        readonly partyId: string | null;
    },
): Promise<DefinitionPage> {
    const filter = page.partyId === null ? null : definitionFilter({ partyId: page.partyId });
    const reply = await session.client.callAuthenticated(
        definitionSubjects.list_report_definitions_request,
        {
            offset: page.offset,
            limit: page.limit,
            order: { field: page.sort, descending: page.descending },
            filter,
            as_of: null,
        },
        z.looseObject({
            result: resultSchema,
            definitions: z.array(row).default([]),
            total: z.int().nonnegative().default(0),
        }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    return { definitions: reply.definitions, total: reply.total };
}

/**
 * One definition, by the id that names it.
 *
 * The generated read by key takes a name, and a name is shared across parties,
 * so the row is read with the list's one-of filter on the primary key instead.
 * That is the same read the list makes, narrowed to one row, so no model change
 * is needed to read a definition exactly.
 */
async function readDefinition(session: LiveSession, id: string): Promise<Record<string, unknown>> {
    const reply = await session.client.callAuthenticated(
        definitionSubjects.list_report_definitions_request,
        {
            offset: 0,
            limit: 1,
            order: { field: '', descending: false },
            filter: definitionFilter({ idOneOf: [id] }),
            as_of: null,
        },
        z.looseObject({ result: resultSchema, definitions: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    const [definition] = reply.definitions;
    if (definition === undefined) {
        throw notFound(ABSENT);
    }
    return definition;
}

/**
 * One definition that carries a name an address asked for.
 *
 * A name is unique only within a party, so an address that names one can match
 * several records. Each choice states the party that owns the definition, when
 * it is scheduled and what its latest run did, which is what a person needs to
 * pick the one they meant.
 */
interface NameChoice {
    readonly id: string;
    readonly name: string;
    readonly partyName: string;
    readonly scheduleExpression: string;
    readonly health: HealthWord | null;
    readonly stateName: string;
    readonly hasRun: boolean;
}

/** What an address segment names, when it is not an id. */
type NameResolution =
    | { readonly kind: 'one'; readonly definition: Record<string, unknown> }
    | { readonly kind: 'many'; readonly choices: readonly NameChoice[] }
    | { readonly kind: 'none' };

/**
 * The definitions of the parties in scope that carry a name.
 *
 * The generated read filters on no name, so the definitions the session can see
 * are read and matched here. A name that matches none is absent, one is the
 * record the address meant, and several are a choice the page must offer rather
 * than a row this side may guess at.
 */
async function definitionsNamed(
    session: LiveSession,
    name: string,
): Promise<readonly Record<string, unknown>[]> {
    const definitions = await readAll(
        session,
        definitionSubjects.list_report_definitions_request,
        'definitions',
        { filter: null, as_of: null },
    );
    return definitions.filter((definition) => text(definition['name']) === name);
}

/** The choices a shared name offers, each with what the list would say about it. */
async function choicesFor(
    session: LiveSession,
    named: readonly Record<string, unknown>[],
): Promise<readonly NameChoice[]> {
    const composed = await compose(session, named);
    return composed.rows.map((row) => ({
        id: row.id,
        name: row.name,
        partyName: row.partyName,
        scheduleExpression: row.scheduleExpression,
        health: row.health,
        stateName: row.stateName,
        hasRun: row.latestRun !== null,
    }));
}

/** What the name an address asked for resolves to. */
async function resolveName(session: LiveSession, name: string): Promise<NameResolution> {
    const named = await definitionsNamed(session, name);
    const [only] = named;
    if (only === undefined) {
        return { kind: 'none' };
    }
    if (named.length === 1) {
        return { kind: 'one', definition: only };
    }
    return { kind: 'many', choices: await choicesFor(session, named) };
}

/**
 * A refusal that carries the definitions the name could have meant.
 *
 * The address does not name one record, and the person cannot be told to find
 * an id: the choices are the answer, so they travel in the refusal rather than
 * as a second response.
 */
function sharedName(name: string, choices: readonly NameChoice[]): HttpFailure {
    return new HttpFailure(
        409,
        {
            code: 'conflict',
            message: `${choices.length} reports of the parties you work in are named ${name}. Choose the one you want.`,
        },
        { choices },
    );
}

/** The latest run of each definition named, one entry per definition that has run. */
async function readLatestRuns(
    session: LiveSession,
    definitionIds: readonly string[],
): Promise<readonly Record<string, unknown>[]> {
    const reply = await session.client.callAuthenticated(
        instanceSubjects.get_latest_report_instances_by_definition_request,
        { definition_ids: [...definitionIds] },
        z.looseObject({ result: resultSchema, instances: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    return reply.instances;
}

/** The health word of each run named, with the state it was derived from. */
async function readHealth(
    session: LiveSession,
    instanceIds: readonly string[],
): Promise<ReadonlyMap<string, { health: HealthWord | null; stateName: string }>> {
    const reply = await session.client.callAuthenticated(
        instanceSubjects.get_report_instance_health_request,
        { instance_ids: [...instanceIds] },
        z.looseObject({ result: resultSchema, health: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    return new Map(
        reply.health.map((entry) => [
            text(entry['report_instance_id']),
            { health: healthOf(entry['health']), stateName: text(entry['state_name']) },
        ]),
    );
}

/** How many of each run's problems are errors and how many are warnings. */
async function readProblemCounts(
    session: LiveSession,
    instanceIds: readonly string[],
): Promise<ReadonlyMap<string, { errors: number; warnings: number }>> {
    const reply = await session.client.callAuthenticated(
        errorSubjects.get_report_error_counts_request,
        { report_instance_ids: [...instanceIds] },
        z.looseObject({ result: resultSchema, counts: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    return new Map(
        reply.counts.map((entry) => [
            text(entry['report_instance_id']),
            { errors: countOf(entry['errors']), warnings: countOf(entry['warnings']) },
        ]),
    );
}

/** When the scheduler says each definition's job fires next, or none when it states none. */
async function readNextFires(
    session: LiveSession,
    definitionIds: readonly string[],
): Promise<ReadonlyMap<string, string | null>> {
    const reply = await session.client.callAuthenticated(
        operationSubjects.get_next_fire_request,
        { definition_ids: [...definitionIds] },
        z.looseObject({ result: resultSchema, schedules: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    return new Map(
        reply.schedules.map((entry) => [
            text(entry['definition_id']),
            nullableText(entry['next_fire_at']),
        ]),
    );
}

/**
 * The name and short code of each party the caller's own account works in.
 *
 * The session carries the party's name and no code, so both come from the
 * account's own parties and from nowhere else. That read is also the list of
 * parties a filter may offer, so it answers the only parties the screen can
 * name. A row whose party is not in this answer keeps empty party cells rather
 * than a guessed one.
 */
async function readPartyNames(session: LiveSession): Promise<ReadonlyMap<string, PartyName>> {
    const parties = await readMyParties(session.client);
    return new Map(
        parties.parties.map((party) => [
            party.partyId,
            { name: party.name, shortCode: party.shortCode },
        ]),
    );
}

/** A refused read or write, as the status that says which. */
function refusalOf(result: z.infer<typeof resultSchema>): HttpFailure {
    switch (result.outcome) {
        case 'denied':
            return new HttpFailure(403, { code: 'forbidden', message: result.message });
        case 'missing':
            return notFound(result.message);
        case 'conflict':
            return new HttpFailure(409, { code: 'conflict', message: result.message });
        case 'unavailable':
        case 'failed':
            return new HttpFailure(502, { code: 'upstream-unavailable', message: result.message });
        default:
            return invalidRequest(result.message);
    }
}

/**
 * The health word of one row.
 *
 * A definition with no run is the one word its absence states, and it is not
 * clean: nothing ran and nothing was checked. When the latest-run read itself
 * failed, the absence belongs to the read and not to the definition, so the word
 * is absent too and the page's unread entry names the read that refused.
 */
function healthOfRow(
    run: Record<string, unknown> | undefined,
    stated: { health: HealthWord | null } | undefined,
    runs: ReadonlyMap<string, Record<string, unknown>> | undefined,
): HealthWord | null {
    if (run === undefined) {
        return runs === undefined ? null : 'never run';
    }
    return stated?.health ?? null;
}

/** One definition and everything the list says about it. */
function rowOf(
    definition: Record<string, unknown>,
    decorated: {
        /** Undefined when the read that names the parties failed. */
        readonly parties: ReadonlyMap<string, PartyName> | undefined;
        /** Undefined when the latest runs could not be read. */
        readonly runs: ReadonlyMap<string, Record<string, unknown>> | undefined;
        readonly health: ReadonlyMap<string, { health: HealthWord | null; stateName: string }>;
        /** Undefined when the problem counts could not be read. */
        readonly counts: ReadonlyMap<string, { errors: number; warnings: number }> | undefined;
        /** Undefined when the next fire could not be read. */
        readonly nextFires: ReadonlyMap<string, string | null> | undefined;
    },
): ReportRow {
    const id = text(definition['id']);
    const partyId = text(definition['party_id']);
    const run = decorated.runs?.get(id);
    const runId = run === undefined ? '' : text(run['id']);
    const stated = decorated.health.get(runId);
    const counted = decorated.counts?.get(runId);
    const party = decorated.parties?.get(partyId);

    return {
        id,
        name: text(definition['name']),
        reportType: text(definition['report_type']),
        partyId,
        partyName: party?.name ?? '',
        partyCode: party === undefined || party.shortCode === '' ? null : party.shortCode,
        scheduleExpression: text(definition['schedule_expression']),
        health: healthOfRow(run, stated, decorated.runs),
        stateName: stated?.stateName ?? '',
        latestRun:
            run === undefined
                ? null
                : {
                      id: runId,
                      startedAt: nullableText(run['started_at']),
                      completedAt: nullableText(run['completed_at']),
                      // A run with no entry recorded no problem, so the absence
                      // is the screen's zero.
                      errors: counted?.errors ?? (decorated.counts === undefined ? null : 0),
                      warnings: counted?.warnings ?? (decorated.counts === undefined ? null : 0),
                  },
        nextFireAt: decorated.nextFires?.get(id) ?? null,
    };
}

/**
 * The rows of the definitions named, with every cell the server can supply.
 *
 * The independent decodings are read at once, and the two that need a run's id
 * follow the latest-run read. A decoration that fails is absent from the rows
 * and present in `unread`, so a screen draws those cells empty and says which
 * read refused it. The parties are returned as well, because the definition's
 * own record states its party by the same read.
 */
async function compose(
    session: LiveSession,
    definitions: readonly Record<string, unknown>[],
): Promise<{
    rows: ReportRow[];
    unread: Unread[];
    parties: ReadonlyMap<string, PartyName> | undefined;
}> {
    const unread: Unread[] = [];
    const ids = definitions.map((definition) => text(definition['id'])).filter((id) => id !== '');
    if (ids.length === 0) {
        return { rows: [], unread, parties: new Map() };
    }

    const [parties, runs, nextFires] = await Promise.all([
        decorate('parties', unread, () => readPartyNames(session)),
        decorate('latestRun', unread, () => readLatestRuns(session, ids)),
        decorate('nextFire', unread, () => readNextFires(session, ids)),
    ]);

    const runIds = (runs ?? []).map((run) => text(run['id'])).filter((id) => id !== '');
    let health: ReadonlyMap<string, { health: HealthWord | null; stateName: string }> = new Map();
    let counts: ReadonlyMap<string, { errors: number; warnings: number }> | undefined;
    if (runIds.length > 0) {
        [health, counts] = await Promise.all([
            decorate('health', unread, () => readHealth(session, runIds)).then(
                (read) => read ?? new Map(),
            ),
            decorate('errors', unread, () => readProblemCounts(session, runIds)),
        ]);
    }

    const byDefinition = new Map(
        (runs ?? []).map((run) => [text(run['definition_id']), run] as const),
    );

    const rows = definitions
        .map((definition) =>
            rowOf(definition, {
                parties,
                runs: runs === undefined ? undefined : byDefinition,
                health,
                counts,
                nextFires,
            }),
        )
        .sort((left, right) => left.name.localeCompare(right.name));

    return { rows, unread, parties };
}

/**
 * The definition as the record page reads it, under the names the record holds.
 *
 * It is a record row, so the shared record parts draw and edit it: the field
 * table names these fields, and the server's history names the same ones. The
 * party is stated by its name and its code, because a screen never shows an
 * identifier where a name would do.
 */
function definitionRowOf(
    definition: Record<string, unknown>,
    parties: ReadonlyMap<string, PartyName> | undefined,
): Record<string, unknown> {
    const partyId = text(definition['party_id']);
    const party = parties?.get(partyId);
    return {
        id: text(definition['id']),
        name: text(definition['name']),
        description: text(definition['description']),
        report_type: text(definition['report_type']),
        party_id: partyId,
        party_name: party?.name ?? '',
        party_code: party === undefined || party.shortCode === '' ? null : party.shortCode,
        schedule_expression: text(definition['schedule_expression']),
        concurrency_policy: text(definition['concurrency_policy']),
        is_official: definition['is_official'] === true,
        pre_processing: text(definition['pre_processing']),
        prepared_input_key: text(definition['prepared_input_key']),
        post_processing: text(definition['post_processing']),
        version: countOf(definition['version']),
        modified_by: text(definition['modified_by']),
        performed_by: text(definition['performed_by']),
        change_reason_code: text(definition['change_reason_code']),
        change_commentary: text(definition['change_commentary']),
        recorded_at: text(definition['recorded_at']),
    };
}

/** The fields the edit form offers, and the only ones a save may change. */
const editSchema = z.object({
    description: z.string().max(2000),
    schedule_expression: z.string().trim().min(1).max(200),
    is_official: z.boolean(),
    pre_processing: z.string().max(2000),
    prepared_input_key: z.string().max(500),
    post_processing: z.string().max(2000),
});

const editBodySchema = z.object({
    version: z.int().nonnegative(),
    change: editSchema,
    intent: intentSchema,
});

/**
 * The write the edit form makes, rebuilt from the read.
 *
 * The form offers six fields and the record has eighteen, so the write is
 * assembled from the definition just read and only the offered fields are
 * replaced. A save therefore cannot blank a field the screen does not show,
 * whatever the browser sends. The version is the one the form was drawn from,
 * so a record somebody else changed in between is refused instead of being
 * overwritten unseen.
 */
function writeOf(
    definition: Record<string, unknown>,
    change: z.infer<typeof editSchema>,
): Record<string, unknown> {
    return {
        id: text(definition['id']),
        name: text(definition['name']),
        party_id: text(definition['party_id']),
        description: change.description,
        report_type: text(definition['report_type']),
        fsm_state_id: nullableText(definition['fsm_state_id']),
        schedule_expression: change.schedule_expression,
        concurrency_policy: text(definition['concurrency_policy']),
        scheduler_job_id: nullableText(definition['scheduler_job_id']),
        run_grant_id: nullableText(definition['run_grant_id']),
        pre_processing: change.pre_processing,
        prepared_input_key: change.prepared_input_key,
        post_processing: change.post_processing,
        is_official: change.is_official,
    };
}

/**
 * One definition with the row the list would draw for it, and the reads that
 * failed, so the detail screen can say that a run could not be checked rather
 * than that the definition never ran.
 */
async function detailOf(
    session: LiveSession,
    definition: Record<string, unknown>,
): Promise<{ report: ReportRow; definition: Record<string, unknown>; unread: Unread[] }> {
    const composed = await compose(session, [definition]);
    const [report] = composed.rows;
    if (report === undefined) {
        throw notFound(ABSENT);
    }
    return {
        report,
        definition: definitionRowOf(definition, composed.parties),
        unread: composed.unread,
    };
}

/**
 * The reporting routes the list and the detail screen call.
 *
 * Party scope is the session's throughout: the definitions read is scoped by
 * row-level security on the caller's own parties, and the party filter narrows
 * that scope to one of them. A party the caller does not work in matches no row.
 */
export function registerReportingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/reporting/reports', async (request) => {
        const session = requireSession(request);
        const page = parseInput(
            pageQuerySchema,
            request.query,
            'A page names an offset, a limit of 1 to 1000, an order and a party.',
        );
        const read = await readDefinitionsPage(session, {
            offset: page.offset,
            limit: page.limit,
            sort: page.sort,
            descending: page.descending,
            partyId: page.party_id ?? null,
        });
        const composed = await compose(session, read.definitions);
        return { rows: composed.rows, total: read.total, unread: composed.unread };
    });

    /*
     * The retired name-based address. It answers with the id of the one
     * definition that carries the name, and refuses with the choices when the
     * name is shared, so a saved link still arrives while a shared name is never
     * guessed. The page itself calls the address route below, which answers a
     * name with the whole record; this stays as the cheap lookup for a caller
     * that only wants the id.
     */
    server.get('/api/reporting/reports/by-name/:name', async (request) => {
        const session = requireSession(request);
        const name = parseInput(
            nameSchema,
            (request.params as { name: string }).name,
            'A report is named by a name of 1 to 200 characters.',
        );
        const resolved = await resolveName(session, name);
        switch (resolved.kind) {
            case 'one':
                return { id: text(resolved.definition['id']) };
            case 'many':
                throw sharedName(name, resolved.choices);
            case 'none':
                throw notFound(ABSENT);
        }
    });

    /*
     * One definition's page, by whatever the address segment is: an id opens
     * that record, and anything else is the name an older address saved. A name
     * that one definition carries answers with that record, so the page lands on
     * it and the browser replaces the address with the id. A shared name answers
     * with the choices, because a person cannot be told to find an id they were
     * never shown.
     */
    server.get('/api/reporting/reports/:segment', async (request) => {
        const session = requireSession(request);
        const segment = (request.params as { segment: string }).segment;
        if (idSchema.safeParse(segment).success) {
            return detailOf(session, await readDefinition(session, segment));
        }
        const resolved = await resolveName(session, segment);
        switch (resolved.kind) {
            case 'one':
                return detailOf(session, resolved.definition);
            case 'many':
                throw sharedName(segment, resolved.choices);
            case 'none':
                throw notFound(ABSENT);
        }
    });

    server.put('/api/reporting/reports/:id', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const body = parseInput(
            editBodySchema,
            request.body,
            'A write names the version it was read at, the fields it changes and its reason.',
        );
        const definition = await readDefinition(session, id);
        const reply = await session.client.callAuthenticated(
            definitionSubjects.put_report_definition_request,
            {
                change: {
                    write: writeOf(definition, body.change),
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
            z.looseObject({
                result: resultSchema,
                report_definition: row.nullable().default(null),
            }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusalOf(reply.result);
        }
        return detailOf(session, reply.report_definition ?? (await readDefinition(session, id)));
    });
}
