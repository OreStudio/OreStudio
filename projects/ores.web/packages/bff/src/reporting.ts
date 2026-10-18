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
import { input, intentSchema, readAll, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The reporting routes: the reports a party runs, one row per definition, and
 * one definition read and edited.
 *
 * The server answers the definitions, each definition's latest run, the health
 * word of that run, its problem counts, its next fire and the party's short
 * code as six separate reads. Composing them here rather than in the browser is
 * what lets the list degrade one cell at a time: the row is a definition, and
 * everything else on it is a decoration that can be absent and say why.
 */

/** The words a row's health cell can hold. The server derives them; this only names them. */
const HEALTH_WORDS = [
    'failed',
    'completed with errors',
    'running',
    'never run',
    'completed clean',
] as const;

type HealthWord = (typeof HEALTH_WORDS)[number];

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
    readonly name: string;
    readonly reportType: string;
    readonly partyId: string;
    readonly partyCode: string | null;
    readonly scheduleExpression: string;
    readonly health: HealthWord | null;
    readonly stateName: string;
    readonly latestRun: RunView | null;
    readonly nextFireAt: string | null;
}

interface Tally {
    readonly failed: number;
    readonly completedWithErrors: number;
    readonly running: number;
    readonly neverRun: number;
    readonly completedClean: number;
    readonly unstated: number;
}

/** The definition as the detail screen reads it. */
interface DefinitionView {
    readonly name: string;
    readonly description: string;
    readonly reportType: string;
    readonly partyId: string;
    readonly scheduleExpression: string;
    readonly concurrencyPolicy: string;
    readonly isOfficial: boolean;
    readonly preProcessing: string;
    readonly preparedInputKey: string;
    readonly postProcessing: string;
    readonly schedulerJobId: string | null;
    readonly runGrantId: string | null;
    readonly fsmStateId: string | null;
    readonly version: number;
    readonly modifiedBy: string;
    readonly performedBy: string;
    readonly changeReasonCode: string;
    readonly changeCommentary: string;
    readonly recordedAt: string;
}

const reportNameSchema = z.string().trim().min(1).max(200);

const nameParam = (request: FastifyRequest): string =>
    input(reportNameSchema, (request.params as { name: string }).name);

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

/** The definitions the session's parties own, page by page. */
async function readDefinitions(session: LiveSession): Promise<readonly Record<string, unknown>[]> {
    return readAll(session, definitionSubjects.list_report_definitions_request, 'definitions', {
        filter: null,
        as_of: null,
    });
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
 * The short code of each party the caller's own account works in.
 *
 * The session carries the party's name and no code (G11), so the code comes
 * from the account's own parties and from nowhere else. A row whose party is not
 * in this answer keeps an empty Party cell rather than a guessed one.
 */
async function readPartyCodes(session: LiveSession): Promise<ReadonlyMap<string, string>> {
    const parties = await readMyParties(session.client);
    return new Map(
        parties.parties
            .filter((party) => party.shortCode !== '')
            .map((party) => [party.partyId, party.shortCode]),
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

/** One definition and everything the list says about it. */
function rowOf(
    definition: Record<string, unknown>,
    decorated: {
        /** Undefined when the read that names the parties failed. */
        readonly partyCodes: ReadonlyMap<string, string> | undefined;
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

    return {
        name: text(definition['name']),
        reportType: text(definition['report_type']),
        partyId,
        partyCode: decorated.partyCodes?.get(partyId) ?? null,
        scheduleExpression: text(definition['schedule_expression']),
        // A definition with no run is the one word its absence states, and it is
        // not clean: nothing ran and nothing was checked.
        health: run === undefined ? 'never run' : (stated?.health ?? null),
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

function tallyOf(rows: readonly ReportRow[]): Tally {
    const tally = {
        failed: 0,
        completedWithErrors: 0,
        running: 0,
        neverRun: 0,
        completedClean: 0,
        unstated: 0,
    };
    for (const row of rows) {
        switch (row.health) {
            case 'failed':
                tally.failed += 1;
                break;
            case 'completed with errors':
                tally.completedWithErrors += 1;
                break;
            case 'running':
                tally.running += 1;
                break;
            case 'never run':
                tally.neverRun += 1;
                break;
            case 'completed clean':
                tally.completedClean += 1;
                break;
            default:
                tally.unstated += 1;
        }
    }
    return tally;
}

/**
 * The rows of the definitions named, with every cell the server can supply.
 *
 * The independent decodings are read at once, and the two that need a run's id
 * follow the latest-run read. A decoration that fails is absent from the rows
 * and present in `unread`, so a screen draws those cells empty and says which
 * read refused it.
 */
async function compose(
    session: LiveSession,
    definitions: readonly Record<string, unknown>[],
): Promise<{ rows: ReportRow[]; tally: Tally; unread: Unread[] }> {
    const unread: Unread[] = [];
    const ids = definitions.map((definition) => text(definition['id'])).filter((id) => id !== '');
    if (ids.length === 0) {
        return { rows: [], tally: tallyOf([]), unread };
    }

    const [partyCodes, runs, nextFires] = await Promise.all([
        decorate('parties', unread, () => readPartyCodes(session)),
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
                partyCodes,
                runs: runs === undefined ? undefined : byDefinition,
                health,
                counts,
                nextFires,
            }),
        )
        .sort((left, right) => left.name.localeCompare(right.name));

    return { rows, tally: tallyOf(rows), unread };
}

/** The definition the address names, as the read returns it. */
async function readDefinition(
    session: LiveSession,
    name: string,
): Promise<Record<string, unknown>> {
    const reply = await session.client.callAuthenticated(
        definitionSubjects.get_report_definition_request,
        { key: { name } },
        z.looseObject({ result: resultSchema, report_definition: row.nullable().default(null) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    if (reply.report_definition === null) {
        throw notFound(`No report definition is named ${name}.`);
    }
    return reply.report_definition;
}

function definitionOf(definition: Record<string, unknown>): DefinitionView {
    return {
        name: text(definition['name']),
        description: text(definition['description']),
        reportType: text(definition['report_type']),
        partyId: text(definition['party_id']),
        scheduleExpression: text(definition['schedule_expression']),
        concurrencyPolicy: text(definition['concurrency_policy']),
        isOfficial: definition['is_official'] === true,
        preProcessing: text(definition['pre_processing']),
        preparedInputKey: text(definition['prepared_input_key']),
        postProcessing: text(definition['post_processing']),
        schedulerJobId: nullableText(definition['scheduler_job_id']),
        runGrantId: nullableText(definition['run_grant_id']),
        fsmStateId: nullableText(definition['fsm_state_id']),
        version: countOf(definition['version']),
        modifiedBy: text(definition['modified_by']),
        performedBy: text(definition['performed_by']),
        changeReasonCode: text(definition['change_reason_code']),
        changeCommentary: text(definition['change_commentary']),
        recordedAt: text(definition['recorded_at']),
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

/** One definition with the row the list would draw for it. */
async function detailOf(
    session: LiveSession,
    definition: Record<string, unknown>,
): Promise<{ report: ReportRow; definition: DefinitionView }> {
    const composed = await compose(session, [definition]);
    const [report] = composed.rows;
    if (report === undefined) {
        throw notFound('The report definition could not be composed.');
    }
    return { report, definition: definitionOf(definition) };
}

/**
 * The reporting routes the list and the detail screen call.
 *
 * Party scope is the session's throughout: the definitions read is scoped by
 * row-level security on the caller's own parties, and no request field names a
 * party.
 */
export function registerReportingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/reporting/reports', async (request) => {
        const session = requireSession(request);
        return compose(session, await readDefinitions(session));
    });

    server.get('/api/reporting/reports/:name', async (request) => {
        const session = requireSession(request);
        return detailOf(session, await readDefinition(session, nameParam(request)));
    });

    server.put('/api/reporting/reports/:name', async (request) => {
        const session = requireSession(request);
        const name = nameParam(request);
        const body = input(editBodySchema, request.body);
        const definition = await readDefinition(session, name);
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
        return detailOf(session, reply.report_definition ?? (await readDefinition(session, name)));
    });
}
