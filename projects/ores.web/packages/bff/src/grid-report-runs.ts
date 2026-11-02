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

import type { FastifyBaseLogger, FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    readBatches,
    reportHealthWordSchema,
    reportRunErrorViewSchema,
    reportRunRowViewSchema,
    reportRunDetailViewSchema,
    type ReportRunErrorView,
    type ReportRunRowView,
    type ReportRunDetailView,
} from '@ores/wire-protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_definition_protocol';
import { subjects as instanceSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_instance_protocol';
import { subjects as errorSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_error_event_protocol';
import { subjects as operationsSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_operations_protocol';
import { invalidRequest, notFound } from './errors.js';
import { NO_ORDER, refusal, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The report run route: the run of a report definition that dispatched a batch.
 *
 * The read is the reporting service's own, on the session's own client, so the
 * row-level policy of the run's party decides what the session sees. A run the
 * session may not see answers the same 404 as a run that does not exist. The health
 * word and the batch are decorations: a failed read of either leaves the cell empty
 * and does not fail the page.
 */

const idParamsSchema = z.object({ id: z.uuid() });

/** The runs of the ids named that the session may read. */
async function readRuns(
    session: LiveSession,
    ids: readonly string[],
): Promise<readonly Record<string, unknown>[]> {
    const reply = await session.client.callAuthenticated(
        instanceSubjects.list_report_instances_request,
        {
            filter: {
                definition_id: null,
                kind: null,
                id_one_of: [...ids],
                definition_id_one_of: null,
                cut_id: null,
                cut_id_one_of: null,
                kind_one_of: null,
            },
            as_of: null,
            offset: 0,
            limit: ids.length,
            order: NO_ORDER,
        },
        z.looseObject({ result: resultSchema, instances: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    return reply.instances;
}

/**
 * Runs a report definition on demand.
 *
 * The trigger belongs to the scheduler's own operation, so the BFF sends it the
 * request the scheduler sends: the definition and the tenant that owns it. The
 * handler applies the definition's concurrency policy, creates the run and
 * dispatches the workflow, and answers the run it created.
 */
export async function triggerReportRun(
    session: LiveSession,
    definitionId: string,
): Promise<string> {
    const reply = await session.client.callAuthenticated(
        operationsSubjects.trigger_report_instance_request,
        {
            report_definition_id: definitionId,
            tenant_id: session.tenantId,
            job_instance_id: 0,
            kind: 'production',
        },
        z.looseObject({
            result: resultSchema,
            report_instance_id: z.string().default(''),
        }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    return reply.report_instance_id;
}

export async function readRun(
    session: LiveSession,
    id: string,
): Promise<Record<string, unknown> | null> {
    return (await readRuns(session, [id]))[0] ?? null;
}

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * The name of each report run that the references name, for the references that are run
 * ids the session may read. A reference that is text, or a run the session cannot see, is
 * left out. A failed read leaves every name out and does not fail the page.
 */
export async function reportRunNames(
    session: LiveSession,
    references: readonly string[],
    log: FastifyBaseLogger,
): Promise<ReadonlyMap<string, string>> {
    const ids = [...new Set(references.filter((reference) => UUID.test(reference)))];
    if (ids.length === 0) {
        return new Map();
    }
    try {
        const runs = await readRuns(session, ids);
        return new Map(runs.map((run) => [text(run['id']).toLowerCase(), text(run['name'])]));
    } catch (error) {
        log.warn({ err: error }, 'The names of the report runs could not be read');
        return new Map();
    }
}

/** The name of a report definition, or nothing when the session cannot read it. */
async function readDefinitionName(
    session: LiveSession,
    id: string,
    log: FastifyBaseLogger,
): Promise<string | null> {
    try {
        const reply = await session.client.callAuthenticated(
            definitionSubjects.list_report_definitions_request,
            {
                filter: {
                    party_id: null,
                    id_one_of: [id],
                    party_id_one_of: null,
                    cut_kind_code: null,
                    cut_kind_code_one_of: null,
                    search: null,
                },
                as_of: null,
                offset: 0,
                limit: 1,
                order: NO_ORDER,
            },
            z.looseObject({ result: resultSchema, definitions: z.array(row).default([]) }),
        );
        const name = text(reply.definitions[0]?.['name']);
        return reply.result.outcome === 'ok' && name !== '' ? name : null;
    } catch (error) {
        log.warn({ err: error }, 'The name of the report definition could not be read');
        return null;
    }
}

/** The health of one run; a failed read leaves it unknown and does not fail the page. */
async function readHealth(
    session: LiveSession,
    id: string,
    log: FastifyBaseLogger,
): Promise<RunHealth> {
    try {
        return (await readRunHealth(session, [id])).get(id) ?? NO_HEALTH;
    } catch (error) {
        log.warn({ err: error }, 'The health of the report run could not be read');
        return NO_HEALTH;
    }
}

/**
 * The batch that names the run in its External Ref, when the session sees one.
 *
 * The search matches text, so the page of 5 is narrowed by an exact match. A run id is
 * a uuid, so a second batch that merely contains it is not expected.
 */
export async function readBatchId(
    session: LiveSession,
    id: string,
    log: FastifyBaseLogger,
): Promise<string | null> {
    try {
        const { batches } = await readBatches(
            session.client,
            { offset: 0, limit: 5 },
            { find: id },
        );
        return batches.find((batch) => batch.external_ref === id)?.id ?? null;
    } catch (error) {
        log.warn({ err: error }, 'The batch of the report run could not be read');
        return null;
    }
}

const nullableText = (value: unknown): string | null => {
    const stated = text(value);
    return stated === '' ? null : stated;
};

/**
 * The runs of one definition, newest first, each with the state it reached.
 *
 * The list carries no order of its own, so the run with the latest start leads,
 * and a report with more runs than the page holds shows its newest ones rather
 * than an arbitrary slice.
 */
async function readRunsOfDefinition(
    session: LiveSession,
    definitionId: string,
): Promise<readonly ReportRunRowView[]> {
    const reply = await session.client.callAuthenticated(
        instanceSubjects.list_report_instances_request,
        {
            filter: {
                definition_id: definitionId,
                kind: null,
                id_one_of: null,
                definition_id_one_of: null,
                cut_id: null,
                cut_id_one_of: null,
                kind_one_of: null,
            },
            as_of: null,
            offset: 0,
            limit: RUNS_OF_REPORT_LIMIT,
            order: NO_ORDER,
        },
        z.looseObject({ result: resultSchema, instances: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    const runs = [...reply.instances].sort((left, right) =>
        text(right['started_at']).localeCompare(text(left['started_at'])),
    );
    const health = await readRunHealth(
        session,
        runs.map((run) => text(run['id'])),
    );
    return runs.map((run) => {
        const id = text(run['id']);
        const started = nullableText(run['started_at']);
        const completed = nullableText(run['completed_at']);
        const word = health.get(id) ?? NO_HEALTH;
        return reportRunRowViewSchema.parse({
            id,
            name: text(run['name']),
            state_name: word.stateName,
            health: word.health,
            started_at: started,
            completed_at: completed,
            duration_ms: durationMs(started, completed),
            priced_at: nullableText(run['priced_at']),
        });
    });
}

/** The health word a run's state reads as, and the state's own name. */
interface RunHealth {
    readonly health: ReportRunDetailView['health'];
    readonly stateName: string;
}

const NO_HEALTH: RunHealth = { health: null, stateName: '' };

/** The health of each run, for the runs whose health could be read. */
async function readRunHealth(
    session: LiveSession,
    instanceIds: readonly string[],
): Promise<ReadonlyMap<string, RunHealth>> {
    if (instanceIds.length === 0) {
        return new Map();
    }
    const reply = await session.client.callAuthenticated(
        instanceSubjects.get_report_instance_health_request,
        { instance_ids: [...instanceIds] },
        z.looseObject({ result: resultSchema, health: z.array(row).default([]) }),
    );
    if (reply.result.outcome !== 'ok') {
        return new Map();
    }
    return new Map(
        reply.health.map((entry) => [
            text(entry['report_instance_id']),
            {
                health: reportHealthWordSchema.safeParse(entry['health']).data ?? null,
                stateName: text(entry['state_name']),
            },
        ]),
    );
}

/** The state name each run reached, for the runs whose health could be read. */
export async function readRunStates(
    session: LiveSession,
    instanceIds: readonly string[],
): Promise<ReadonlyMap<string, string>> {
    const health = await readRunHealth(session, instanceIds);
    return new Map([...health].map(([id, word]) => [id, word.stateName]));
}

/** The time from a run's start to its completion, or null until both are known. */
function durationMs(started: string | null, completed: string | null): number | null {
    if (started === null || completed === null) {
        return null;
    }
    const elapsed = Date.parse(completed) - Date.parse(started);
    return Number.isNaN(elapsed) || elapsed < 0 ? null : elapsed;
}

/**
 * The one thing a problem names, as a person reads it: the curve a curve failure
 * is about, otherwise the record the engine named.
 */
function aboutOf(entry: Record<string, unknown>): string {
    if (text(entry['scope_code']) === 'Curve' && text(entry['cause']) !== '') {
        return text(entry['cause']);
    }
    return text(entry['subject_id']) || text(entry['cause']);
}

/** How many of a report's runs a page asks for. */
const RUNS_OF_REPORT_LIMIT = 200;

export function registerGridReportRunRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Starts a run of the report definition, and answers the run it created. */
    server.post('/api/reporting/reports/:id/run', async (request): Promise<{ runId: string }> => {
        const session = requireSession(request);
        const parsed = idParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address names an id that is not a uuid.');
        }
        const runId = await triggerReportRun(session, parsed.data.id);
        return { runId };
    });

    /** The runs of one report, newest first, with the state each reached. */
    server.get(
        '/api/reporting/reports/:id/runs',
        async (request): Promise<{ runs: readonly ReportRunRowView[] }> => {
            const session = requireSession(request);
            const parsed = idParamsSchema.safeParse(request.params);
            if (!parsed.success) {
                throw invalidRequest('The address names an id that is not a uuid.');
            }
            const runs = await readRunsOfDefinition(session, parsed.data.id);
            return { runs };
        },
    );

    /** The problems one run recorded, one row each. */
    server.get(
        '/api/reporting/runs/:id/errors',
        async (request): Promise<{ total: number; errors: readonly ReportRunErrorView[] }> => {
            const session = requireSession(request);
            const parsed = idParamsSchema.safeParse(request.params);
            if (!parsed.success) {
                throw invalidRequest('The address names an id that is not a uuid.');
            }
            const reply = await session.client.callAuthenticated(
                errorSubjects.list_by_report_instance_id_report_error_events_request,
                {
                    report_instance_id: parsed.data.id,
                    scope: 'direct',
                    offset: 0,
                    limit: 500,
                    order: { field: '', descending: false },
                    filter: null,
                },
                z.looseObject({
                    result: resultSchema,
                    errors: z.array(row).default([]),
                    total: z.number().default(0),
                }),
            );
            if (reply.result.outcome !== 'ok') {
                throw refusal(reply.result);
            }
            return {
                total: reply.total,
                errors: reply.errors.map((entry) =>
                    reportRunErrorViewSchema.parse({
                        id: text(entry['id']),
                        severity: text(entry['severity']),
                        scope: text(entry['scope_code']),
                        about: aboutOf(entry),
                        message: text(entry['message']),
                        subject: nullableText(entry['subject']),
                        subject_id: nullableText(entry['subject_id']),
                        cause: nullableText(entry['cause']),
                        time: text(entry['event_time']),
                    }),
                ),
            };
        },
    );
    /** One run: what it is, the state it reached, its batch and where its output is. */
    server.get(
        '/api/reporting/runs/:id',
        async (request): Promise<ReportRunDetailView> => {
            const session = requireSession(request);
            const parsed = idParamsSchema.safeParse(request.params);
            if (!parsed.success) {
                throw invalidRequest('The address names an id that is not a uuid.');
            }
            const id = parsed.data.id;
            const run = await readRun(session, id);
            if (run === null) {
                throw notFound('No report run has this id.');
            }
            const [health, batchId, definitionName] = await Promise.all([
                readHealth(session, id, request.log),
                readBatchId(session, id, request.log),
                readDefinitionName(session, text(run['definition_id']), request.log),
            ]);
            const started = nullableText(run['started_at']);
            const completed = nullableText(run['completed_at']);
            return reportRunDetailViewSchema.parse({
                id: text(run['id']),
                name: text(run['name']),
                description: text(run['description']),
                definition_id: text(run['definition_id']),
                definition_name: definitionName,
                party_id: text(run['party_id']),
                party_name:
                    session.availableParties.find((party) => party.id === text(run['party_id']))
                        ?.name ?? null,
                kind: text(run['kind']),
                state_name: health.stateName,
                health: health.health,
                output_message: text(run['output_message']),
                output_storage_key: text(run['output_storage_key']),
                started_at: started,
                completed_at: completed,
                duration_ms: durationMs(started, completed),
                priced_at: nullableText(run['priced_at']),
                price_note: text(run['price_note']),
                batch_id: batchId,
                modified_by: text(run['modified_by']),
                recorded_at: text(run['recorded_at']),
            });
        },
    );
}
