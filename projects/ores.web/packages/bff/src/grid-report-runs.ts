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
    gridReportRunViewSchema,
    readBatches,
    reportHealthWordSchema,
    type GridReportRunView,
} from '@ores/wire-protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_definition_protocol';
import { subjects as instanceSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_instance_protocol';
import { subjects as errorSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_error_event_protocol';
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

async function readRun(session: LiveSession, id: string): Promise<Record<string, unknown> | null> {
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
                filter: { party_id: null, id_one_of: [id], party_id_one_of: null, search: null },
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

async function readHealth(
    session: LiveSession,
    id: string,
    log: FastifyBaseLogger,
): Promise<{ health: GridReportRunView['health']; stateName: string }> {
    try {
        const reply = await session.client.callAuthenticated(
            instanceSubjects.get_report_instance_health_request,
            { instance_ids: [id] },
            z.looseObject({ result: resultSchema, health: z.array(row).default([]) }),
        );
        const entry = reply.result.outcome === 'ok' ? reply.health[0] : undefined;
        return {
            health: reportHealthWordSchema.safeParse(entry?.['health']).data ?? null,
            stateName: text(entry?.['state_name']),
        };
    } catch (error) {
        log.warn({ err: error }, 'The health of the report run could not be read');
        return { health: null, stateName: '' };
    }
}

/**
 * The batch that names the run in its External Ref, when the session sees one.
 *
 * The search matches text, so the page of 5 is narrowed by an exact match. A run id is
 * a uuid, so a second batch that merely contains it is not expected.
 */
async function readBatchId(
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

export function registerGridReportRunRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get(
        '/api/operations/grid/report-runs/:id/errors',
        async (request): Promise<{ total: number; errors: readonly unknown[] }> => {
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
                errors: reply.errors.map((entry) => ({
                    id: text(entry['id']),
                    scope: text(entry['scope_code']),
                    severity: text(entry['severity']),
                    message: text(entry['message']),
                    subject: nullableText(entry['subject']),
                    subject_id: nullableText(entry['subject_id']),
                    cause: nullableText(entry['cause']),
                    subject_version:
                        typeof entry['subject_version'] === 'number'
                            ? entry['subject_version']
                            : null,
                    subject_as_of: nullableText(entry['subject_as_of']),
                    time: text(entry['event_time']),
                })),
            };
        },
    );
    server.get(
        '/api/operations/grid/report-runs/:id',
        async (request): Promise<GridReportRunView> => {
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
            return gridReportRunViewSchema.parse({
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
                started_at: nullableText(run['started_at']),
                completed_at: nullableText(run['completed_at']),
                priced_at: nullableText(run['priced_at']),
                price_note: text(run['price_note']),
                batch_id: batchId,
                modified_by: text(run['modified_by']),
                recorded_at: text(run['recorded_at']),
            });
        },
    );
}
