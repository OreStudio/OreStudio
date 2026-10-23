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
 */

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import type { Batch } from './generated/compute/domain/batch.js';
import type { Result } from './generated/compute/domain/result.js';
import type { Workunit } from './generated/compute/domain/workunit.js';
import {
    subjects as batchSubjects,
    type ListBatchesRequest,
    type ListBatchesResponse,
} from './generated/compute/protocol/batch_protocol.js';
import {
    subjects as resultSubjects,
    type GetResultRequest,
    type GetResultResponse,
    type ListByWorkunitIdResultsRequest,
    type ListByWorkunitIdResultsResponse,
    type ListResultsRequest,
    type ListResultsResponse,
} from './generated/compute/protocol/result_protocol.js';
import {
    subjects as workunitSubjects,
    type GetWorkunitRequest,
    type GetWorkunitResponse,
    type ListByBatchIdWorkunitsRequest,
    type ListByBatchIdWorkunitsResponse,
} from './generated/compute/protocol/workunit_protocol.js';
import { decidedResultSchema, orderSchema } from './operations.js';
import { nullableWireTimestampSchema, wireTimestampSchema } from './time.js';

/**
 * The batches, workunits and results of the compute grid, as the Jobs tab and
 * the job page read them.
 *
 * Every read is the entity's own generated read, so the compute service checks
 * the entity's read code and the row-level policy narrows what the session
 * sees. Nothing here widens a read or names a tenant.
 *
 * The service states an unset value in the record's own type: the nil uuid
 * for an id, an empty string for text, and the epoch for an instant. A view
 * states none of these. It states `null`, so a screen cannot read an unset
 * value as a real one.
 */

const text = z.string().default('');
const NIL_UUID = '00000000-0000-0000-0000-000000000000';
const EPOCH = '1970-01-01 00:00:00.000000Z';

const batchSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    external_ref: text,
    party_id: z.string().nullable().default(null),
    status: text,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<Batch>;

const workunitSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    party_id: z.string().nullable().default(null),
    batch_id: z.string(),
    app_version_id: z.string(),
    input_uri: text,
    config_uri: text,
    priority: z.int().default(0),
    target_redundancy: z.int().default(1),
    canonical_result_id: text,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<Workunit>;

const resultSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    party_id: z.string().nullable().default(null),
    workunit_id: z.string(),
    host_id: text,
    server_state: z.int().default(0),
    outcome: z.int().default(0),
    output_uri: text,
    error_message: text,
    received_at: wireTimestampSchema,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<Result>;

const pageFields = {
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
};

/**
 * `list_batches_request`, sent on `compute.v1.batches.list`.
 *
 * An empty order field is the model's default order, which for batches is
 * the recording time, newest first. A batch's key is a random identifier and
 * the key only breaks ties, so a page is stable and reads in recording order.
 */
export const listBatchesRequestSchema = z.object({
    ...pageFields,
    filter: z
        .object({
            party_id: z.string().nullable().default(null),
            status: z.string().nullable().default(null),
            id_one_of: z.array(z.string()).nullable().default(null),
            party_id_one_of: z.array(z.string()).nullable().default(null),
            status_one_of: z.array(z.string()).nullable().default(null),
            search: z.string().nullable().default(null),
        })
        .nullable()
        .default(null),
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<ListBatchesRequest>;

export const listBatchesReplySchema = z.object({
    result: decidedResultSchema,
    batches: z.array(batchSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListBatchesResponse>;

/** `list_by_batch_id_workunits_request`, sent on `compute.v1.workunits.list_by_batch_id`. */
export const listWorkunitsByBatchRequestSchema = z.object({
    batch_id: z.string(),
    scope: z.enum(['direct', 'subtree']).default('direct'),
    ...pageFields,
    filter: z.null().default(null),
}) satisfies z.ZodType<ListByBatchIdWorkunitsRequest>;

export const listWorkunitsByBatchReplySchema = z.object({
    result: decidedResultSchema,
    workunits: z.array(workunitSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListByBatchIdWorkunitsResponse>;

/** `list_by_workunit_id_results_request`, sent on `compute.v1.results.list_by_workunit_id`. */
export const listResultsByWorkunitRequestSchema = z.object({
    workunit_id: z.string(),
    scope: z.enum(['direct', 'subtree']).default('direct'),
    ...pageFields,
    filter: z.null().default(null),
}) satisfies z.ZodType<ListByWorkunitIdResultsRequest>;

export const listResultsByWorkunitReplySchema = z.object({
    result: decidedResultSchema,
    results: z.array(resultSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListByWorkunitIdResultsResponse>;

/**
 * `list_results_request`, sent on `compute.v1.results.list`.
 *
 * The list reads every run the session may see. A filter by outcome narrows it. The
 * result model states no default order, so a read that wants newest first asks for the
 * recording time, which the model declares sortable.
 */
export const listResultsRequestSchema = z.object({
    ...pageFields,
    filter: z
        .object({
            workunit_id: z.string().nullable().default(null),
            outcome: z.int().nullable().default(null),
            id_one_of: z.array(z.string()).nullable().default(null),
            workunit_id_one_of: z.array(z.string()).nullable().default(null),
            outcome_one_of: z.array(z.int()).nullable().default(null),
        })
        .nullable()
        .default(null),
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<ListResultsRequest>;

export const listResultsReplySchema = z.object({
    result: decidedResultSchema,
    results: z.array(resultSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListResultsResponse>;

export const getWorkunitRequestSchema = z.object({
    key: z.object({ id: z.string() }),
}) satisfies z.ZodType<GetWorkunitRequest>;

export const getWorkunitReplySchema = z.object({
    result: decidedResultSchema,
    workunit: workunitSchema.nullable().default(null),
}) satisfies z.ZodType<GetWorkunitResponse>;

export const getResultRequestSchema = z.object({
    key: z.object({ id: z.string() }),
}) satisfies z.ZodType<GetResultRequest>;

export const getResultReplySchema = z.object({
    result: decidedResultSchema,
    result_value: resultSchema.nullable().default(null),
}) satisfies z.ZodType<GetResultResponse>;

/** The subjects the reads travel on, from the generated protocols. */
export const GRID_JOB_SUBJECTS = {
    listBatches: batchSubjects.list_batches_request,
    listWorkunitsByBatch: workunitSubjects.list_by_batch_id_workunits_request,
    getWorkunit: workunitSubjects.get_workunit_request,
    listResults: resultSubjects.list_results_request,
    listResultsByWorkunit: resultSubjects.list_by_workunit_id_results_request,
    getResult: resultSubjects.get_result_request,
} as const;

/** One batch as a screen reads it. */
export const batchViewSchema = z.object({
    id: z.string(),
    external_ref: z.string(),
    party_id: z.string().nullable(),
    status: z.string(),
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type BatchView = z.infer<typeof batchViewSchema>;

/** One workunit as a screen reads it. */
export const workunitViewSchema = z.object({
    id: z.string(),
    batch_id: z.string(),
    app_version_id: z.string(),
    input_uri: z.string(),
    config_uri: z.string().nullable(),
    priority: z.int(),
    target_redundancy: z.int(),
    canonical_result_id: z.string().nullable(),
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type WorkunitView = z.infer<typeof workunitViewSchema>;

/**
 * One result as a screen reads it.
 *
 * `host_hidden` says the session may not read the node, so a screen states
 * that rather than the node not yet being known. A run in flight has no node
 * and no output yet, and states `null` for both.
 */
export const resultViewSchema = z.object({
    id: z.string(),
    workunit_id: z.string(),
    host_id: z.string().nullable(),
    host_hidden: z.boolean(),
    server_state: z.int(),
    outcome: z.int(),
    output_uri: z.string().nullable(),
    error_message: z.string().nullable(),
    received_at: nullableWireTimestampSchema,
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type ResultView = z.infer<typeof resultViewSchema>;

/** A page of rows with the count of everything the read matches. */
const pageOf = <Row extends z.ZodType>(row: Row) =>
    z.object({
        read_at: wireTimestampSchema,
        rows: z.array(row),
        total: z.int().nonnegative(),
        offset: z.int().nonnegative(),
        limit: z.int().positive(),
    });

export const batchPageSchema = pageOf(batchViewSchema);
export const workunitPageSchema = pageOf(workunitViewSchema);
export const resultPageSchema = pageOf(resultViewSchema);
export type BatchPage = z.infer<typeof batchPageSchema>;
export type WorkunitPage = z.infer<typeof workunitPageSchema>;
export type ResultPage = z.infer<typeof resultPageSchema>;

const orNull = (value: string): string | null => (value === '' ? null : value);
const idOrNull = (value: string): string | null =>
    value === '' || value === NIL_UUID ? null : value;

export function toBatchView(batch: Batch): BatchView {
    return batchViewSchema.parse({
        id: batch.id,
        external_ref: batch.external_ref,
        party_id: batch.party_id === NIL_UUID ? null : batch.party_id,
        status: batch.status,
        modified_by: batch.modified_by,
        recorded_at: batch.recorded_at,
    });
}

export function toWorkunitView(workunit: Workunit): WorkunitView {
    return workunitViewSchema.parse({
        id: workunit.id,
        batch_id: workunit.batch_id,
        app_version_id: workunit.app_version_id,
        input_uri: workunit.input_uri,
        config_uri: orNull(workunit.config_uri),
        priority: workunit.priority,
        target_redundancy: workunit.target_redundancy,
        canonical_result_id: idOrNull(workunit.canonical_result_id),
        modified_by: workunit.modified_by,
        recorded_at: workunit.recorded_at,
    });
}

/**
 * A result as a screen reads it, with the node stated only when the reader may
 * see it.
 *
 * Which roles see a node is a decision of its own. Until it is made the node
 * is the operator's alone, as the Browse the grid's jobs journey states.
 */
export function toResultView(result: Result, mayReadHost: boolean): ResultView {
    const host = idOrNull(result.host_id);
    return resultViewSchema.parse({
        id: result.id,
        workunit_id: result.workunit_id,
        host_id: mayReadHost ? host : null,
        host_hidden: !mayReadHost && host !== null,
        server_state: result.server_state,
        outcome: result.outcome,
        output_uri: orNull(result.output_uri),
        error_message: orNull(result.error_message),
        received_at: result.received_at === EPOCH ? null : result.received_at,
        modified_by: result.modified_by,
        recorded_at: result.recorded_at,
    });
}

/**
 * A read the compute service refused or could not find.
 *
 * The outcome is the one the service stated, so a route answers with the
 * status that matches and not with a failure it cannot explain.
 */
export type GridJobsRefusal =
    'invalid' | 'denied' | 'missing' | 'conflict' | 'unavailable' | 'failed';

export class GridJobsRefusedError extends OperationFailedError {
    readonly outcome: GridJobsRefusal;

    constructor(subject: string, outcome: GridJobsRefusal, message: string) {
        super(subject, message);
        this.outcome = outcome;
    }
}

export function requireOk(
    subject: string,
    result: { readonly outcome: string; readonly message: string },
): void {
    if (result.outcome === 'ok') {
        return;
    }
    throw new GridJobsRefusedError(subject, result.outcome as GridJobsRefusal, result.message);
}

/**
 * What narrows the batch list. A status and a party are equals conditions and the
 * reference is a contains match that ignores case. The compute service applies them
 * with the row-level policy, so a filter can only narrow what the session may see.
 */
export interface BatchFilter {
    readonly status?: string | undefined;
    readonly partyId?: string | undefined;
    readonly find?: string | undefined;
}

/** The page the caller asked for. */
export interface JobPageRequest {
    readonly offset: number;
    readonly limit: number;
}

/**
 * One page of the batches the session may see, newest first.
 *
 * The page names no order, so the compute service applies the model's
 * default, which is the recording time of the current version of each batch.
 * A batch whose status changes is recorded again and moves to the front.
 *
 * The total is the count of every batch the session may see, which is what a
 * pager reads.
 */
export async function readBatches(
    caller: AuthenticatedCaller,
    page: JobPageRequest,
    filter: BatchFilter = {},
): Promise<{ readonly batches: readonly Batch[]; readonly total: number }> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.listBatches,
        listBatchesRequestSchema.parse({
            ...page,
            filter: {
                status: filter.status ?? null,
                party_id: filter.partyId ?? null,
                search: filter.find ?? null,
            },
        }),
        listBatchesReplySchema,
    );
    requireOk(GRID_JOB_SUBJECTS.listBatches, reply.result);
    return { batches: reply.batches, total: reply.total };
}

/**
 * One batch by its id, or nothing when the session may not see it.
 *
 * The generated batch read is keyed by the external reference, which two
 * batches may share, so the id goes through the list read's own id filter.
 */
export async function readBatch(caller: AuthenticatedCaller, id: string): Promise<Batch | null> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.listBatches,
        listBatchesRequestSchema.parse({
            offset: 0,
            limit: 1,
            filter: { id_one_of: [id] },
        }),
        listBatchesReplySchema,
    );
    requireOk(GRID_JOB_SUBJECTS.listBatches, reply.result);
    return reply.batches[0] ?? null;
}

/** One page of the workunits of one batch. */
export async function readWorkunitsOfBatch(
    caller: AuthenticatedCaller,
    batchId: string,
    page: JobPageRequest,
): Promise<{ readonly workunits: readonly Workunit[]; readonly total: number }> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.listWorkunitsByBatch,
        listWorkunitsByBatchRequestSchema.parse({ batch_id: batchId, ...page }),
        listWorkunitsByBatchReplySchema,
    );
    requireOk(GRID_JOB_SUBJECTS.listWorkunitsByBatch, reply.result);
    return { workunits: reply.workunits, total: reply.total };
}

/** One workunit by its id, or nothing when the session may not see it. */
export async function readWorkunit(
    caller: AuthenticatedCaller,
    id: string,
): Promise<Workunit | null> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.getWorkunit,
        getWorkunitRequestSchema.parse({ key: { id } }),
        getWorkunitReplySchema,
    );
    if (reply.result.outcome === 'missing') {
        return null;
    }
    requireOk(GRID_JOB_SUBJECTS.getWorkunit, reply.result);
    return reply.workunit;
}

/** One page of the runs of one workunit. */
export async function readResultsOfWorkunit(
    caller: AuthenticatedCaller,
    workunitId: string,
    page: JobPageRequest,
): Promise<{ readonly results: readonly Result[]; readonly total: number }> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.listResultsByWorkunit,
        listResultsByWorkunitRequestSchema.parse({ workunit_id: workunitId, ...page }),
        listResultsByWorkunitReplySchema,
    );
    requireOk(GRID_JOB_SUBJECTS.listResultsByWorkunit, reply.result);
    return { results: reply.results, total: reply.total };
}

/**
 * One page of the runs that ended with an outcome, newest first.
 *
 * This answers which of the runs failed. The outcome is a code: 1 is Success, 3 is
 * ClientError, 4 is NoReply, and 0 is a run that has no outcome yet.
 */
export async function readResultsByOutcome(
    caller: AuthenticatedCaller,
    outcome: number,
    page: JobPageRequest,
): Promise<{ readonly results: readonly Result[]; readonly total: number }> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.listResults,
        listResultsRequestSchema.parse({
            ...page,
            order: { field: 'recorded_at', descending: true },
            filter: { outcome },
        }),
        listResultsReplySchema,
    );
    requireOk(GRID_JOB_SUBJECTS.listResults, reply.result);
    return { results: reply.results, total: reply.total };
}

/** One result by its id, or nothing when the session may not see it. */
export async function readResult(caller: AuthenticatedCaller, id: string): Promise<Result | null> {
    const reply = await caller.callAuthenticated(
        GRID_JOB_SUBJECTS.getResult,
        getResultRequestSchema.parse({ key: { id } }),
        getResultReplySchema,
    );
    if (reply.result.outcome === 'missing') {
        return null;
    }
    requireOk(GRID_JOB_SUBJECTS.getResult, reply.result);
    return reply.result_value;
}
