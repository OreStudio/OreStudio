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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    type BatchFilter,
    batchPageSchema,
    batchViewSchema,
    GridJobsRefusedError,
    readBatch,
    readBatches,
    readResult,
    readResultsByOutcome,
    readResultsOfWorkunit,
    readWorkunit,
    readWorkunitsOfBatch,
    resultPageSchema,
    resultViewSchema,
    toBatchView,
    toResultView,
    toWorkunitView,
    toWireTimestamp,
    workunitPageSchema,
    workunitViewSchema,
} from '@ores/wire-protocol';
import { invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The jobs routes: the batches, workunits and results of the compute grid, one
 * page at a time, as the Jobs tab and the job page read them.
 *
 * Every route is one generated read on the session's own client. The compute
 * service checks the entity's read code and the row-level policy narrows the
 * rows, so a route states no tenant and no party and cannot widen a read. A
 * tenant's session reads its own jobs, and an operator reads the jobs of the
 * tenant the session is inside.
 */

// The wire schema allows a page of up to 1000 rows. The BFF holds a page to 200.
const PAGE_DEFAULT = 25;
const PAGE_MAX = 200;

// An empty or signed value is refused, where a plain coercion would read it as 0.
const wholeNumber = z.string().regex(/^\d+$/).transform(Number);

const pageQuerySchema = z
    .object({
        offset: wholeNumber.pipe(z.int().min(0)).default(0),
        limit: wholeNumber.pipe(z.int().min(1).max(PAGE_MAX)).default(PAGE_DEFAULT),
    })
    .strict();

/** What narrows the batch list: a status, a party and part of the reference. */
const batchQuerySchema = pageQuerySchema.extend({
    status: z.string().trim().min(1).max(32).optional(),
    party: z.uuid().optional(),
    find: z.string().trim().max(256).optional(),
});

/** The runs that ended with an outcome code: 0 none yet, 1 Success, 3 ClientError, 4 NoReply. */
const OUTCOME_CODES = [0, 1, 3, 4] as const;
const outcomeQuerySchema = pageQuerySchema.extend({
    outcome: wholeNumber.pipe(z.union(OUTCOME_CODES.map((code) => z.literal(code)))),
});

const idParamsSchema = z.object({
    id: z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i),
});

const BATCH_REFUSED =
    'The batch list takes a page, a status of up to 32 characters, a party id and up to 256 characters of the reference.';
const PAGE_REFUSED = `The jobs read takes an offset of 0 or more and a limit of 1 to ${PAGE_MAX}.`;

/**
 * Which sessions may read the node a result ran on.
 *
 * The node belongs to the installation, so until the owner decides who else
 * may read it, the operator alone does.
 */
function mayReadHost(session: LiveSession): boolean {
    return session.mode === 'system-administration';
}

/** States a refusal the compute service decided as the status that matches it. */
async function answering<T>(read: () => Promise<T>): Promise<T> {
    try {
        return await read();
    } catch (error) {
        if (error instanceof GridJobsRefusedError) {
            if (error.outcome === 'denied') {
                throw notPermitted(error.message || 'Your role cannot read jobs.');
            }
            if (error.outcome === 'missing') {
                throw notFound('No job matches this address.');
            }
            if (error.outcome === 'invalid') {
                throw invalidRequest(error.message || PAGE_REFUSED);
            }
        }
        throw error;
    }
}

function batchRequest(request: FastifyRequest): {
    page: { offset: number; limit: number };
    filter: BatchFilter;
} {
    const parsed = batchQuerySchema.safeParse(request.query ?? {});
    if (!parsed.success) {
        throw invalidRequest(BATCH_REFUSED);
    }
    const { offset, limit, status, party, find } = parsed.data;
    return {
        page: { offset, limit },
        filter: {
            status,
            partyId: party,
            find: find === '' ? undefined : find,
        },
    };
}

function pageRequest(request: FastifyRequest): { offset: number; limit: number } {
    const parsed = pageQuerySchema.safeParse(request.query ?? {});
    if (!parsed.success) {
        throw invalidRequest(PAGE_REFUSED);
    }
    return parsed.data;
}

function idParam(request: FastifyRequest): string {
    const parsed = idParamsSchema.safeParse(request.params);
    if (!parsed.success) {
        throw invalidRequest('The address names an id that is not a uuid.');
    }
    return parsed.data.id;
}

export function registerGridJobsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** One page of the batches the session may see, newest first. */
    server.get('/api/operations/grid/jobs/batches', async (request) => {
        const session = requireSession(request);
        const { page, filter } = batchRequest(request);
        const { batches, total } = await answering(() => readBatches(session.client, page, filter));
        return batchPageSchema.parse({
            read_at: toWireTimestamp(new Date()),
            rows: batches.map(toBatchView),
            total,
            ...page,
        });
    });

    server.get('/api/operations/grid/jobs/batches/:id', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const batch = await answering(() => readBatch(session.client, id));
        if (batch === null) {
            throw notFound('No batch has this id.');
        }
        return batchViewSchema.parse(toBatchView(batch));
    });

    /** One page of the workunits of one batch. */
    server.get('/api/operations/grid/jobs/batches/:id/workunits', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const page = pageRequest(request);
        const { workunits, total } = await answering(() =>
            readWorkunitsOfBatch(session.client, id, page),
        );
        return workunitPageSchema.parse({
            read_at: toWireTimestamp(new Date()),
            rows: workunits.map(toWorkunitView),
            total,
            ...page,
        });
    });

    server.get('/api/operations/grid/jobs/workunits/:id', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const workunit = await answering(() => readWorkunit(session.client, id));
        if (workunit === null) {
            throw notFound('No workunit has this id.');
        }
        return workunitViewSchema.parse(toWorkunitView(workunit));
    });

    /** One page of the runs of one workunit. */
    server.get('/api/operations/grid/jobs/workunits/:id/results', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const page = pageRequest(request);
        const { results, total } = await answering(() =>
            readResultsOfWorkunit(session.client, id, page),
        );
        return resultPageSchema.parse({
            read_at: toWireTimestamp(new Date()),
            rows: results.map((result) => toResultView(result, mayReadHost(session))),
            total,
            ...page,
        });
    });

    /** One page of the runs that ended with an outcome, the answer to which of them failed. */
    server.get('/api/operations/grid/jobs/results', async (request) => {
        const session = requireSession(request);
        const parsed = outcomeQuerySchema.safeParse(request.query ?? {});
        if (!parsed.success) {
            throw invalidRequest(
                `The runs read takes an outcome code of 0, 1, 3 or 4, an offset of 0 or more and a limit of 1 to ${PAGE_MAX}.`,
            );
        }
        const { outcome, offset, limit } = parsed.data;
        const { results, total } = await answering(() =>
            readResultsByOutcome(session.client, outcome, { offset, limit }),
        );
        return resultPageSchema.parse({
            read_at: toWireTimestamp(new Date()),
            rows: results.map((result) => toResultView(result, mayReadHost(session))),
            total,
            offset,
            limit,
        });
    });

    server.get('/api/operations/grid/jobs/results/:id', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const result = await answering(() => readResult(session.client, id));
        if (result === null) {
            throw notFound('No result has this id.');
        }
        return resultViewSchema.parse(toResultView(result, mayReadHost(session)));
    });
}
