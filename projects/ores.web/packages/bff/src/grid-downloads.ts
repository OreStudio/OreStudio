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

import { Readable } from 'node:stream';
import type { FastifyInstance, FastifyReply, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { GridJobsRefusedError, readResult, readWorkunit } from '@ores/wire-protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The download routes: the stored input of a workunit and the stored output of a run.
 *
 * A workunit names its input and a run names its output as stored objects. The browser
 * has no route to them, so the BFF streams them. The BFF reads the owning record first
 * on the session's own client, so the compute service checks the read code and the row
 * policy, and a caller who may not read the record is refused before any byte of the
 * file is asked for. The file is then fetched from the storage service with the
 * session's own token and streamed on, so a large file is never held in memory.
 */

const idParamsSchema = z.object({
    id: z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i),
});

/** Every stored object of the platform lives under this prefix of the storage service. */
const STORAGE_PREFIX = '/api/v1/storage/';

/**
 * The path of a stored object, or nothing when the address is not one.
 *
 * The address comes from a row, not from the caller, but the BFF still asks the
 * storage service only for a plain object path: under the storage prefix, with no dot
 * segment, no query and no fragment, so a bad row cannot reach another route.
 */
export function storagePath(uri: string): string | null {
    if (!uri.startsWith(STORAGE_PREFIX) || uri.length === STORAGE_PREFIX.length) {
        return null;
    }
    if (/[?#\\]/.test(uri) || uri.includes('//', STORAGE_PREFIX.length - 1)) {
        return null;
    }
    const segments = uri.slice(STORAGE_PREFIX.length).split('/');
    if (segments.some((segment) => segment === '' || segment === '.' || segment === '..')) {
        return null;
    }
    return uri;
}

/** The file name a download is saved as: the last segment of the object path. */
export function downloadName(path: string): string {
    return path.slice(path.lastIndexOf('/') + 1);
}

function idParam(request: FastifyRequest): string {
    const parsed = idParamsSchema.safeParse(request.params);
    if (!parsed.success) {
        throw invalidRequest('The address names an id that is not a uuid.');
    }
    return parsed.data.id;
}

async function answering<T>(call: () => Promise<T>): Promise<T> {
    try {
        return await call();
    } catch (error) {
        if (error instanceof GridJobsRefusedError) {
            if (error.outcome === 'denied') {
                throw notPermitted(error.message || 'Your role cannot read jobs.');
            }
            if (error.outcome === 'missing') {
                throw notFound('No job matches this address.');
            }
        }
        throw error;
    }
}

/** Streams the stored object to the caller, or states why it cannot. */
async function stream(
    session: LiveSession,
    reply: FastifyReply,
    uri: string,
    what: string,
): Promise<FastifyReply> {
    const path = storagePath(uri);
    if (path === null) {
        throw notFound(`This ${what} names no stored file.`);
    }
    const base = await session.client.discoverHttpBaseUrl();
    if (base === null) {
        throw notFound('The deployment has no file service.');
    }
    let upstream: Response;
    try {
        upstream = await fetch(new URL(path, base), {
            headers: { Authorization: `Bearer ${session.client.token}` },
        });
    } catch {
        throw new HttpFailure(502, {
            code: 'upstream-unavailable',
            message: 'The file service did not answer.',
        });
    }
    if (upstream.status === 404) {
        throw notFound(`The stored ${what} is no longer there.`);
    }
    if (upstream.status === 401 || upstream.status === 403) {
        throw notPermitted(`The file service refused the stored ${what}.`);
    }
    if (!upstream.ok || upstream.body === null) {
        throw new HttpFailure(502, {
            code: 'upstream-unavailable',
            message: `The stored ${what} could not be read.`,
        });
    }
    reply
        .header('Content-Type', upstream.headers.get('content-type') ?? 'application/octet-stream')
        .header(
            'Content-Disposition',
            `attachment; filename="${downloadName(path).replace(/"/g, '')}"`,
        )
        .header('Cache-Control', 'private, no-store');
    const length = upstream.headers.get('content-length');
    if (length !== null) {
        reply.header('Content-Length', length);
    }
    return reply.send(Readable.fromWeb(upstream.body as never));
}

export function registerGridDownloadRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** The input bundle of a workunit. */
    server.get('/api/operations/grid/jobs/workunits/:id/input', async (request, reply) => {
        const session = requireSession(request);
        const id = idParam(request);
        const workunit = await answering(() => readWorkunit(session.client, id));
        if (workunit === null) {
            throw notFound('No workunit has this id.');
        }
        return stream(session, reply, workunit.input_uri, 'input');
    });

    /** The output of a run. */
    server.get('/api/operations/grid/jobs/results/:id/output', async (request, reply) => {
        const session = requireSession(request);
        const id = idParam(request);
        const result = await answering(() => readResult(session.client, id));
        if (result === null) {
            throw notFound('No result has this id.');
        }
        return stream(session, reply, result.output_uri, 'output');
    });
}
