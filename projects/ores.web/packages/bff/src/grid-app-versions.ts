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
import {
    GridJobsRefusedError,
    RELEASE_NOTE_MAX,
    appVersionPageViewSchema,
    readAppVersionPage,
    writeReleaseNote,
} from '@ores/wire-protocol';
import { invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The app version routes: one build of the compute catalogue with its release note,
 * its packages and the other versions of its app, and the edit of the note.
 *
 * A read is the entity's own generated read on the session's own client, so the
 * compute service checks the read code. The note is written by the super
 * administrator alone: the catalogue's versions belong to the system tenant, and the
 * route says so before any call goes out. The service refuses any other writer too.
 */

const idParamsSchema = z.object({
    id: z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i),
});

const noteBodySchema = z
    .object({
        release_note: z.string().max(RELEASE_NOTE_MAX),
        version: z.int().min(0),
        commentary: z.string().max(500).default('Edited the release note'),
    })
    .strict();

/** Only an operator in system administration writes the catalogue. */
function mayWrite(session: LiveSession): boolean {
    return session.mode === 'system-administration';
}

async function answering<T>(call: () => Promise<T>): Promise<T> {
    try {
        return await call();
    } catch (error) {
        if (error instanceof GridJobsRefusedError) {
            if (error.outcome === 'denied') {
                throw notPermitted(error.message || 'Your role cannot do this.');
            }
            if (error.outcome === 'missing') {
                throw notFound('No app version has this id.');
            }
            if (error.outcome === 'invalid') {
                throw invalidRequest(error.message || 'The request was refused.');
            }
        }
        throw error;
    }
}

function idParam(request: FastifyRequest): string {
    const parsed = idParamsSchema.safeParse(request.params);
    if (!parsed.success) {
        throw invalidRequest('The address names an id that is not a uuid.');
    }
    return parsed.data.id;
}

export function registerGridAppVersionRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** One app version with its packages and the other versions of its app. */
    server.get('/api/operations/grid/app-versions/:id', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        const page = await answering(() =>
            readAppVersionPage(session.client, id, mayWrite(session)),
        );
        if (page === null) {
            throw notFound('No app version has this id.');
        }
        return appVersionPageViewSchema.parse(page);
    });

    /**
     * Replaces the release note. The body states the version the editor read, so a
     * note edited by someone else since is refused as a conflict.
     */
    server.put('/api/operations/grid/app-versions/:id/release-note', async (request) => {
        const session = requireSession(request);
        const id = idParam(request);
        if (!mayWrite(session)) {
            throw notPermitted('Only a super administrator edits a release note.');
        }
        const body = noteBodySchema.safeParse(request.body ?? {});
        if (!body.success) {
            throw invalidRequest(
                `The release note takes up to ${RELEASE_NOTE_MAX} characters and the version it read.`,
            );
        }
        const written = await answering(() =>
            writeReleaseNote(session.client, id, {
                releaseNote: body.data.release_note,
                version: body.data.version,
                commentary: body.data.commentary,
            }),
        );
        if (written === null) {
            throw notFound('No app version has this id.');
        }
        const page = await answering(() => readAppVersionPage(session.client, id, true));
        if (page === null) {
            throw notFound('No app version has this id.');
        }
        return appVersionPageViewSchema.parse(page);
    });
}
