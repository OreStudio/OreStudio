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
import { AGILE_DOC_TYPES } from '@ores/contracts';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import { InvalidSprintNameError, type AgileStore } from './agile/agile-store.js';
import { InvalidDocIdError, type DocText } from './scenarios/scenario-store.js';
import type { LiveSession } from './sessions.js';

/**
 * The agile browser routes: the sprints, one sprint's board, the backlog
 * buckets, and any agile doc by id.
 *
 * A route never touches a file. It asks the agile store, so a database store
 * can replace the files. With no store the browser is off and every route
 * answers not found. Every route needs a signed-in session. The doc route
 * serves sprints, stories, tasks, captures and test scenarios only, so the rest of the doc
 * root stays unread.
 */

const SERVED_TYPES: ReadonlySet<string> = new Set(AGILE_DOC_TYPES);

const BROWSER_OFF =
    'The agile browser is off. Set ORES_WEB_DOC_ROOT to the folder that holds the agile tree.';

/** The codes of a file call that fails for a reason outside the code. */
const FILE_ERROR_CODES: ReadonlySet<string> = new Set([
    'EACCES',
    'EBUSY',
    'EIO',
    'EMFILE',
    'ENFILE',
    'ENOENT',
    'ENOTDIR',
    'EPERM',
    'ETIMEDOUT',
]);

function failure(error: unknown): unknown {
    if (error instanceof InvalidSprintNameError || error instanceof InvalidDocIdError) {
        return invalidRequest(error.message);
    }
    const code = (error as { code?: unknown } | null)?.code;
    if (typeof code === 'string' && FILE_ERROR_CODES.has(code)) {
        return new HttpFailure(503, {
            code: 'upstream-unavailable',
            message: 'The agile files cannot be read now. Try again.',
        });
    }
    return error;
}

async function guarded<T>(work: () => Promise<T>): Promise<T> {
    try {
        return await work();
    } catch (error) {
        throw failure(error);
    }
}

export function registerAgileRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
    store: AgileStore | undefined,
    // The doc route reads through the scenario store's id index, so it reads the
    // same doc root as the file agile store only while both come from the config.
    readDoc: ((id: string) => Promise<DocText | null>) | undefined,
): void {
    const open = (request: FastifyRequest): AgileStore => {
        requireSession(request);
        if (store === undefined) throw notFound(BROWSER_OFF);
        return store;
    };

    server.get('/api/agile/sprints', async (request) => {
        const agile = open(request);
        return { sprints: await guarded(() => agile.sprints()) };
    });

    server.get('/api/agile/board', async (request) => {
        const agile = open(request);
        const { sprint } = request.query as { sprint?: unknown };
        // A repeated parameter arrives as an array. Only one name is a sprint.
        if (sprint !== undefined && typeof sprint !== 'string') {
            throw invalidRequest('Give one sprint name.');
        }
        const board = await guarded(() => agile.board(sprint === '' ? undefined : sprint));
        if (board === null) throw notFound('No sprint has this name.');
        return { board };
    });

    server.get('/api/agile/backlog', async (request) => {
        const agile = open(request);
        return guarded(() => agile.backlog());
    });

    server.get('/api/agile/docs/:id', async (request) => {
        open(request);
        if (readDoc === undefined) throw notFound(BROWSER_OFF);
        const { id } = request.params as { id: string };
        const doc = await guarded(() => readDoc(id));
        if (doc === null || doc.type === null || !SERVED_TYPES.has(doc.type)) {
            throw notFound('No sprint, story, task, capture or scenario has this id.');
        }
        return { doc };
    });
}
