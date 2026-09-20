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

import type { FastifyInstance, FastifyReply, FastifyRequest } from 'fastify';
import type { z } from 'zod';
import type { LiveSession } from './sessions.js';

/**
 * The five routes every entity has, registered from one declaration.
 *
 * An entity used to add its own handlers here: a list, a save, a delete and a
 * history, each written out again for the next entity with the field names
 * changed. That is how two entities of one kind end up behaving differently.
 *
 * What is left to an entity is the part that is genuinely its own: the subjects,
 * the schemas, the name of the natural key, and how a wire row becomes the row
 * the table shows. Everything else -- the route shape, the paging parameters,
 * the refusal status, the session and the tenant -- is here, once.
 *
 * A refusal is not a failure. When the service answers `success: false` the
 * client did nothing wrong, so the response is a 409 carrying the service's own
 * words rather than a 500 carrying ours.
 */
export interface EntityRouteDescriptor {
  /** The collection segment of the path, without `/api/`, e.g. `countries`. */
  readonly collection: string;
  /** The name of the path parameter holding the natural key, e.g. `code`. */
  readonly key: string;
  readonly subjects: {
    readonly list: string;
    /** Present only when the service can answer for one record by its key. */
    readonly get?: string;
    readonly save: string;
    readonly remove: string;
    /** Present only when the entity is temporal. */
    readonly history?: string;
  };
  /** Builds the list request from the query string. */
  readonly listRequest: (query: Record<string, string | undefined>) => unknown;
  readonly listResponse: z.ZodTypeAny;
  /** The field in the list response holding the rows. */
  readonly rowsField: string;
  /** Turns one wire row into the row the table shows. */
  readonly view: (row: never) => unknown;
  /** The body of a save: the record, the reason and the commentary. */
  readonly saveBody: z.ZodTypeAny;
  readonly saveRequest: (body: never, session: LiveSession) => unknown;
  readonly saveResponse: z.ZodTypeAny;
  readonly removeRequest: (key: string) => unknown;
  readonly removeResponse: z.ZodTypeAny;
  readonly getRequest?: (key: string) => unknown;
  readonly getResponse?: z.ZodTypeAny;
  readonly getView?: (row: never) => unknown;
  readonly historyRequest?: (key: string) => unknown;
  readonly historyResponse?: z.ZodTypeAny;
}

/** Passed in rather than imported, because the session store is per-server. */
export type RequireSession = (request: FastifyRequest) => LiveSession;

/**
 * Registers an entity's routes.
 *
 * The list answers `{ rows, totalCount }` for every entity, and the history
 * answers `{ versions, message }`. One shape, so the browser's hooks are one
 * set rather than one per entity.
 */
export function registerEntityRoutes(
  server: FastifyInstance,
  requireSession: RequireSession,
  descriptor: EntityRouteDescriptor,
): void {
  const base = `/api/${descriptor.collection}`;
  const keyPath = `${base}/:${descriptor.key}`;

  server.get(base, async (request: FastifyRequest) => {
    const session = requireSession(request);
    const query = request.query as Record<string, string | undefined>;
    const response = (await session.client.callAuthenticated(
      descriptor.subjects.list,
      descriptor.listRequest(query),
      descriptor.listResponse,
    )) as Record<string, unknown>;

    const rows = response[descriptor.rowsField] as readonly unknown[];
    return {
      rows: rows.map((row) => descriptor.view(row as never)),
      totalCount: response['total_available_count'],
    };
  });

  const { get: getSubject } = descriptor.subjects;
  const { getRequest, getResponse, getView } = descriptor;
  if (getSubject !== undefined && getRequest !== undefined && getResponse !== undefined) {
    server.get(keyPath, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const key = readKey(request, descriptor.key);
      const response = (await session.client.callAuthenticated(
        getSubject,
        getRequest(key),
        getResponse,
      )) as Record<string, unknown>;
      const row = (getView ?? descriptor.view)(response['data'] as never);
      return { row };
    });
  }

  server.post(base, async (request: FastifyRequest, reply: FastifyReply) => {
    const session = requireSession(request);
    const body = descriptor.saveBody.parse(request.body);
    const response = (await session.client.callAuthenticated(
      descriptor.subjects.save,
      descriptor.saveRequest(body as never, session),
      descriptor.saveResponse,
    )) as { success: boolean; message: string };

    if (!response.success) {
      return reply.code(409).send({ message: response.message });
    }
    return { ok: true, message: response.message };
  });

  server.delete(keyPath, async (request: FastifyRequest, reply: FastifyReply) => {
    const session = requireSession(request);
    const key = readKey(request, descriptor.key);
    const response = (await session.client.callAuthenticated(
      descriptor.subjects.remove,
      descriptor.removeRequest(key),
      descriptor.removeResponse,
    )) as { success: boolean; message: string };

    if (!response.success) {
      return reply.code(409).send({ message: response.message });
    }
    return { ok: true, message: response.message };
  });

  const { history: historySubject } = descriptor.subjects;
  const { historyRequest, historyResponse } = descriptor;
  if (historySubject !== undefined && historyRequest !== undefined && historyResponse !== undefined) {
    server.get(`${keyPath}/history`, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const key = readKey(request, descriptor.key);
      const response = (await session.client.callAuthenticated(
        historySubject,
        historyRequest(key),
        historyResponse,
      )) as { history: readonly unknown[]; message: string };

      /*
       * The service returns newest first and that is what a person wants: what
       * changed last is the question being asked. It is passed through rather
       * than reordered, because reversing it here quietly made the screen
       * compare the two oldest versions as though they were the current pair.
       */
      return {
        versions: response.history.map((row) => descriptor.view(row as never)),
        message: response.message,
      };
    });
  }
}

function readKey(request: FastifyRequest, name: string): string {
  const params = request.params as Record<string, string>;
  return params[name] ?? '';
}
