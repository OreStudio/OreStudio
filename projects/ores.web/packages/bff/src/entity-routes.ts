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
import { z } from 'zod';
import type { LiveSession } from './sessions.js';

/**
 * The five routes every entity has, registered from one declaration.
 *
 * An entity used to add its own handlers here: a list, a save, a delete and a
 * history, each written out again for the next entity with the field names
 * changed. That is how two entities of one kind end up behaving differently.
 *
 * The declaration holds /values/ and no functions. That is the point: a
 * descriptor with a callback in it is code, and code cannot be generated from
 * an entity model. The four request envelopes are regular across every entity in
 * the tree, so the factory builds them from the key field and the collection:
 *
 *   list     { offset, limit }
 *   save     { data }
 *   delete   { <keys>: [key] }
 *   history  { <keyField>: key }
 *
 * A refusal is not a failure. When the service answers `success: false` the
 * client did nothing wrong, so the response is a 409 carrying the service's own
 * words rather than a 500 carrying ours.
 */
export interface EntityRouteDescriptor {
  /** The collection segment of the path, without `/api/`, e.g. `tenant-types`. */
  readonly collection: string;
  /** The name of the path parameter holding the natural key, e.g. `id`. */
  readonly key: string;
  /** The wire field the natural key lives in, e.g. `type`. */
  readonly keyField: string;
  /**
   * The array field a batch delete names its keys in, e.g. `types`.
   *
   * Stated rather than pluralised here. `code` to `codes` and `id` to `ids` are
   * mechanical, `status` to `statuses` is not, and a wrong guess produces a
   * request the service refuses with no clue why. The model knows the plural, so
   * the model states it.
   */
  readonly deleteKeysField: string;
  readonly subjects: {
    readonly list: string;
    /** Present only when the service can answer for one record by its key. */
    readonly get?: string;
    readonly save: string;
    readonly remove: string;
    /** Present only when the entity is temporal. */
    readonly history?: string;
  };
  /** The array field the list response holds its rows in, e.g. `tenant_types`. */
  readonly rowsField: string;
  /**
   * The response schemas, when a caller wants them.
   *
   * Absent by default, and that is deliberate: the service owns the shape of its
   * own reply and has already validated it, so the BFF parses the envelope it
   * needs and passes the rows through. A second declaration of the same shape
   * here is the drift this factory exists to remove.
   */
  readonly listResponse?: z.ZodTypeAny;
  readonly saveResponse?: z.ZodTypeAny;
  readonly removeResponse?: z.ZodTypeAny;
  readonly getResponse?: z.ZodTypeAny;
  readonly historyResponse?: z.ZodTypeAny;
  /** The field a single-record read holds its row in, e.g. `data`. */
  readonly getRowField?: string;
}

/** Passed in rather than imported, because the session store is per-server. */
export type RequireSession = (request: FastifyRequest) => LiveSession;

const identity = z.unknown();

/** A parsed-but-unvalidated body, read by field name. */
function body(value: unknown): Record<string, unknown> {
  return (value ?? {}) as Record<string, unknown>;
}

function rows(value: unknown, field: string): readonly unknown[] {
  const found = body(value)[field];
  return Array.isArray(found) ? (found as readonly unknown[]) : [];
}

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
  const keyOf = (request: FastifyRequest): string => {
    const params = request.params as Record<string, string>;
    return params[descriptor.key] ?? '';
  };

  server.get(base, async (request: FastifyRequest) => {
    const session = requireSession(request);
    const query = request.query as Record<string, string | undefined>;
    const response = await session.client.callAuthenticated(
      descriptor.subjects.list,
      {
        offset: query['offset'] === undefined ? 0 : Number(query['offset']),
        limit: query['limit'] === undefined ? 100 : Number(query['limit']),
      },
      descriptor.listResponse ?? identity,
    );

    return {
      rows: rows(response, descriptor.rowsField),
      totalCount: body(response)['total_available_count'] ?? 0,
    };
  });

  const getSubject = descriptor.subjects.get;
  if (getSubject !== undefined) {
    server.get(keyPath, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        getSubject,
        { [descriptor.keyField]: keyOf(request) },
        descriptor.getResponse ?? identity,
      );
      return { row: body(response)[descriptor.getRowField ?? 'data'] };
    });
  }

  server.post(base, async (request: FastifyRequest, reply: FastifyReply) => {
    const session = requireSession(request);
    const incoming = body(request.body);
    const response = await session.client.callAuthenticated(
      descriptor.subjects.save,
      { data: incoming['data'] },
      descriptor.saveResponse ?? identity,
    );

    if (body(response)['success'] === false) {
      return reply.code(409).send({ message: body(response)['message'] ?? '' });
    }
    return { ok: true, message: body(response)['message'] ?? '' };
  });

  server.delete(keyPath, async (request: FastifyRequest, reply: FastifyReply) => {
    const session = requireSession(request);
    const response = await session.client.callAuthenticated(
      descriptor.subjects.remove,
      { [descriptor.deleteKeysField]: [keyOf(request)] },
      descriptor.removeResponse ?? identity,
    );

    if (body(response)['success'] === false) {
      return reply.code(409).send({ message: body(response)['message'] ?? '' });
    }
    return { ok: true, message: body(response)['message'] ?? '' };
  });

  const historySubject = descriptor.subjects.history;
  if (historySubject !== undefined) {
    server.get(`${keyPath}/history`, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        historySubject,
        { [descriptor.keyField]: keyOf(request) },
        descriptor.historyResponse ?? identity,
      );

      /*
       * The service returns newest first and that is what a person wants: what
       * changed last is the question being asked. It is passed through rather
       * than reordered, because reversing it here quietly made the screen
       * compare the two oldest versions as though they were the current pair.
       */
      return {
        versions: rows(response, 'history'),
        message: body(response)['message'] ?? '',
      };
    });
  }
}
