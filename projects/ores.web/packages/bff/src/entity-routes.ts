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
import { toWireTimestamp } from '@ores/wire-protocol';
import { z } from 'zod';
import type { LiveSession } from './sessions.js';

/**
 * An entity's routes, registered from one declaration.
 *
 * The declaration holds /values/ and no functions. That is the point: a
 * descriptor with a callback in it is code, and code cannot be generated from
 * an entity model. The request envelopes are regular across every entity in
 * the tree, so the factory builds them from the key field and the collection:
 *
 *   list     { offset, limit }
 *   save     { data }
 *   delete   { <keys>: [key] }
 *   history  { <keyField>: key }
 *
 * The delete and history envelopes carry the natural key, because the path
 * segment the web builds is the natural key. A descriptor states them only
 * when the entity's own request is keyed by that same key; an entity whose
 * request carries its surrogate primary key instead has no such route, rather
 * than a route that sends a value no row matches.
 *
 * A refusal is not a failure. When the service answers `success: false` the
 * client did nothing wrong, so the response is a 409 carrying the service's own
 * words rather than a 500 carrying ours.
 */
export interface EntityRouteDescriptor {
  /** The collection segment of the path, without `/api/`, e.g. `tenant_types`. */
  readonly collection: string;
  /** The name of the path parameter holding the natural key, e.g. `id`. */
  readonly key: string;
  /** The wire field the natural key lives in, e.g. `type`. */
  readonly keyField: string;
  /**
   * The array field a batch delete names its keys in, e.g. `types`.
   *
   * Present exactly when `subjects.remove` is, which is when the delete
   * request is keyed by the natural key. Stated rather than pluralised here:
   * `code` to `codes` and `id` to `ids` are mechanical, `status` to `statuses`
   * is not, and a wrong guess produces a request the service refuses with no
   * clue why. The model knows the plural, so the model states it.
   */
  readonly deleteKeysField?: string;
  readonly subjects: {
    readonly list: string;
    /** Present only when the service can answer for one record by its key. */
    readonly get?: string;
    readonly save: string;
    /** Present only when the delete request is keyed by the natural key. */
    readonly remove?: string;
    /**
     * Present only when the entity is temporal and the history request is
     * keyed by the natural key.
     */
    readonly history?: string;
  };
  /** The array field the list response holds its rows in, e.g. `tenant_types`. */
  readonly rowsField: string;
  /**
   * The array field the history response holds its versions in, e.g. `history`.
   *
   * Present exactly when `subjects.history` is.
   */
  readonly historyRowsField?: string;
  /**
   * The audit timestamp fields the wire decoder refuses to see empty.
   *
   * The shared web container seeds every member, so an audit timestamp arrives
   * at the save as an empty string. The service stamps the real time from its
   * own clock; the save only has to send a value the decoder accepts, and an
   * empty string is not one.
   */
  readonly timestampFields?: readonly string[];
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

/**
 * The page window a generic list accepts.
 *
 * The service takes unsigned integers, and an offset or limit that is `NaN`,
 * negative or unbounded is not a page it can answer. The query is parsed and
 * validated rather than coerced with `Number`, which turns a malformed value
 * into `NaN` and passes it on.
 */
const listRequestSchema = z.object({
  offset: z.int().nonnegative(),
  limit: z.int().positive().max(1000),
});

/** A parsed-but-unvalidated body, read by field name. */
function body(value: unknown): Record<string, unknown> {
  return (value ?? {}) as Record<string, unknown>;
}

function rows(value: unknown, field: string): readonly unknown[] {
  const found = body(value)[field];
  return Array.isArray(found) ? (found as readonly unknown[]) : [];
}

/** The value at a possibly-dotted field path of a save payload. */
function readPath(root: Record<string, unknown>, path: string): unknown {
  let current: unknown = root;
  for (const part of path.split('.')) {
    if (typeof current !== 'object' || current === null) {
      return undefined;
    }
    current = (current as Record<string, unknown>)[part];
  }
  return current;
}

/** Sets a possibly-dotted field path, creating the members it walks through. */
function writePath(
  root: Record<string, unknown>,
  path: string,
  value: unknown,
): void {
  const parts = path.split('.');
  const last = parts[parts.length - 1];
  if (last === undefined) {
    return;
  }
  let current = root;
  for (const part of parts.slice(0, -1)) {
    const next = current[part];
    if (typeof next !== 'object' || next === null) {
      current[part] = {};
    }
    current = current[part] as Record<string, unknown>;
  }
  current[last] = value;
}

/**
 * A copy of a save payload whose empty audit timestamps carry the current time.
 *
 * The shared web container seeds every member, so an audit timestamp arrives
 * here as an empty string, and the service's decoder refuses an empty string as
 * a timestamp. The service stamps the real time; this value only has to decode.
 */
function stampTimestamps(
  value: unknown,
  fields: readonly string[],
): unknown {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    return value;
  }
  const stamped = { ...(value as Record<string, unknown>) };
  for (const field of fields) {
    const current = readPath(stamped, field);
    if (typeof current === 'string' && current.length > 0) {
      continue;
    }
    writePath(stamped, field, toWireTimestamp(new Date()));
  }
  return stamped;
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
    const input = listRequestSchema.parse({
      offset: query['offset'] === undefined ? 0 : Number(query['offset']),
      limit: query['limit'] === undefined ? 100 : Number(query['limit']),
    });
    const response = await session.client.callAuthenticated(
      descriptor.subjects.list,
      input,
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
    const data = incoming['data'];
    const response = await session.client.callAuthenticated(
      descriptor.subjects.save,
      {
        data: descriptor.timestampFields === undefined
          ? data
          : stampTimestamps(data, descriptor.timestampFields),
      },
      descriptor.saveResponse ?? identity,
    );

    if (body(response)['success'] === false) {
      return reply.code(409).send({ message: body(response)['message'] ?? '' });
    }
    return { ok: true, message: body(response)['message'] ?? '' };
  });

  const removeSubject = descriptor.subjects.remove;
  const deleteKeysField = descriptor.deleteKeysField;
  if (removeSubject !== undefined && deleteKeysField !== undefined) {
    server.delete(keyPath, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        removeSubject,
        { [deleteKeysField]: [keyOf(request)] },
        descriptor.removeResponse ?? identity,
      );

      if (body(response)['success'] === false) {
        return reply.code(409).send({ message: body(response)['message'] ?? '' });
      }
      return { ok: true, message: body(response)['message'] ?? '' };
    });
  }

  const historySubject = descriptor.subjects.history;
  const historyRowsField = descriptor.historyRowsField;
  if (historySubject !== undefined && historyRowsField !== undefined) {
    server.get(`${keyPath}/history`, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        historySubject,
        { [descriptor.keyField]: keyOf(request) },
        descriptor.historyResponse ?? identity,
      );

      /*
       * The service returns the versions newest first, which is how a person
       * reads a history: what changed last is the question being asked. The
       * rows are passed through in that order.
       */
      return {
        versions: rows(response, historyRowsField),
        message: body(response)['message'] ?? '',
      };
    });
  }
}
