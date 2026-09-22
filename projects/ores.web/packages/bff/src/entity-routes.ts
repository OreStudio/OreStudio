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
 * An entity's routes, registered from one declaration.
 *
 * The declaration holds /values/ and no functions. That is the point: a
 * descriptor with a callback in it is code, and code cannot be generated from
 * an entity model. The request envelopes are regular across every entity in
 * the tree, so the factory builds them from the key and the collection:
 *
 *   list     { offset, limit }
 *   get      { key }
 *   save     { change, intent }
 *   delete   { removal, intent }
 *   history  { entity_type, entity_id }
 *
 * Every one of those states the key the model declares, which is the key the
 * path carries, so nothing is translated between an address and a request. The
 * path has one segment per key member: a junction's key is the pair it links,
 * and stating one member of it would address half a row.
 *
 * The history request is the exception, and states one id. An entity whose key
 * has more than one member therefore has no history route, because the request
 * cannot name a pair and joining one into a string addresses no row.
 *
 * A refusal is not a failure. When the service answers `success: false` the
 * client did nothing wrong, so the response is a 409 carrying the service's own
 * words rather than a 500 carrying ours.
 */
export interface EntityRouteDescriptor {
  /**
   * The owning component and the entity, which together are the dispatch key
   * the generic history request carries (`ores.iam.tenant`).
   */
  readonly component: string;
  readonly entity: string;
  /** The collection segment of the path, without `/api/`, e.g. `tenant_types`. */
  readonly collection: string;
  /**
   * The members of the key the model declares, in the order it declares them.
   *
   * The route's address is a path and a segment carries one value, so a key
   * with two members is addressed by two segments and this names both. A
   * junction's key is the pair it links, which is the case that needs it;
   * every other entity's is one field and its path is one segment.
   */
  readonly keyFields: readonly string[];
  readonly subjects: {
    readonly list: string;
    /** Present only when the service can answer for one record by its key. */
    readonly get?: string;
    /**
     * Present unless the entity is read-only.
     *
     * A read-only entity derives no put request, so there is no write to
     * offer and no `POST` route is registered. The screen still reads, which
     * is why `list` is the only subject a descriptor cannot do without.
     */
    readonly save?: string;
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
  // One path segment per key member, named for the member, so the address and
  // the request it fills state the same key and neither is derived from the
  // other.
  const keyPath = `${base}${descriptor.keyFields
    .map((field) => `/:${field}`)
    .join('')}`;
  const keyOf = (request: FastifyRequest): Record<string, string> => {
    const params = request.params as Record<string, string>;
    const key: Record<string, string> = {};
    for (const field of descriptor.keyFields) {
      key[field] = params[field] ?? '';
    }
    return key;
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
      /*
       * The order is part of the page, not an extra. The service's list
       * request declares the field and the direction and refuses a payload
       * that omits them, so a caller that names no order still states one:
       * an empty field is the order by key, which is what keeps a page
       * stable.
       */
      { ...input, order: { field: '', descending: false } },
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
        // The request's addressing is the key record the model declares, and
        // the path segment carries the same key, so the two agree by
        // construction rather than by a translation here.
        { key: keyOf(request) },
        descriptor.getResponse ?? identity,
      );
      return { row: body(response)[descriptor.getRowField ?? 'data'] };
    });
  }

  const saveSubject = descriptor.subjects.save;
  if (saveSubject !== undefined) {
    server.post(base, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const incoming = body(request.body);
      const data = incoming['data'];
      /*
       * A write states what it believes about the row, and why it is being
       * made. The version is the one the screen read; stating none means the
       * write creates, which the store refuses over a live row rather than
       * replacing it.
       */
      const version = incoming['version'];
      const response = await session.client.callAuthenticated(
        saveSubject,
        {
          change: {
            write: data,
            precondition: {
              kind: typeof version === 'number'
                ? 'must_match_version'
                : 'must_not_exist',
              version: typeof version === 'number' ? version : null,
            },
          },
          intent: body(incoming['intent']),
        },
        descriptor.saveResponse ?? identity,
      );

      if (body(response)['success'] === false) {
        return reply.code(409).send({ message: body(response)['message'] ?? '' });
      }
      return { ok: true, message: body(response)['message'] ?? '' };
    });
  }

  const removeSubject = descriptor.subjects.remove;
  if (removeSubject !== undefined) {
    server.delete(keyPath, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const incoming = body(request.body);
      const version = incoming['version'];
      const response = await session.client.callAuthenticated(
        removeSubject,
        {
          removal: {
            key: keyOf(request),
            precondition: {
              kind: typeof version === 'number'
                ? 'must_match_version'
                : 'any',
              version: typeof version === 'number' ? version : null,
            },
          },
          intent: body(incoming['intent']),
        },
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
        // One generic request serves every entity: the type is the dispatch
        // key and the id is the declared key's value, rendered as the string
        // the request carries.
        {
          entity_type: `ores.${descriptor.component}.${descriptor.entity}`,
          entity_id: String(keyOf(request)[descriptor.keyFields[0] ?? ''] ?? ''),
        },
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
