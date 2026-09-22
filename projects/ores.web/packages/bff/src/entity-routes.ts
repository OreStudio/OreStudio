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
import { randomUUID } from 'node:crypto';
import { z } from 'zod';
import type { LiveSession } from './sessions.js';

/**
 * An entity's routes, registered from one declaration.
 *
 * The declaration holds /values/ and no functions. That is the point: a
 * descriptor with a callback in it is code, and code cannot be generated from
 * an entity model. The canonical protocol states one envelope per verb, so the
 * factory builds them from the descriptor's key, its write record's members and
 * the subjects:
 *
 *   list      { offset, limit, order, as_of?, filter? }
 *   get       { key: { <keyField> } }
 *   save      { change: { write, precondition }, intent }
 *   delete    { removal: { key, precondition }, intent }
 *   versions  { key: { <keyField> }, offset, limit, order, filter? }
 *
 * Every request that names a row names it through the entity's key record, and
 * the path segment the web builds carries the natural key, so a route exists
 * only when the two are the same field: an entity whose key record holds its
 * surrogate primary key has no delete or versions route, rather than a route
 * that sends a value no row matches.
 *
 * A refusal is not a failure. The service answers with a result whose outcome
 * says what went wrong, so the client did nothing wrong and the response is a
 * 409 carrying the service's own words rather than a 500 carrying ours.
 */
export interface EntityRouteDescriptor {
  /** The collection segment of the path, without `/api/`, e.g. `tenant_types`. */
  readonly collection: string;
  /** The name of the path parameter holding the natural key, e.g. `id`. */
  readonly key: string;
  /** The wire field the natural key lives in, e.g. `type`. */
  readonly keyField: string;
  /**
   * The wire field a single-record read answers in, e.g. `tenant`.
   *
   * The canonical get response carries the entity under its own singular
   * name, which is the name the model gave it rather than a name to guess.
   */
  readonly rowField: string;
  /**
   * The write record's members, in the order the model declares them.
   *
   * A save sends these and nothing else. The record a form holds carries
   * members the write record does not -- the version the edit was made
   * against, the reason it was made -- and those travel in the precondition
   * and the intent, while the rest are the server's to own.
   */
  readonly writeFields: readonly string[];
  /**
   * What each write member takes when the form does not carry it.
   *
   * A create sends the whole record, and a member the form does not show -- a
   * surrogate key, a field the entity hides -- has no value on the row yet.
   * The blank is the empty of that member's own type, and `'uuid'` means the
   * caller mints the identifier, because the model says that member names a
   * row the store has never seen.
   */
  readonly writeDefaults: Readonly<Record<string, unknown>>;
  /**
   * The row fields carrying the change intent, e.g. `change_reason_code`.
   *
   * Empty strings for an entity whose rows keep no audit columns, which is
   * an entity whose intent is empty rather than one whose intent is refused.
   */
  readonly intentFields: {
    readonly reason: string;
    readonly commentary: string;
  };
  /** Whether the list request carries an as-of instant. */
  readonly listHasAsOf: boolean;
  /** Whether the list request carries a filter record. */
  readonly listHasFilter: boolean;
  /** Whether the versions request carries a filter record. */
  readonly versionsHasFilter: boolean;
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
     * Present only when the entity is temporal and the versions request is
     * keyed by the natural key.
     */
    readonly history?: string;
  };
  /** The array field the list response holds its rows in, e.g. `tenant_types`. */
  readonly rowsField: string;
  /**
   * The array field the versions response holds its rows in, e.g. `versions`.
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

/**
 * The order a page is returned in, when the caller names none.
 *
 * An empty field is the order by key, which is what makes a caller that names
 * no order still get a stable page. The protocol carries the order on every
 * list request, so a request that omitted it would not decode.
 */
const defaultOrder = { field: '', descending: false } as const;

/** The versions a history route asks for: one record, newest first. */
const HISTORY_LIMIT = 1000;

/** A parsed-but-unvalidated body, read by field name. */
function body(value: unknown): Record<string, unknown> {
  return (value ?? {}) as Record<string, unknown>;
}

function rows(value: unknown, field: string): readonly unknown[] {
  const found = body(value)[field];
  return Array.isArray(found) ? (found as readonly unknown[]) : [];
}

/** The outcome the service reported, or a value no caller can mistake for ok. */
function outcome(value: unknown): string {
  const result = body(body(value)['result']);
  return typeof result['outcome'] === 'string' ? result['outcome'] : 'failed';
}

/** The service's own words about how the request ended. */
function resultMessage(value: unknown): string {
  const message = body(body(value)['result'])['message'];
  return typeof message === 'string' ? message : '';
}

/**
 * The value a write member takes when the form carried none.
 *
 * A member whose default is `'uuid'` names a row the store has never seen, so
 * the caller mints it; every other default is already the value to send.
 */
function blankWriteValue(blank: unknown): unknown {
  return blank === 'uuid' ? randomUUID() : blank;
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
  /** The key record every canonical request that names a row carries. */
  const keyRecord = (request: FastifyRequest): Record<string, string> => ({
    [descriptor.keyField]: keyOf(request),
  });

  server.get(base, async (request: FastifyRequest) => {
    const session = requireSession(request);
    const query = request.query as Record<string, string | undefined>;
    const input = listRequestSchema.parse({
      offset: query['offset'] === undefined ? 0 : Number(query['offset']),
      limit: query['limit'] === undefined ? 100 : Number(query['limit']),
    });
    const asOf = query['asOf'];
    const response = await session.client.callAuthenticated(
      descriptor.subjects.list,
      {
        ...input,
        order: defaultOrder,
        ...(descriptor.listHasAsOf
          ? { as_of: asOf === undefined || asOf.length === 0 ? null : asOf }
          : {}),
        ...(descriptor.listHasFilter ? { filter: null } : {}),
      },
      descriptor.listResponse ?? identity,
    );

    return {
      rows: rows(response, descriptor.rowsField),
      totalCount: body(response)['total'] ?? 0,
    };
  });

  const getSubject = descriptor.subjects.get;
  if (getSubject !== undefined) {
    server.get(keyPath, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        getSubject,
        { key: keyRecord(request) },
        descriptor.getResponse ?? identity,
      );
      return { row: body(response)[descriptor.rowField] };
    });
  }

  const saveSubject = descriptor.subjects.save;
  if (saveSubject !== undefined) {
    server.post(base, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const incoming = body(request.body);
      const data = incoming['data'];
      const response = await session.client.callAuthenticated(
        saveSubject,
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
  }

  const removeSubject = descriptor.subjects.remove;
  if (removeSubject !== undefined) {
    server.delete(keyPath, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        removeSubject,
        {
          /*
           * A removal the path segment drives is unconditional: the route
           * carries the key and no version, and a version the client never
           * read would be a claim it cannot make.
           */
          removal: {
            key: keyRecord(request),
            precondition: { kind: 'any', version: null },
          },
          intent: { reason_code: '', commentary: '' },
        },
        descriptor.removeResponse ?? identity,
      );

      if (outcome(response) !== 'ok') {
        return reply.code(409).send({ message: resultMessage(response) });
      }
      return { ok: true, message: resultMessage(response) };
    });
  }

  const historySubject = descriptor.subjects.history;
  const historyRowsField = descriptor.historyRowsField;
  if (historySubject !== undefined && historyRowsField !== undefined) {
    server.get(`${keyPath}/history`, async (request: FastifyRequest) => {
      const session = requireSession(request);
      const response = await session.client.callAuthenticated(
        historySubject,
        {
          key: keyRecord(request),
          offset: 0,
          limit: HISTORY_LIMIT,
          /*
           * Newest first, because that is how a person reads a history: what
           * changed last is the question being asked. The service answers in
           * the order it is asked for, so the request states it.
           */
          order: { field: '', descending: true },
          ...(descriptor.versionsHasFilter ? { filter: null } : {}),
        },
        descriptor.historyResponse ?? identity,
      );

      return {
        versions: rows(response, historyRowsField),
        message: resultMessage(response),
      };
    });
  }
}
