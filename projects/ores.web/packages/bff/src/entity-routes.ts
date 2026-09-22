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

import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyReply, FastifyRequest } from 'fastify';
import { z } from 'zod';
import type { LiveSession } from './sessions.js';

/**
 * The write default that says the caller names the row.
 *
 * A write member that is a surrogate identifier has no empty of its own type:
 * the caller mints it. The generated descriptor states this marker for those
 * members, and the factory replaces it with a fresh identifier, so the marker
 * never reaches the wire.
 */
export const MINTED_WRITE_DEFAULT = '<minted>';

/** What a write member takes when the form carries no value for it. */
export type WriteDefault = string | number | boolean | null;

/**
 * An entity's routes, registered from one declaration.
 *
 * The declaration holds /values/ and no functions. That is the point: a
 * descriptor with a callback in it is code, and code cannot be generated from
 * an entity model. The request envelopes are regular across every entity in
 * the tree, so the factory builds them from the key and the collection:
 *
 *   list     { offset, limit, order[, as_of][, filter] }
 *   get      { key }
 *   save     { change: { write, precondition }, intent }
 *   delete   { removal: { key, precondition }, intent }
 *   history  { key, offset, limit, order[, filter] }
 *
 * Every one of those states the key the model declares, which is the key the
 * path carries, so nothing is translated between an address and a request. The
 * path has one segment per key member: a junction's key is the pair it links,
 * and stating one member of it would address half a row.
 *
 * The wire format states every member a request or record declares, so the
 * optional ones are sent as null and a write member the form does not carry
 * takes the default the model gave it. A descriptor that omitted either would
 * send a message the service cannot decode.
 *
 * The history request names the entity's key, so an entity whose key has more
 * than one member has no history route: the route would address the pair the
 * key record already states.
 *
 * A refusal is not a failure. When the service answers an outcome that is not
 * `ok` the client did nothing wrong, so the response is a 409 carrying the
 * service's own words rather than a 500 carrying ours.
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
  /**
   * The write record's members, in the order the model declares them.
   *
   * A save sends these and nothing else. The record a form holds carries
   * members the write record does not -- the version the edit was made
   * against, the reason it was made -- and those travel in the precondition
   * and the intent, while the rest are the server's to own.
   */
  readonly writeFields?: readonly string[];
  /**
   * What each write member takes when the form does not carry it.
   *
   * The wire format states every member of a record, so a member the form
   * does not show -- a surrogate key, a field the entity hides -- still has
   * to be sent. The default is the empty of that member's own type, or
   * {@link MINTED_WRITE_DEFAULT} for a member the caller has to name.
   */
  readonly writeDefaults?: Readonly<Record<string, WriteDefault>>;
  /**
   * Whether the list request carries an as-of instant.
   *
   * The wire format states every member of a request, so the factory has to
   * know which optional members a request declares and send them as null.
   */
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

/** The order a list is read in when the caller names none: by key, ascending. */
const defaultOrder = { field: '', descending: false } as const;

/** How many versions a history answers with. One page, newest first. */
const HISTORY_LIMIT = 500;

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
 * A member whose default is {@link MINTED_WRITE_DEFAULT} names a row the store
 * has never seen, so the caller mints it; every other default is already the
 * value to send. A member the descriptor states no default for is a descriptor
 * that disagrees with the model it was generated from, so it is refused rather
 * than sent as no value.
 */
function blankWriteValue(blank: WriteDefault | undefined): unknown {
  if (blank === undefined) {
    throw new Error('the write record states no default for a member');
  }
  return blank === MINTED_WRITE_DEFAULT ? randomUUID() : blank;
}

/**
 * The reason and commentary a write carries.
 *
 * The wire format states every member, so a caller that names neither still
 * sends both as empty strings rather than sending an intent that does not
 * decode.
 */
function intent(value: unknown): { reason_code: string; commentary: string } {
  const incoming = body(value);
  const reason = incoming['reason_code'];
  const commentary = incoming['commentary'];
  return {
    reason_code: typeof reason === 'string' ? reason : '',
    commentary: typeof commentary === 'string' ? commentary : '',
  };
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
      { ...input, order: defaultOrder,
        ...(descriptor.listHasAsOf ? { as_of: null } : {}),
        ...(descriptor.listHasFilter ? { filter: null } : {}) },
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
    const writeFields = descriptor.writeFields ?? [];
    const writeDefaults = descriptor.writeDefaults ?? {};
    server.post(base, async (request: FastifyRequest, reply: FastifyReply) => {
      const session = requireSession(request);
      const incoming = body(request.body);
      const data = body(incoming['data']);
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
            /*
             * The record states every member the write record declares. A
             * member the form did not carry -- a surrogate key, a member the
             * screen hides -- takes the default the model gave it, because a
             * record with a member absent does not decode.
             */
            write: Object.fromEntries(
              writeFields.map((field) => [
                field,
                data[field] === undefined
                  ? blankWriteValue(writeDefaults[field])
                  : data[field],
              ]),
            ),
            precondition: {
              kind: typeof version === 'number'
                ? 'must_match_version'
                : 'must_not_exist',
              version: typeof version === 'number' ? version : null,
            },
          },
          intent: intent(incoming['intent']),
        },
        descriptor.saveResponse ?? identity,
      );

      if (outcome(response) !== 'ok') {
        return reply.code(409).send({ message: resultMessage(response) });
      }
      return { ok: true, message: resultMessage(response) };
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
          intent: intent(incoming['intent']),
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
        // The versions request addresses the entity's own key record, the same
        // one every other canonical request states, so the address and the
        // request carry one key rather than two spellings of it.
        {
          key: keyOf(request),
          offset: 0,
          limit: HISTORY_LIMIT,
          /*
           * Key order, which is the only order the store pages in. A history
           * is read newest first, so the rows are reversed below rather than
           * asked for in an order the store refuses.
           */
          order: { field: '', descending: false },
          ...(descriptor.versionsHasFilter ? { filter: null } : {}),
        },
        descriptor.historyResponse ?? identity,
      );

      /*
       * Newest first, because that is how a person reads a history: what
       * changed last is the question being asked. The store answers in key
       * order, so the screen's order is stated here once rather than by every
       * caller.
       */
      return {
        versions: [...rows(response, historyRowsField)].reverse(),
        message: resultMessage(response),
      };
    });
  }
}
