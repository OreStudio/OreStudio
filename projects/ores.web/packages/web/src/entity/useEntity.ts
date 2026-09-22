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

import {
  useMutation,
  useQuery,
  useQueryClient,
  type UseMutationResult,
  type UseQueryResult,
} from '@tanstack/react-query';
import { z } from 'zod';
import { request } from '../api/transport.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * One hook set for every entity.
 *
 * A schema for the page shape, a row projection, and the four calls a screen
 * makes. None of it belongs to one entity -- the page shape is the BFF's
 * contract, the projection is the column metadata's, and the calls are the same
 * four for every entity.
 *
 * What is per entity is the declaration, which is generated. Everything in this
 * file is written once.
 *
 * The row is validated as an open record rather than against a per-entity schema.
 * That is deliberate: the BFF has already parsed the service's response against
 * the schema codegen generated from the entity's model, so re-validating the same
 * shape here would be a second declaration of one contract, which is the drift
 * this design avoids. What the browser does validate is the envelope, because
 * that is this layer's own contract with the BFF.
 */
export type EntityRow = Readonly<Record<string, unknown>>;

const rowSchema = z.record(z.string(), z.unknown());

const pageSchema = z.object({
  rows: z.array(rowSchema),
  totalCount: z.number(),
});

const writeSchema = z.object({
  ok: z.boolean(),
  message: z.string().optional(),
});

const historySchema = z.object({
  versions: z.array(rowSchema),
  message: z.string().optional(),
});

/**
 * How many rows a single-record read will scan for its key.
 *
 * The same ceiling the list screen loads under. See `useEntity`.
 */
const LOAD_ONE_CEILING = 1000;

export interface EntityListQuery {
  /** One-based, because that is what the paging control shows. */
  readonly page: number;
  readonly pageSize: number;
}

export interface EntityPage {
  readonly rows: readonly EntityRow[];
  readonly totalCount: number;
}

export interface EntityWrite {
  readonly data: Readonly<Record<string, unknown>>;
  /**
   * Why the write is being made.
   *
   * User-owned, unlike the audit provenance: the service derives who and when
   * from the authenticated context, and the reason is the one thing only the
   * caller knows.
   */
  readonly intent: { readonly reason_code: string; readonly commentary: string };
  /**
   * The version the screen read, for a change to an existing row.
   *
   * Absent for a create, which states the absence of a row rather than a
   * version. Stating the version the screen read is what makes two people
   * editing one row a conflict rather than a silent overwrite.
   */
  readonly version?: number | undefined;
}

/**
 * The query key, namespaced by component so two components cannot collide.
 *
 * Exported because a screen invalidates and watches by key, and a key built at
 * the call site is a key that eventually disagrees with the one the query was
 * filed under.
 */
export function entityListKey(
  descriptor: EntityDescriptor,
  query: EntityListQuery,
): readonly unknown[] {
  return [descriptor.component, descriptor.entity, 'list', query.page, query.pageSize];
}

export function entityKey(
  descriptor: EntityDescriptor,
  key: string,
): readonly unknown[] {
  return [descriptor.component, descriptor.entity, 'record', key];
}

export function useEntityList(
  descriptor: EntityDescriptor,
  query: EntityListQuery,
): UseQueryResult<EntityPage> {
  return useQuery({
    queryKey: entityListKey(descriptor, query),
    queryFn: async () => {
      const offset = (query.page - 1) * query.pageSize;
      const body = await request(
        `${descriptor.apiBase}?offset=${String(offset)}&limit=${String(query.pageSize)}`,
        { method: 'GET' },
      );
      return pageSchema.parse(body);
    },
  });
}

/**
 * Whether a row's natural key is the one the route asked for.
 *
 * A key read from the URL is always a string, while a row may hold a number: the
 * model can declare a numeric natural key, and the protocol carries it as a
 * number. Comparing the two as text matches a numeric key against the string the
 * route carries without loosening the match for a key that is already a string.
 */
function keyMatches(value: unknown, key: string): boolean {
  return typeof value === 'string' ? value === key : String(value) === key;
}

/**
 * One record, by its natural key.
 *
 * Read from the list rather than from a single-record endpoint, because the
 * entity protocol defines no single-record read: the model derives a list, a
 * save, a delete and a history, and nothing that answers for one record. The
 * BFF's factory serves `GET /api/<collection>/<key>` only where the model does
 * declare one, so asking for it here would 404 on every entity that does not.
 *
 * The ceiling is the one the list screen already loads under, and a collection
 * larger than it answers "not found" for a record past the ceiling. That is the
 * defect a real single-record read would remove. This is an honest stand-in for
 * one, not the finished shape: when the model gains a single-record read, this
 * becomes a direct fetch again and the ceiling goes.
 */
export function useEntity(
  descriptor: EntityDescriptor,
  key: string | undefined,
): UseQueryResult<EntityRow | undefined> {
  return useQuery({
    queryKey: entityKey(descriptor, key ?? ''),
    enabled: key !== undefined && key.length > 0,
    queryFn: async () => {
      const body = await request(
        `${descriptor.apiBase}?offset=0&limit=${String(LOAD_ONE_CEILING)}`,
        { method: 'GET' },
      );
      const page = pageSchema.parse(body);
      return page.rows.find(
        (row) => keyMatches(row[descriptor.meta.keyField], key ?? ''),
      );
    },
  });
}

export function useEntityHistory(
  descriptor: EntityDescriptor,
  key: string | undefined,
): UseQueryResult<readonly EntityRow[]> {
  return useQuery({
    queryKey: [...entityKey(descriptor, key ?? ''), 'history'],
    enabled: key !== undefined && key.length > 0,
    queryFn: async () => {
      const body = await request(
        `${descriptor.apiBase}/${encodeURIComponent(key ?? '')}/history`,
        { method: 'GET' },
      );
      return historySchema.parse(body).versions;
    },
  });
}

export function useSaveEntity(
  descriptor: EntityDescriptor,
): UseMutationResult<unknown, Error, EntityWrite> {
  const client = useQueryClient();
  return useMutation({
    mutationFn: async (write: EntityWrite) =>
      writeSchema.parse(
        await request(descriptor.apiBase, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            data: write.data,
            intent: write.intent,
            version: write.version,
          }),
        }),
      ),
    onSuccess: () => {
      // A save can change any page's contents, so the whole entity is
      // invalidated rather than the page that happened to be open.
      void client.invalidateQueries({ queryKey: [descriptor.component, descriptor.entity] });
    },
  });
}

export function useDeleteEntity(
  descriptor: EntityDescriptor,
): UseMutationResult<
  unknown,
  Error,
  {
    readonly key: string;
    readonly intent: { readonly reason_code: string; readonly commentary: string };
    readonly version?: number | undefined;
  }
> {
  const client = useQueryClient();
  return useMutation({
    mutationFn: async (input) =>
      writeSchema.parse(
        await request(`${descriptor.apiBase}/${encodeURIComponent(input.key)}`, {
          method: 'DELETE',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            intent: input.intent,
            version: input.version,
          }),
        }),
      ),
    onSuccess: () => {
      void client.invalidateQueries({ queryKey: [descriptor.component, descriptor.entity] });
    },
  });
}
