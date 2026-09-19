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

import { z } from 'zod';

/**
 * The HTTP contract for the connections pre-authentication experience.
 *
 * Both sides import these, so the browser parses what the server serialised
 * from the same definition. Nothing here carries a password: the store's
 * secrets stay on the server and the browser is told what exists, not what it
 * would take to sign in.
 */

/**
 * Identifiers over the wire.
 *
 * These stay plain strings here. Branding them would mean importing the store's
 * domain types, and this package is loaded by the browser as well as by the
 * server, so it must carry no Node dependency at all. The server brands them
 * when it hands them to the store.
 */
const idSchema = z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);

export const environmentIdSchema = idSchema;
export const connectionIdSchema = idSchema;
export const folderIdSchema = idSchema;

export const endpointSchema = z.object({
  host: z.string().min(1),
  /** The NATS port. */
  port: z.int().min(1).max(65535),
  /** The subject namespace, for example `ores.dev.local1`. */
  subjectPrefix: z.string(),
  /** The companion HTTP server port. */
  httpPort: z.int().min(1).max(65535),
});
export type EndpointView = z.infer<typeof endpointSchema>;

export const environmentViewSchema = z.object({
  id: environmentIdSchema,
  name: z.string(),
  description: z.string(),
  endpoint: endpointSchema,
  folderId: folderIdSchema.nullable(),
  tagNames: z.array(z.string()),
  updatedAt: z.string(),
});
export type EnvironmentView = z.infer<typeof environmentViewSchema>;

/**
 * A saved connection as the browser sees it.
 *
 * `hasSavedPassword` says whether a credential exists. The credential itself
 * never crosses this boundary.
 */
export const connectionViewSchema = z.object({
  id: connectionIdSchema,
  name: z.string(),
  username: z.string(),
  description: z.string(),
  folderId: folderIdSchema.nullable(),
  tagNames: z.array(z.string()),
  hasSavedPassword: z.boolean(),
  environment: z.discriminatedUnion('kind', [
    z.object({
      kind: z.literal('environment'),
      id: environmentIdSchema,
      name: z.string(),
    }),
    z.object({ kind: z.literal('standalone'), endpoint: endpointSchema }),
  ]),
});
export type ConnectionView = z.infer<typeof connectionViewSchema>;

export const folderViewSchema = z.object({
  id: folderIdSchema,
  parentId: folderIdSchema.nullable(),
  name: z.string(),
  description: z.string(),
});
export type FolderView = z.infer<typeof folderViewSchema>;

export const tagViewSchema = z.object({
  id: z.string(),
  name: z.string(),
});
export type TagView = z.infer<typeof tagViewSchema>;

/** Everything the pre-authentication screens need, in one read. */
export const connectionsCatalogSchema = z.object({
  store: z.object({
    /** Where the database is, so a person can find and back it up. */
    databasePath: z.string(),
    directory: z.string(),
    source: z.string(),
    /** True when the store has no master password yet. */
    uninitialised: z.boolean(),
    /** True when a master password has been supplied and checks out. */
    unlocked: z.boolean(),
    hasSavedPasswords: z.boolean(),
  }),
  environments: z.array(environmentViewSchema),
  connections: z.array(connectionViewSchema),
  folders: z.array(folderViewSchema),
  tags: z.array(tagViewSchema),
});
export type ConnectionsCatalog = z.infer<typeof connectionsCatalogSchema>;

export const unlockRequestSchema = z.object({
  masterPassword: z.string().min(1),
});
export type UnlockRequest = z.infer<typeof unlockRequestSchema>;

export const unlockResultSchema = z.object({
  unlocked: z.boolean(),
  /** Set when the store had no master password and this call created one. */
  initialised: z.boolean(),
});
export type UnlockResult = z.infer<typeof unlockResultSchema>;

export const environmentInputSchema = z.object({
  name: z.string().min(1),
  host: z.string().min(1),
  port: z.int().min(1).max(65535),
  httpPort: z.int().min(1).max(65535).default(8080),
  subjectPrefix: z.string().default(''),
  description: z.string().default(''),
  folderId: folderIdSchema.nullable().default(null),
  tagNames: z.array(z.string()).default([]),
});
/**
 * The parsed shape, which is what both sides hold.
 *
 * Defaults are applied by the schema, so a field with one is always present
 * here and a caller need not spell it out. Identifiers stay plain strings,
 * because this is the wire rather than the store's domain.
 */
export type EnvironmentInput = z.infer<typeof environmentInputSchema>;

export const connectionInputSchema = z
  .object({
    name: z.string().min(1),
    username: z.string().min(1),
    /**
     * The password to save.
     *
     * Omitted on an edit leaves the stored one alone, so changing a description
     * cannot silently drop a credential. Empty string clears it.
     */
    password: z.string().optional(),
    description: z.string().default(''),
    folderId: folderIdSchema.nullable().default(null),
    tagNames: z.array(z.string()).default([]),
    environmentId: environmentIdSchema.nullable().default(null),
    host: z.string().nullable().default(null),
    port: z.int().min(1).max(65535).nullable().default(null),
  })
  .refine(
    (value) =>
      (value.environmentId !== null && value.host === null) ||
      (value.environmentId === null && value.host !== null && value.port !== null),
    {
      message: 'A connection either names an environment or states its own host and port',
      path: ['environmentId'],
    },
  );
export type ConnectionInput = z.infer<typeof connectionInputSchema>;

export const folderInputSchema = z.object({
  name: z.string().min(1),
  description: z.string().default(''),
  parentId: folderIdSchema.nullable().default(null),
});
export type FolderInput = z.infer<typeof folderInputSchema>;

/** How to treat a record whose name already exists during an import. */
export const conflictStrategySchema = z.enum(['skip', 'rename', 'replace']);
export type ConflictStrategy = z.infer<typeof conflictStrategySchema>;

export const importRequestSchema = z.object({
  /**
   * The incoming database, base64 encoded.
   *
   * The browser read it from a file the person chose, so the destination stays
   * theirs to pick and this server never opens a path it was handed.
   */
  database: z.string().min(1),
  /** The incoming store's master password, when its credentials are wanted. */
  sourcePassword: z.string().default(''),
  /** When false the structure is imported and the credentials are left behind. */
  includeCredentials: z.boolean().default(true),
  conflict: conflictStrategySchema.default('skip'),
  /**
   * The destination master password, when the store is locked.
   *
   * On a store with no master password yet, the first one supplied becomes it,
   * so a fresh install can be populated from a file in one step.
   */
  targetPassword: z.string().default(''),
  /** Report what would happen without writing anything. */
  dryRun: z.boolean().default(false),
});
export type ImportRequest = z.infer<typeof importRequestSchema>;

export const importReportSchema = z.object({
  dryRun: z.boolean(),
  folders: z.int().nonnegative(),
  environments: z.int().nonnegative(),
  connections: z.int().nonnegative(),
  skipped: z.array(z.string()),
  renamed: z.array(z.object({ from: z.string(), to: z.string() })),
  replaced: z.array(z.string()),
  passwordsImported: z.int().nonnegative(),
  passwordsDropped: z.int().nonnegative(),
});
export type ImportReport = z.infer<typeof importReportSchema>;
