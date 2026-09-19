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
import {
  LIVE_WORKSPACE_ID,
  SYSTEM_TENANT_ID,
  toWireTimestamp,
  uuid,
  wireTimestamp,
  type Uuid,
} from './primitives.js';

/**
 * The account classifications the server understands.
 *
 * `user` accounts authenticate with a password; the rest authenticate through
 * sessions. See `ores.iam.api/domain/account.hpp`.
 */
export const ACCOUNT_TYPES = ['user', 'service', 'algorithm', 'llm'] as const;
export type AccountType = (typeof ACCOUNT_TYPES)[number];

const accountTypeSchema = z.enum(ACCOUNT_TYPES);

/**
 * A UUID as the server writes it: canonical lowercase, hyphenated.
 *
 * `z.uuid()` alone would accept an uppercase spelling the server never
 * produces, so the pattern is narrowed explicitly, and the value is branded
 * through {@link uuid} so the compile-time type is the same {@link Uuid} every
 * other layer uses.
 */
export const uuidSchema = z
  .string()
  .regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)
  .transform((value): Uuid => uuid(value));

/**
 * An instant as the server writes it: `YYYY-MM-DD HH:MM:SSZ`.
 *
 * Written out rather than imported so this module stays free of a cycle with
 * the wire schemas.
 */
export const wireTimestampSchema = z.string().transform((value, ctx) => {
  try {
    return wireTimestamp(value);
  } catch (cause) {
    ctx.addIssue({ code: 'custom', message: 'Not a wire timestamp', cause });
    return z.NEVER;
  }
});

/**
 * A tenant-scoped account, minus every credential field.
 *
 * The server's `account` struct carries `password_hash`, `password_salt` and
 * `totp_secret`. Those exist to be written, never to be read back, so they are
 * dropped at the parse boundary and cannot reach the browser.
 */
export const accountSchema = z.object({
  /** Optimistic-locking version. Bumped on every accepted write. */
  version: z.int().nonnegative(),
  id: uuidSchema,
  tenantId: uuidSchema,
  username: z.string(),
  /** Present only for accounts that represent a person. */
  fullName: z.string(),
  email: z.string(),
  accountType: accountTypeSchema,
  jobTitle: z.string(),
  /** Reporting line, or `null` when the account sits at the top. */
  reportsToAccountId: uuidSchema.nullable(),
  /** Quick-login party, or `null` when the account always picks a party. */
  defaultPartyId: uuidSchema.nullable(),
  modifiedBy: z.string(),
  changeReasonCode: z.string(),
  changeCommentary: z.string(),
  performedBy: z.string(),
  recordedAt: wireTimestampSchema,
});

export type Account = z.infer<typeof accountSchema>;

/** One selectable party offered at login, or switchable mid-session. */
export const partySummarySchema = z.object({
  id: uuidSchema,
  name: z.string(),
  /** `System` or `Operational`. */
  partyCategory: z.string(),
  /** FpML business-centre code, for example `GBLO`. */
  businessCenterCode: z.string(),
});

export type PartySummary = z.infer<typeof partySummarySchema>;

/** A page of accounts. `totalCount` counts every account the caller can see. */
export const accountPageSchema = z.object({
  accounts: z.array(accountSchema),
  totalCount: z.int().nonnegative(),
  offset: z.int().nonnegative(),
  limit: z.int().positive(),
});

export type AccountPage = z.infer<typeof accountPageSchema>;

/**
 * The live session's selected party, as the handover carries it.
 *
 * The account and tenant identifiers travel alongside the party because the
 * browser needs them to render the shell, and re-deriving them from a token it
 * must not read would be pointless indirection.
 */
export const activePartySchema = z.object({
  accountId: uuidSchema,
  tenantId: uuidSchema,
  tenantName: z.string(),
  party: partySummarySchema,
  sessionId: z.string(),
  /** When the token was last issued, so the browser can refresh before expiry. */
  issuedAt: wireTimestampSchema,
  accessLifetimeSeconds: z.int().positive(),
});

export type ActiveParty = z.infer<typeof activePartySchema>;

/** Re-exported so consumers do not import the sentinels from two places. */
export { LIVE_WORKSPACE_ID, SYSTEM_TENANT_ID, toWireTimestamp };
