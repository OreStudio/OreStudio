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
import { accountSchema, partySummarySchema, uuidSchema, wireTimestampSchema } from './domain.js';
import type { Account, PartySummary } from './domain.js';
import type { Uuid } from './primitives.js';

/**
 * The request and response bodies for every subject this client speaks.
 *
 * The literals here are the protocol: a rename in C++ must be mirrored here or
 * the boundary test fails. Subjects are relative; the transport prepends the
 * configured prefix.
 */

export const SUBJECTS = {
  login: 'iam.v1.auth.login',
  logout: 'iam.v1.auth.logout',
  refresh: 'iam.v1.auth.refresh',
  selectParty: 'iam.v1.accounts.select-party',
  switchParty: 'iam.v1.accounts.switch-party',
  listAccounts: 'iam.v1.accounts.list',
  listChangeReasons: 'dq.v1.change_reasons.list',
  getImages: 'assets.v1.images.get',
  listImages: 'assets.v1.images.list',
  httpInfo: 'http-server.v1.info.get',
} as const;

/**
 * Whether the deployment still needs provisioning.
 *
 * The IAM service answers the flag and leaves the sentence to the caller: a
 * deployment in bootstrap mode has no accounts to sign in with, so the words
 * that say so are the interface's, not the wire's.
 */
export const bootstrapStatusResponseSchema = z.object({
  is_in_bootstrap_mode: z.boolean().default(false),
  message: z.string().default(''),
});

const uuidLike = z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);

/**
 * A flag that the server may omit.
 *
 * The C++ structs give most fields an initialiser and `rfl::msgpack` writes
 * every member, so a well-formed reply carries them all. A default keeps a
 * partial reply readable instead of turning it into a hard failure.
 */
const flag = z.boolean().default(false);
const text = z.string().default('');

/**
 * `login_request`. The credential field is `principal`, not `username`.
 */
export const loginRequestSchema = z.object({
  principal: z.string(),
  password: z.string(),
});
export type LoginRequest = z.infer<typeof loginRequestSchema>;

/** `party_summary` as it appears inside `login_response`. */
export const wirePartySchema = z.object({
  id: uuidSchema,
  name: z.string(),
  party_category: z.string(),
  business_center_code: z.string(),
});

function mapParty(row: z.infer<typeof wirePartySchema>): PartySummary {
  return {
    id: row.id,
    name: row.name,
    partyCategory: row.party_category,
    businessCenterCode: row.business_center_code,
  };
}

/**
 * `login_response`.
 *
 * `token` is the bearer JWT, and it doubles as the party-selection credential:
 * the server issues a single-use token when `selected_party_id` is empty. The
 * login flow therefore holds this token until the party picker resolves, or
 * until `select_party` replaces it.
 */
export const loginResponseSchema = z
  .object({
    success: flag,
    account_id: text,
    tenant_id: text,
    tenant_name: text,
    username: text,
    email: text,
    password_reset_required: flag,
    tenant_bootstrap_mode: flag,
    party_setup_required: flag,
    party_setup_warning: text,
    token: text,
    error_message: text,
    message: text,
    selected_party_id: text,
    available_parties: z.array(wirePartySchema).default([]),
    default_party_id: text,
    access_lifetime_s: z.int().default(1800),
    session_id: text,
  })
  .transform((row) => ({
    success: row.success,
    accountId: row.account_id,
    tenantId: row.tenant_id,
    tenantName: row.tenant_name,
    username: row.username,
    email: row.email,
    passwordResetRequired: row.password_reset_required,
    tenantBootstrapMode: row.tenant_bootstrap_mode,
    partySetupRequired: row.party_setup_required,
    partySetupWarning: row.party_setup_warning,
    token: row.token,
    errorMessage: row.error_message,
    message: row.message,
    selectedPartyId: row.selected_party_id,
    availableParties: row.available_parties.map(mapParty),
    defaultPartyId: row.default_party_id,
    accessLifetimeSeconds: row.access_lifetime_s,
    sessionId: row.session_id,
  }));

export type LoginResponse = z.infer<typeof loginResponseSchema>;

/**
 * `logout_request` carries no body. The server still expects a decodable
 * payload, so the codec writes an empty map.
 */
export const emptyRequestSchema = z.object({});

/** `logout_response`. */
export const logoutResponseSchema = z.object({
  success: flag,
  message: text,
});
export type LogoutResponse = z.infer<typeof logoutResponseSchema>;

/** `refresh_request` carries no body; identity comes from the bearer token. */
export const refreshResponseSchema = z.object({
  success: flag,
  token: text,
  message: text,
  access_lifetime_s: z.int().default(1800),
});
export type RefreshResponse = z.infer<typeof refreshResponseSchema>;

/** `select_party_request` and `switch_party_request` share a body and reply. */
export const partyRequestSchema = z.object({
  party_id: uuidLike,
});
export type PartyRequest = z.infer<typeof partyRequestSchema>;

export const partyResponseSchema = z
  .object({
    success: flag,
    message: text,
    token: text,
    username: text,
    tenant_name: text,
    party_name: text,
    party_setup_required: flag,
    party_setup_warning: text,
    access_lifetime_s: z.int().default(1800),
  })
  .transform((row) => ({
    success: row.success,
    message: row.message,
    token: row.token,
    username: row.username,
    tenantName: row.tenant_name,
    partyName: row.party_name,
    partySetupRequired: row.party_setup_required,
    partySetupWarning: row.party_setup_warning,
    accessLifetimeSeconds: row.access_lifetime_s,
  }));
export type PartyResponse = z.infer<typeof partyResponseSchema>;

/**
 * `http-server.v1.info.get` carries no body. The reply tells the client where
 * the companion HTTP server listens, which the Qt client discovers right after
 * login.
 */
export const httpInfoResponseSchema = z.object({
  base_url: text,
  success: flag,
  message: text,
});
export type HttpInfoResponse = z.infer<typeof httpInfoResponseSchema>;

/** `lock_account_request` and `unlock_account_request` share a body and reply. */
export const accountIdsRequestSchema = z.object({
  account_ids: z.array(uuidLike).default([]),
});
export type AccountIdsRequest = z.infer<typeof accountIdsRequestSchema>;

/** `account_operation_result`, one per requested id. */
export const accountOperationResultSchema = z.object({
  success: flag,
  message: text,
});
export type AccountOperationResult = z.infer<typeof accountOperationResultSchema>;

export const lockResultSchema = z.object({
  results: z.array(accountOperationResultSchema).default([]),
});
export type LockResult = z.infer<typeof lockResultSchema>;

/**
 * `change_password_request_typed`.
 *
 * The untyped `change_password_request` in the header has no subject, so this
 * is the one the client sends.
 */
export const changePasswordRequestSchema = z.object({
  current_password: z.string(),
  new_password: z.string(),
});
export type ChangePasswordRequest = z.infer<typeof changePasswordRequestSchema>;

export const changePasswordResultSchema = z.object({
  success: flag,
  message: text,
});
export type ChangePasswordResult = z.infer<typeof changePasswordResultSchema>;

/** `get_accounts_request_typed`, sent on `iam.v1.accounts.list`. */
export const listAccountsRequestSchema = z.object({
  offset: z.int().nonnegative().default(0),
  limit: z.int().positive().max(1000).default(100),
});
export type ListAccountsRequest = z.infer<typeof listAccountsRequestSchema>;

const wireAccountSchema = z.object({
  version: z.int().nonnegative().default(0),
  id: uuidSchema,
  tenant_id: uuidSchema,
  username: text,
  full_name: text,
  email: text,
  account_type: z.string().default('user'),
  job_title: text,
  reports_to_account_id: uuidSchema.nullable().default(null),
  default_party_id: uuidSchema.nullable().default(null),
  modified_by: text,
  change_reason_code: text,
  change_commentary: text,
  performed_by: text,
  recorded_at: wireTimestampSchema,
});

/**
 * Translates one wire account into the domain type.
 *
 * The nil UUID is the server's "no value" sentinel on the reference fields, so
 * it becomes `null` here. Every credential field the struct carries --
 * `password_hash`, `password_salt`, `totp_secret`, `image_id` -- is absent
 * from the schema, so it is dropped rather than forwarded.
 */
function mapAccount(row: z.infer<typeof wireAccountSchema>): Account {
  return {
    version: row.version,
    id: row.id,
    tenantId: row.tenant_id,
    username: row.username,
    fullName: row.full_name,
    email: row.email,
    accountType: parseAccountType(row.account_type),
    jobTitle: row.job_title,
    reportsToAccountId: orNil(row.reports_to_account_id),
    defaultPartyId: orNil(row.default_party_id),
    modifiedBy: row.modified_by,
    changeReasonCode: row.change_reason_code,
    changeCommentary: row.change_commentary,
    performedBy: row.performed_by,
    recordedAt: row.recorded_at,
  };
}

const NIL_UUID = '00000000-0000-0000-0000-000000000000';

function orNil(value: Uuid | null): Uuid | null {
  return value === null || value === NIL_UUID ? null : value;
}

function parseAccountType(value: string): Account['accountType'] {
  const parsed = accountSchema.shape.accountType.safeParse(value);
  return parsed.success ? parsed.data : 'user';
}

/**
 * `get_accounts_response`.
 *
 * `total_available_count` is renamed to `totalCount` for the HTTP contract, so
 * this schema is the HTTP shape directly.
 */
export const accountPageSchema = z
  .object({
    accounts: z.array(wireAccountSchema).default([]),
    total_available_count: z.int().nonnegative().default(0),
  })
  .transform((row) => ({
    accounts: row.accounts.map(mapAccount),
    totalCount: row.total_available_count,
  }));

/** The translated page the BFF returns and the browser consumes. */
export type WireAccountPage = z.infer<typeof accountPageSchema>;

/** Narrowing helper: a UUID the endpoint will accept. */
export const partyIdSchema = z.string().regex(/^[0-9a-f-]{36}$/);

/** Re-exported so callers can validate a party in isolation. */
export { partySummarySchema };


/**
 * The change reasons a write may carry.
 *
 * Read from the DQ service rather than declared here as a list, because the set
 * is data that differs per deployment. The three `applies_to_*` flags are what
 * decide which reasons are offered for which operation, and
 * `requires_commentary` decides whether an explanation is mandatory.
 *
 * `applies_to_new` is the wire name; the model calls the same idea create.
 */
export const changeReasonSchema = z.object({
  version: z.int().nonnegative().default(0),
  code: z.string(),
  description: z.string().default(''),
  category_code: z.string().default(''),
  applies_to_new: z.boolean().default(false),
  applies_to_amend: z.boolean().default(false),
  applies_to_delete: z.boolean().default(false),
  requires_commentary: z.boolean().default(false),
  display_order: z.int().default(0),
});

export type ChangeReason = z.infer<typeof changeReasonSchema>;

export const changeReasonPageSchema = z.object({
  reasons: z.array(changeReasonSchema).default([]),
  total_available_count: z.int().nonnegative().default(0),
  success: z.boolean().default(false),
  message: z.string().default(''),
});
