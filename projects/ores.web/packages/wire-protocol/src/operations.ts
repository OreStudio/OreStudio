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

import type { ListSeedProfilesRequest as GeneratedListSeedProfilesRequest } from './generated/iam/protocol/seed_profile_protocol.js';
import type { ListSessionsRequest as GeneratedListSessionsRequest } from './generated/iam/protocol/session_protocol.js';
import type { ListLoginInfoRequest as GeneratedListLoginInfoRequest } from './generated/iam/protocol/login_info_protocol.js';
import type { ListAccountsRequest as GeneratedListAccountsRequest } from './generated/iam/protocol/account_protocol.js';
import { z } from 'zod';
import {
    accountSchema,
    partySummarySchema,
    uuidSchema,
    wireTimestampSchema,
    type LoginInfo,
    type Session,
} from './domain.js';
import type { Account, PartySummary } from './domain.js';
import { subjects as httpInfoSubjects } from './generated/http/protocol/http_info_protocol.js';
import { subjects as bootstrapSubjects } from './generated/iam/protocol/bootstrap_protocol.js';
import { subjects as seedProfileSubjects } from './generated/iam/protocol/seed_profile_protocol.js';
import { subjects as seedProfileParameterSubjects } from './generated/iam/protocol/seed_profile_parameter_protocol.js';
import { subjects as seedProfileStepSubjects } from './generated/iam/protocol/seed_profile_step_protocol.js';
import { subjects as registrationPolicySubjects } from './generated/iam/protocol/registration_policy_protocol.js';
import { subjects as signupSubjects } from './generated/iam/protocol/signup_protocol.js';
import { subjects as tenantProvisioningSubjects } from './generated/iam/protocol/tenant_provisioning_protocol.js';
import type {
    ProvisionPartyCommand,
    ProvisionTenantCommand,
} from './generated/iam/protocol/tenant_provisioning_protocol.js';
import { subjects as partySubjects } from './generated/refdata/protocol/party_protocol.js';
import { subjects as businessCentreSubjects } from './generated/refdata/protocol/business_centre_protocol.js';
import { subjects as countrySubjects } from './generated/refdata/protocol/country_protocol.js';
import { subjects as imageSubjects } from './generated/assets/protocol/image_protocol.js';
import {
    subjects as workflowSubjects,
    type GetWorkflowStepsRequest,
    type GetWorkflowStepsResponse,
    type RetryWorkflowInstanceRequest as WireRetryWorkflowInstanceRequest,
    type RetryWorkflowInstanceResponse,
    type WorkflowStepSummary as WireWorkflowStepSummary,
} from './generated/workflow/protocol/workflow_protocol.js';
import type { PartyChange } from './generated/refdata/protocol/party_protocol.js';
import type { Uuid } from './primitives.js';

/**
 * The request and response bodies for every subject this client speaks.
 *
 * A subject another component generates is imported from that component's
 * generated protocol rather than copied here, so a rename reaches this client
 * through the generator. The remaining literals are mirrored, and a rename in
 * C++ must be mirrored here or the boundary test fails. Subjects are relative;
 * the transport prepends the configured prefix.
 */

export const SUBJECTS = {
    login: 'iam.v1.auth.login',
    logout: 'iam.v1.auth.logout',
    refresh: 'iam.v1.auth.refresh',
    selectParty: 'iam.v1.accounts.select-party',
    switchParty: 'iam.v1.accounts.switch-party',
    listAccounts: 'iam.v1.accounts.list',
    listChangeReasons: 'dq.v1.change_reasons.list',
    listImages: imageSubjects.list_images_request,
    bootstrapStatus: bootstrapSubjects.bootstrap_status_request,
    createInitialAdmin: bootstrapSubjects.create_initial_admin_request,
    httpInfo: httpInfoSubjects.get_http_info_request,
    listSeedProfiles: seedProfileSubjects.list_seed_profiles_request,
    listSeedProfileSteps:
        seedProfileStepSubjects.list_by_seed_profile_id_seed_profile_steps_request,
    listSeedProfileParameters:
        seedProfileParameterSubjects.list_by_seed_profile_id_seed_profile_parameters_request,
    provisionTenant: tenantProvisioningSubjects.provision_tenant_command,
    provisionParty: tenantProvisioningSubjects.provision_party_command,
    listParties: partySubjects.list_parties_request,
    putParty: partySubjects.put_party_request,
    listBusinessCentres: businessCentreSubjects.list_business_centres_request,
    listCountries: countrySubjects.list_countries_request,
    workflowInstanceSteps: workflowSubjects.get_workflow_steps_request,
    retryWorkflowInstance: workflowSubjects.retry_workflow_instance_request,
    passwordPolicy: 'iam.v1.auth.password-policy',
    registrationPolicy: registrationPolicySubjects.registration_policy_request,
    signup: signupSubjects.signup_request,
    leiEntitiesSummary: 'dq.v1.lei-entities.summary',
    leiEntitiesSearch: 'dq.v1.lei-entities.search',
} as const;

/**
 * Whether the deployment still needs provisioning.
 *
 * The IAM service answers the flags and leaves the sentence to the caller: a
 * deployment in bootstrap mode has no accounts to sign in with, so the words
 * that say so are the interface's, not the wire's.
 */
export const bootstrapStatusResponseSchema = z.object({
    is_in_bootstrap_mode: z.boolean().default(false),
    message: z.string().default(''),
    /*
     * Whether the deployment has a tenant of its own, the system tenant being
     * its bookkeeping rather than a tenant somebody set up. A deployment with
     * none has not been set up, which the interface states alongside the
     * administrator question: the screen that brings an installation to life
     * stays the screen a person belongs on until there is something in it.
     */
    has_tenant: z.boolean().default(false),
    /*
     * The build the answering service runs. It travels with this read because
     * this is the read a browser makes before it has a session, and a screen
     * states the deployment's version whether or not it still needs an
     * administrator.
     */
    version: z.string().default(''),
});

/**
 * The first administrator, created with no session.
 *
 * The one write a deployment accepts before it has any account, and the write
 * that closes bootstrap mode: the function behind it clears the flag, so the
 * deployment stops answering the setup screen from the moment this succeeds.
 *
 * `success` defaults to false rather than true, so a reply that arrives without
 * the field reads as a failure the caller can see instead of a success nobody
 * has checked.
 */
export const createInitialAdminRequestSchema = z.object({
    principal: z.string().min(1),
    password: z.string().min(1),
    email: z.string().min(1),
});

export const createInitialAdminResponseSchema = z.object({
    success: z.boolean().default(false),
    error_message: z.string().default(''),
    account_id: z.string().default(''),
    tenant_id: z.string().default(''),
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
        version: text,
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
        version: row.version,
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
 * `http.v1.info.get` carries no body. The reply tells the client where the
 * companion HTTP server listens, which the web client discovers right after
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

/** How a list request orders its page. An empty field means the key. */
export const orderSchema = z.object({
    field: z.string().default(''),
    descending: z.boolean().default(false),
});

/** `get_accounts_request_typed`, sent on `iam.v1.accounts.list`. */
export const listAccountsRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
}) satisfies z.ZodType<GeneratedListAccountsRequest>;

/** `list_login_info_request`, sent on `iam.v1.login_info.list`. */
export const listLoginInfoRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
}) satisfies z.ZodType<GeneratedListLoginInfoRequest>;
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
    image_id: uuidSchema.nullable().default(null),
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
 * `password_hash`, `password_salt`, `totp_secret` -- is absent from the
 * schema, so it is dropped rather than forwarded.
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
        imageId: orNil(row.image_id),
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
 * The count is the wire's `total`, renamed to `totalCount` for the HTTP
 * contract, so this schema is the HTTP shape directly. `total_available_count`
 * belongs to other components' list replies; IAM states `total`.
 */
export const accountPageSchema = z
    .object({
        accounts: z.array(wireAccountSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        accounts: row.accounts.map(mapAccount),
        totalCount: row.total,
    }));

/** The translated page the BFF returns and the browser consumes. */
export type WireAccountPage = z.infer<typeof accountPageSchema>;

/** `get_account_response`: the account, or nothing when the username is unknown. */
export const accountReplySchema = z
    .object({ account: wireAccountSchema.nullable().default(null) })
    .transform((row) => (row.account === null ? null : mapAccount(row.account)));

/** One login record, as the server writes it. */
const wireLoginInfoSchema = z.object({
    tenant_id: uuidSchema,
    account_id: uuidSchema,
    last_ip: text,
    last_attempt_ip: text,
    failed_logins: z.int().nonnegative().default(0),
    locked: z.boolean().default(false),
    last_login: text,
    online: z.boolean().default(false),
    password_reset_required: z.boolean().default(false),
});

/**
 * Translates one wire login record into the domain type.
 *
 * Field by field rather than by renaming keys, so a column added to the server's
 * struct is not forwarded until somebody decides a screen may show it.
 */
function mapLoginInfo(row: z.infer<typeof wireLoginInfoSchema>): LoginInfo {
    return {
        tenantId: row.tenant_id,
        accountId: row.account_id,
        lastIp: row.last_ip,
        lastAttemptIp: row.last_attempt_ip,
        failedLogins: row.failed_logins,
        locked: row.locked,
        lastLogin: row.last_login,
        online: row.online,
        passwordResetRequired: row.password_reset_required,
    };
}

/** `get_login_info_response`: the record, or nothing when the account has none. */
export const loginInfoReplySchema = z
    .object({ login_info: wireLoginInfoSchema.nullable().default(null) })
    .transform((row) => (row.login_info === null ? null : mapLoginInfo(row.login_info)));

/** `list_login_info_response`. IAM states the count as `total`. */
export const loginInfoPageSchema = z
    .object({
        login_info: z.array(wireLoginInfoSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        loginInfo: row.login_info.map(mapLoginInfo),
        totalCount: row.total,
    }));

/** The translated page the BFF returns and the browser consumes. */
export type WireLoginInfoPage = z.infer<typeof loginInfoPageSchema>;

/** One session, as the server writes it. */
const wireSessionSchema = z.object({
    tenant_id: uuidSchema,
    id: uuidSchema,
    account_id: uuidSchema,
    start_time: text,
    end_time: text,
    client_ip: text,
    client_identifier: text,
    client_version_major: z.int().nonnegative().default(0),
    client_version_minor: z.int().nonnegative().default(0),
    bytes_sent: z.int().nonnegative().default(0),
    bytes_received: z.int().nonnegative().default(0),
    country_code: text,
    protocol: text,
});

/** Translates one wire session into the domain type, field by field. */
function mapSession(row: z.infer<typeof wireSessionSchema>): Session {
    return {
        tenantId: row.tenant_id,
        id: row.id,
        accountId: row.account_id,
        startTime: row.start_time,
        endTime: row.end_time,
        clientIp: row.client_ip,
        clientIdentifier: row.client_identifier,
        clientVersionMajor: row.client_version_major,
        clientVersionMinor: row.client_version_minor,
        bytesSent: row.bytes_sent,
        bytesReceived: row.bytes_received,
        countryCode: row.country_code,
        protocol: row.protocol,
    };
}

/** `list_sessions_response`. IAM states the count as `total`. */
export const sessionPageSchema = z
    .object({
        sessions: z.array(wireSessionSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        sessions: row.sessions.map(mapSession),
        totalCount: row.total,
    }));

/** The translated page the BFF returns and the browser consumes. */
export type WireSessionPage = z.infer<typeof sessionPageSchema>;

/**
 * `get_active_sessions_response`.
 *
 * The reply carries `success` and a message beside the rows, and the handler
 * behind it answers `{success: true}` with no rows today: the read is a stub, so
 * an empty list here is the server's answer rather than a failure.
 */
export const activeSessionsReplySchema = z
    .object({ sessions: z.array(wireSessionSchema).default([]) })
    .transform((row) => ({ sessions: row.sessions.map(mapSession) }));

/** The active sessions the BFF returns and the browser consumes. */
export type WireActiveSessions = z.infer<typeof activeSessionsReplySchema>;

/** `list_sessions_request`, sent on `iam.v1.sessions.list`. */
export const listSessionsRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
}) satisfies z.ZodType<GeneratedListSessionsRequest>;

/**
 * `get_account_request`, and `get_login_info_request` beside it.
 *
 * Both reads name one row by a key object rather than by a bare field, which is
 * the shape the server decodes: a `get` over the derived CRUD protocol takes
 * `{key: {...}}` so that a `get_many` can take `{keys: [...]}` without a second
 * request shape.
 */
export const accountUsernameRequestSchema = z.object({
    key: z.object({ username: z.string().min(1) }),
});

export const loginInfoKeyRequestSchema = z.object({
    key: z.object({ account_id: uuidSchema }),
});

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

/**
 * The result every generated entity response carries.
 *
 * Only the outcome and its words: a caller that acts on a failure reads the
 * code, and the field failures a validation carries are for a form that is
 * not what reads these.
 */
export const resultEnvelopeSchema = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string().default(''),
    message: z.string().default(''),
});

/**
 * One step kind a starting point orders.
 *
 * The arguments the kind consumes stay on the server. The kind fixes their
 * shape in code, so a screen that carried them would be reading a handler's
 * contract; what the starting-point card shows is the kind and its position.
 */
export const seedProfileStepSchema = z.object({
    step_kind: z.string(),
    display_order: z.int().default(0),
});

/**
 * A list of strings the server holds as a serialised JSON array.
 *
 * The column is `jsonb`, so the database guarantees valid JSON and not an
 * array: a writer can store an object there. Reading one as a list is
 * therefore a parse with a refusal, not a cast, so a row that is not a list
 * fails the read it reached rather than quietly showing no bullets.
 */
const jsonStringListSchema = z.string().transform((raw, ctx) => {
    /*
     * A nullable `jsonb` column reaches the wire as an empty string, not as
     * null: the generated domain member is a plain `std::string`, so the
     * mapper writes nothing for a SQL null. No choices is that empty string.
     */
    if (raw === '') {
        return [];
    }
    let parsed: unknown;
    try {
        parsed = JSON.parse(raw);
    } catch (cause) {
        ctx.addIssue({ code: 'custom', message: 'Not JSON', cause });
        return z.NEVER;
    }
    if (!Array.isArray(parsed) || parsed.some((item) => typeof item !== 'string')) {
        ctx.addIssue({ code: 'custom', message: 'Not a JSON array of strings' });
        return z.NEVER;
    }
    return parsed as string[];
});

/**
 * One input a starting point's form declares.
 *
 * The type is a word rather than a value because one table carries every
 * type, and the default stays text for the same reason: the form reads the
 * type to pick its widget and parses the default with it. A `choice` carries
 * the values it accepts; anything else carries none.
 */
export const seedProfileParameterSchema = z.object({
    name: z.string(),
    label: z.string().default(''),
    data_type: z.string().default('string'),
    choices_json: jsonStringListSchema.nullable().default(null),
    default_value: z.string().default(''),
    is_required: z.boolean().default(false),
    description: z.string().default(''),
    display_order: z.int().default(0),
});

/**
 * A starting point a new tenant is provisioned from.
 *
 * The tenant details are the ones the profile prefills, and an empty one
 * states that the form starts blank there. The bullets are the card's, in
 * the order it shows them. The surrogate id is carried because the steps and
 * the parameters are read by it, and the audit tail and the tenant the row
 * belongs to are not: nothing that reads a starting point may act on them.
 */
export const seedProfileSchema = z.object({
    id: uuidSchema,
    code: z.string(),
    name: z.string(),
    summary: z.string().default(''),
    audience: z.string().default(''),
    bullets_json: jsonStringListSchema.default([]),
    tenant_name: z.string().default(''),
    tenant_code: z.string().default(''),
    tenant_hostname: z.string().default(''),
    admin_username: z.string().default(''),
    admin_email: z.string().default(''),
    inherits_admin_password: z.boolean().default(false),
    force_password_change: z.boolean().default(false),
    display_order: z.int().default(0),
});

/** One page of starting points, and the result the read carries. */
export const listSeedProfilesRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(200).default(50),
    order: z
        .object({
            field: z.string().default(''),
            descending: z.boolean().default(false),
        })
        .default({ field: '', descending: false }),
    filter: z.null().default(null),
}) satisfies z.ZodType<GeneratedListSeedProfilesRequest>;

/**
 * One profile's steps, or its parameters.
 *
 * Every field is stated rather than left out, including the filter, because
 * the server decodes the request into a struct that names them all. The
 * scope and the order are stated and the server's own read decides both: a
 * profile's children are a flat list ordered by the display order the rows
 * carry.
 */
export const listSeedProfileChildrenRequestSchema = z.object({
    seed_profile_id: uuidSchema,
    scope: z.enum(['direct', 'subtree']).default('direct'),
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(200),
    order: z
        .object({
            field: z.string().default(''),
            descending: z.boolean().default(false),
        })
        .default({ field: '', descending: false }),
    filter: z.null().default(null),
});

export const seedProfilePageSchema = z.object({
    result: resultEnvelopeSchema,
    seed_profiles: z.array(seedProfileSchema).default([]),
    total: z.int().nonnegative().default(0),
});

export const seedProfileStepPageSchema = z.object({
    result: resultEnvelopeSchema,
    seed_profile_steps: z.array(seedProfileStepSchema).default([]),
    total: z.int().nonnegative().default(0),
});

export const seedProfileParameterPageSchema = z.object({
    result: resultEnvelopeSchema,
    seed_profile_parameters: z.array(seedProfileParameterSchema).default([]),
    total: z.int().nonnegative().default(0),
});

/**
 * A starting point as the interface reads it.
 *
 * The three reads behind it are composed into one shape, because the screen
 * that chooses a starting point shows the profile, the steps it orders and
 * the form it declares at once, and a browser that joined three replies would
 * be holding the profile's structure itself. The names are camelCase from
 * here on: this is the shape the browser parses, not the wire.
 */
export const seedProfileChoiceSchema = z.object({
    code: z.string(),
    name: z.string(),
    summary: z.string().default(''),
    audience: z.string().default(''),
    bullets: z.array(z.string()).default([]),
    tenant: z.object({
        name: z.string().default(''),
        code: z.string().default(''),
        hostname: z.string().default(''),
        adminUsername: z.string().default(''),
        adminEmail: z.string().default(''),
    }),
    inheritsAdminPassword: z.boolean().default(false),
    forcePasswordChange: z.boolean().default(false),
    order: z.int().default(0),
    steps: z
        .array(
            z.object({
                kind: z.string(),
                order: z.int().default(0),
            }),
        )
        .default([]),
    parameters: z
        .array(
            z.object({
                name: z.string(),
                label: z.string().default(''),
                dataType: z.string().default('string'),
                choices: z.array(z.string()).default([]),
                defaultValue: z.string().default(''),
                required: z.boolean().default(false),
                hint: z.string().default(''),
                order: z.int().default(0),
            }),
        )
        .default([]),
});

export type SeedProfileChoice = z.infer<typeof seedProfileChoiceSchema>;

/**
 * Joins a profile and its two child reads into the shape the interface reads.
 *
 * The children are the server's order, which is the display order they
 * carry. The profile itself is not: its page is ordered by the key, so the
 * caller sorts the joined list by the order the profile declares.
 */
export function toSeedProfileChoice(
    profile: z.infer<typeof seedProfileSchema>,
    steps: readonly z.infer<typeof seedProfileStepSchema>[],
    parameters: readonly z.infer<typeof seedProfileParameterSchema>[],
): SeedProfileChoice {
    return {
        code: profile.code,
        name: profile.name,
        summary: profile.summary,
        audience: profile.audience,
        bullets: profile.bullets_json,
        tenant: {
            name: profile.tenant_name,
            code: profile.tenant_code,
            hostname: profile.tenant_hostname,
            adminUsername: profile.admin_username,
            adminEmail: profile.admin_email,
        },
        inheritsAdminPassword: profile.inherits_admin_password,
        forcePasswordChange: profile.force_password_change,
        order: profile.display_order,
        steps: steps.map((step) => ({ kind: step.step_kind, order: step.display_order })),
        parameters: parameters.map((parameter) => ({
            name: parameter.name,
            label: parameter.label,
            dataType: parameter.data_type,
            choices: parameter.choices_json ?? [],
            defaultValue: parameter.default_value,
            required: parameter.is_required,
            hint: parameter.description,
            order: parameter.display_order,
        })),
    };
}

/** The body of the browser's starting-point read. */
export const seedProfilesResponseSchema = z.object({
    profiles: z.array(seedProfileChoiceSchema).default([]),
});

export type SeedProfilesResponse = z.infer<typeof seedProfilesResponseSchema>;

/**
 * One legal entity a deployment can start a tenant from, as a screen reads it.
 *
 * The read behind it matches the root legal entities the deployment holds
 * against what a person typed, and this is the part of that answer a screen
 * needs: the LEI that names the entity, the name a person recognises it by, the
 * country it is registered in, and how many parties its hierarchy would create,
 * which is the work choosing it starts.
 */
export const leiEntityChoiceSchema = z.object({
    lei: z.string().default(''),
    legalName: z.string().default(''),
    country: z.string().default(''),
    partyCount: z.number().default(0),
});

export type LeiEntityChoice = z.infer<typeof leiEntityChoiceSchema>;

/** The body of the browser's legal-entity read. */
export const leiEntitiesResponseSchema = z.object({
    entities: z.array(leiEntityChoiceSchema).default([]),
});

export type LeiEntitiesResponse = z.infer<typeof leiEntitiesResponseSchema>;

/** The server's own answer, before it is read in the interface's terms. */
export const leiEntitySummaryResponseSchema = z.object({
    success: z.boolean().default(false),
    error_message: z.string().default(''),
    entities: z
        .array(
            z.object({
                lei: z.string().default(''),
                entity_legal_name: z.string().default(''),
                entity_category: z.string().default(''),
                country: z.string().default(''),
            }),
        )
        .default([]),
});

export type LeiEntitySummaryResponse = z.infer<typeof leiEntitySummaryResponseSchema>;

/** The server's answer to a legal-entity search, before it is read. */
export const searchLeiEntitiesResponseSchema = z.object({
    success: z.boolean().default(false),
    error_message: z.string().default(''),
    entities: z
        .array(
            z.object({
                lei: z.string().default(''),
                entity_legal_name: z.string().default(''),
                entity_category: z.string().default(''),
                country: z.string().default(''),
                party_count: z.number().default(0),
            }),
        )
        .default([]),
});

export type SearchLeiEntitiesResponse = z.infer<typeof searchLeiEntitiesResponseSchema>;

/**
 * A party of the caller's tenant, as a screen or a route reads it.
 *
 * The identifier the parent column carries is the one that decides where the
 * party sits: a party hangs under another, and exactly one party of a tenant
 * sits at the top. The audit tail and the image are not here, because nothing
 * that reads a party to place it may act on them.
 */
const optionalUuidSchema = z
    .string()
    .nullish()
    .transform((value) => (value === null || value === undefined || value === '' ? null : value));

export const partyWireRowSchema = z.object({
    id: uuidSchema,
    short_code: z.string().default(''),
    full_name: z.string().default(''),
    party_category: z.string().default(''),
    party_type: z.string().default(''),
    parent_party_id: optionalUuidSchema,
    business_center_code: z.string().default(''),
    status: z.string().default(''),
});

export type PartyRow = z.infer<typeof partyWireRowSchema>;

/** The server's answer to a party page read, before it is read in one's terms. */
export const listPartiesReplySchema = z.object({
    result: resultEnvelopeSchema,
    parties: z.array(partyWireRowSchema).default([]),
    total: z.int().nonnegative().default(0),
});

/**
 * One party a party journey needs placed, and the request that places it.
 *
 * A party write states what the row is and what the writer believes about it.
 * A party being added believes nothing exists yet, which is the precondition
 * that makes a second attempt at the same short code a refusal rather than a
 * second row.
 */
export function toPutPartyChange(input: {
    readonly id: string;
    readonly shortCode: string;
    readonly fullName: string;
    readonly parentPartyId: string | null;
}): PartyChange {
    return {
        write: {
            id: input.id,
            short_code: input.shortCode,
            full_name: input.fullName,
            /*
             * Left blank so the server generates it: the codename is the
             * party's queue prefix and is immutable once assigned, which is a
             * rule about the deployment and not about this screen.
             */
            codename: '',
            transliterated_name: null,
            /* A party a person adds is an operational one, never the system's. */
            party_category: 'Operational',
            party_type: 'Corporate',
            parent_party_id: input.parentPartyId,
            /* The global sentinel business centre, which every tenant seeds. */
            business_center_code: 'WRLD',
            /*
             * Inactive until the run reaches its activate step. A party born
             * active would make that step a step that does nothing.
             */
            status: 'Inactive',
            image_id: null,
            /*
             * A party a person adds is never the place registrations land:
             * the tenant administrator nominates that one deliberately, on
             * the tenant's own party.
             */
            is_registration_default: false,
        },
        precondition: { kind: 'must_not_exist', version: null },
    };
}

/** What a party write answered, as the interface reads it. */
export const putPartyResultSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    partyId: z.string().default(''),
});

export type PutPartyResult = z.infer<typeof putPartyResultSchema>;

/** The server's own answer to a party write, before it is read as a result. */
export const putPartyReplySchema = z.object({
    result: resultEnvelopeSchema,
    party: partyWireRowSchema.nullish(),
});

/** The interface's result, read from the server's answer. */
export function toPutPartyResult(
    reply: z.infer<typeof putPartyReplySchema>,
    partyId: string,
): PutPartyResult {
    return {
        success: reply.result.outcome === 'ok',
        message: reply.result.message,
        partyId: reply.result.outcome === 'ok' ? partyId : '',
    };
}

/**
 * A request to add one party of the caller's own tenant.
 *
 * The legal name is what the party is called and the short code is what people
 * type to reach it. The LEI is carried only when the party was built from a
 * legal entity the deployment holds: a party that is not one of them has no
 * LEI, and an empty value is what says so rather than a flag beside it.
 *
 * The starting point is not here. A party's data is the deployment's own, the
 * profiles that state it are the system tenant's rows, and a tenant
 * administrator reads only its own; the service that holds both is the one that
 * decides which stage a party is published by.
 */
export const provisionPartyRequestSchema = z.object({
    fullName: z.string().min(1),
    shortCode: z.string().min(1),
    lei: z.string().default(''),
});

export type ProvisionPartyRequest = z.infer<typeof provisionPartyRequestSchema>;

/**
 * What adding a party answered, as the interface reads it.
 *
 * The party exists and the run that publishes its data exists by the time this
 * answers, so both identifiers travel: the party is what the screen names and
 * the run is what it follows.
 */
export const provisionPartyResultSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instanceId: z.string().default(''),
    partyId: z.string().default(''),
});

export type ProvisionPartyResult = z.infer<typeof provisionPartyResultSchema>;

/** The server's own answer to the party stage, before it is read as a result. */
export const provisionPartyReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instance_id: z.string().default(''),
    party_id: z.string().default(''),
});

/**
 * The wire command one party-stage request becomes.
 *
 * The LEI travels with it when the party was built from an entity the
 * deployment holds, and the run records it against the party: a party
 * identifier carries the party its writing session acts in, so the person who
 * adds a party cannot write it from the session they are in.
 */
export function toProvisionPartyCommand(input: {
    readonly party: string;
    readonly profileCode: string;
    readonly lei: string;
}): ProvisionPartyCommand {
    return { party: input.party, profile_code: input.profileCode, lei: input.lei };
}

/** The interface's result, read from the server's answer. */
export function toProvisionPartyResult(
    reply: z.infer<typeof provisionPartyReplySchema>,
): ProvisionPartyResult {
    return {
        success: reply.success,
        message: reply.message,
        instanceId: reply.instance_id,
        partyId: reply.party_id,
    };
}

/**
 * A request to provision one tenant from a starting point.
 *
 * The profile's parameters travel as a list of `name=value` entries, because
 * the shell fills them from one command line; the browser holds them as a map
 * keyed by parameter name, which is what its form produces.
 * `toProvisionTenantCommand` is the one place the two meet.
 *
 * The tenant's type is not here. It is the profile's, and a request that named
 * it would be a second starting point beside the one the person chose.
 */
export const provisionTenantRequestSchema = z.object({
    profileCode: z.string().min(1),
    tenantCode: z.string().min(1),
    tenantName: z.string().min(1),
    tenantHostname: z.string().min(1),
    tenantDescription: z.string().default(''),
    adminUsername: z.string().min(1),
    adminEmail: z.string().min(1),
    adminPassword: z.string().min(1),
    parameters: z.record(z.string(), z.string()).default({}),
});

export type ProvisionTenantRequest = z.infer<typeof provisionTenantRequestSchema>;

/**
 * What the provision verb answered, as the interface reads it.
 *
 * The instance id is what the journey follows: the steps the profile orders run
 * after the answer, and the progress read names them by this id.
 *
 * `success` defaults to false, so a reply that arrives without the field reads
 * as a failure the caller can see rather than a success nobody has checked.
 */
export const provisionTenantResultSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instanceId: z.string().default(''),
    tenantId: z.string().default(''),
    accountId: z.string().default(''),
});

export type ProvisionTenantResult = z.infer<typeof provisionTenantResultSchema>;

/** The server's own answer to the provision verb, before it is read as a result. */
export const provisionTenantReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instance_id: z.string().default(''),
    tenant_id: z.string().default(''),
    account_id: z.string().default(''),
});

/** The wire command one request becomes. */
export function toProvisionTenantCommand(request: ProvisionTenantRequest): ProvisionTenantCommand {
    return {
        profile_code: request.profileCode,
        tenant_code: request.tenantCode,
        tenant_name: request.tenantName,
        tenant_hostname: request.tenantHostname,
        tenant_description: request.tenantDescription,
        admin_username: request.adminUsername,
        admin_email: request.adminEmail,
        admin_password: request.adminPassword,
        parameters: Object.entries(request.parameters).map(([name, value]) => `${name}=${value}`),
    };
}

/** The interface's result, read from the server's answer. */
export function toProvisionTenantResult(
    reply: z.infer<typeof provisionTenantReplySchema>,
): ProvisionTenantResult {
    return {
        success: reply.success,
        message: reply.message,
        instanceId: reply.instance_id,
        tenantId: reply.tenant_id,
        accountId: reply.account_id,
    };
}

/**
 * One line of the rail a person watching a run reads.
 *
 * The engine's own summary of a step, mirrored: the operation's name, the
 * state it is in, the error it carries when it failed, and one log entry per
 * item it could not do cleanly. `status` is the state's name rather than an
 * id, because the state machine's names are what the server answers and a
 * client that mapped them would be a second place for the machine to live.
 */
export const workflowStepSummarySchema = z.object({
    id: z.string().default(''),
    name: z.string().default(''),
    /*
     * The step's name and description in a person's words, as the run's own
     * definition declared them. Both default to empty, because an instance
     * started before a step had words carries none and the name stands in.
     */
    label: z.string().default(''),
    description: z.string().default(''),
    status: z.string().default(''),
    step_index: z.number().int().default(0),
    created_at: z.string().default(''),
    started_at: z.string().nullable().default(null),
    completed_at: z.string().nullable().default(null),
    error: z.string().default(''),
    log: z
        .array(
            z.object({
                level: z.string().default('info'),
                message: z.string().default(''),
                context: z.string().default(''),
            }),
        )
        .default([]),
}) satisfies z.ZodType<WireWorkflowStepSummary>;

export type WorkflowStepSummary = z.infer<typeof workflowStepSummarySchema>;

/**
 * A run as the progress read answers it.
 *
 * `status` names the run's state, `current_step_index` says which step it is
 * executing, `step_count` says how many it declared, and `steps` holds one
 * summary per step the run has materialised. `success` defaults to false, so
 * an answer that arrives without it reads as a failure the caller can see.
 */
export const workflowProgressSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    status: z.string().default(''),
    error: z.string().default(''),
    step_count: z.number().int().default(0),
    current_step_index: z.number().int().default(0),
    steps: z.array(workflowStepSummarySchema).default([]),
}) satisfies z.ZodType<GetWorkflowStepsResponse>;

export type WorkflowProgress = z.infer<typeof workflowProgressSchema>;

/** The wire request the progress read takes, as the generated type states it. */
export function toGetWorkflowStepsRequest(instanceId: string): GetWorkflowStepsRequest {
    return { workflow_instance_id: instanceId };
}

/**
 * A retry, as the interface asks for one.
 *
 * The step is named only when a person chooses to resume somewhere other than
 * where the run stopped; an empty name means the step that failed.
 */
export const retryWorkflowInstanceRequestSchema = z.object({
    workflowInstanceId: z.string().min(1),
    stepName: z.string().default(''),
});

export type RetryWorkflowInstanceRequest = z.infer<typeof retryWorkflowInstanceRequestSchema>;

/** What the retry answered, as the interface reads it. */
export const retryWorkflowInstanceResultSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    instanceId: z.string().default(''),
    stepIndex: z.number().int().default(-1),
    stepName: z.string().default(''),
});

export type RetryWorkflowInstanceResult = z.infer<typeof retryWorkflowInstanceResultSchema>;

/** The server's own answer to the retry, before it is read as a result. */
export const retryWorkflowInstanceReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    workflow_instance_id: z.string().default(''),
    step_index: z.number().int().default(-1),
    step_name: z.string().default(''),
}) satisfies z.ZodType<RetryWorkflowInstanceResponse>;

/** The wire command one retry request becomes. */
export function toRetryWorkflowInstanceCommand(
    request: RetryWorkflowInstanceRequest,
): WireRetryWorkflowInstanceRequest {
    return {
        workflow_instance_id: request.workflowInstanceId,
        step_name: request.stepName,
    };
}

/** The interface's result, read from the server's answer. */
export function toRetryWorkflowInstanceResult(
    reply: z.infer<typeof retryWorkflowInstanceReplySchema>,
): RetryWorkflowInstanceResult {
    return {
        success: reply.success,
        message: reply.message,
        instanceId: reply.workflow_instance_id,
        stepIndex: reply.step_index,
        stepName: reply.step_name,
    };
}

/**
 * The rules a password must satisfy, as the server states them.
 *
 * A screen shows these rules before anybody has signed in, so the read needs
 * no session. The rules live in the server's validator and nowhere else: a
 * client that states them states the server's record rather than a copy of it.
 */
export const passwordPolicySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    minLength: z.number().int().default(0),
    requireUppercase: z.boolean().default(false),
    requireLowercase: z.boolean().default(false),
    requireDigit: z.boolean().default(false),
    requireSpecial: z.boolean().default(false),
    specialChars: z.string().default(''),
});

export type PasswordPolicy = z.infer<typeof passwordPolicySchema>;

/** The server's own answer, before it is read in the interface's terms. */
export const passwordPolicyReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    min_length: z.number().int().default(0),
    require_uppercase: z.boolean().default(false),
    require_lowercase: z.boolean().default(false),
    require_digit: z.boolean().default(false),
    require_special: z.boolean().default(false),
    special_chars: z.string().default(''),
});

/** The interface's record, read from the server's answer. */
export function toPasswordPolicy(reply: z.infer<typeof passwordPolicyReplySchema>): PasswordPolicy {
    return {
        success: reply.success,
        message: reply.message,
        minLength: reply.min_length,
        requireUppercase: reply.require_uppercase,
        requireLowercase: reply.require_lowercase,
        requireDigit: reply.require_digit,
        requireSpecial: reply.require_special,
        specialChars: reply.special_chars,
    };
}

/**
 * What the deployment offers somebody who is not in it yet.
 *
 * The door asks this before it offers a form, because a deployment that refuses
 * registrations should say so rather than accept a form and refuse it. The
 * tenant is resolved from the address the request arrived at, so it is a field
 * of the request rather than something the person types.
 */
export const registrationPolicyRequestSchema = z.object({
    hostname: z.string().default(''),
});

export const registrationPolicyReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    error_code: z.string().default(''),
    signups_enabled: z.boolean().default(false),
    authorization_required: z.boolean().default(false),
    tenant_id: z.string().default(''),
    tenant_name: z.string().default(''),
    party_id: z.string().default(''),
    party_name: z.string().default(''),
    role_id: z.string().default(''),
    role_name: z.string().default(''),
    usable_now: z.boolean().default(false),
});

export type RegistrationPolicy = {
    readonly success: boolean;
    readonly message: string;
    readonly errorCode: string;
    readonly signupsEnabled: boolean;
    readonly authorizationRequired: boolean;
    readonly tenantId: string;
    readonly tenantName: string;
    readonly partyId: string;
    readonly partyName: string;
    readonly roleId: string;
    readonly roleName: string;
    readonly usableNow: boolean;
};

export function toRegistrationPolicy(
    reply: z.infer<typeof registrationPolicyReplySchema>,
): RegistrationPolicy {
    return {
        success: reply.success,
        message: reply.message,
        errorCode: reply.error_code,
        signupsEnabled: reply.signups_enabled,
        authorizationRequired: reply.authorization_required,
        tenantId: reply.tenant_id,
        tenantName: reply.tenant_name,
        partyId: reply.party_id,
        partyName: reply.party_name,
        roleId: reply.role_id,
        roleName: reply.role_name,
        usableNow: reply.usable_now,
    };
}

/** A registration to make, with the address the person arrived at. */
export const signupCommandSchema = z.object({
    principal: z.string().default(''),
    password: z.string().default(''),
    email: z.string().default(''),
    hostname: z.string().default(''),
});

export const signupReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    error_code: z.string().default(''),
    account_id: z.string().default(''),
    account_status: z.string().default(''),
    party_id: z.string().default(''),
    role_id: z.string().default(''),
});

export type SignupOutcome = {
    readonly success: boolean;
    readonly message: string;
    readonly errorCode: string;
    readonly accountId: string;
    /** The state the account was created in: `active` or `pending`. */
    readonly accountStatus: string;
    readonly partyId: string;
    readonly roleId: string;
};

export function toSignupOutcome(reply: z.infer<typeof signupReplySchema>): SignupOutcome {
    return {
        success: reply.success,
        message: reply.message,
        errorCode: reply.error_code,
        accountId: reply.account_id,
        accountStatus: reply.account_status,
        partyId: reply.party_id,
        roleId: reply.role_id,
    };
}
