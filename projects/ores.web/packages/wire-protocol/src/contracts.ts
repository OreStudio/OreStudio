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
    activePartySchema,
    partySummarySchema,
    tenantStatusSchema,
    tenantTypeSchema,
} from './domain.js';

/**
 * The HTTP contract between the BFF and the browser.
 *
 * Both sides import these schemas, so the browser parses what the BFF sent
 * with the same definition the BFF serialised it from. That removes the usual
 * hand-written mirror of an API shape, which cannot be checked by the
 * compiler across a network boundary.
 *
 * The session never carries the bearer token. The browser holds an opaque
 * cookie instead, and the token stays on the server.
 */

/**
 * The context a session runs in.
 *
 * The server decides it, and the browser reads it. A super administrator works
 * in the system tenant's context, a tenant administrator in a tenant's, and a
 * party user in a party's, and the server already scopes every request by that
 * context: stating it is what stops the shell having to guess it, and what
 * keeps the menu from disagreeing with the data.
 *
 * The difference between a privileged and a regular party user is not a mode.
 * They share the application context, and what separates them is the
 * permissions they hold.
 */
export const sessionModeSchema = z.enum([
    'system-administration',
    'tenant-administration',
    'application',
]);
export type SessionMode = z.infer<typeof sessionModeSchema>;

/**
 * The tenant lifecycle statuses, with the badge each one is painted with.
 *
 * The words come from the status row and the colours from the badge, so a
 * screen showing a tenant's status reads one list rather than joining two.
 */
export const tenantStatusesResponseSchema = z.object({
    statuses: z.array(tenantStatusSchema),
});
export type TenantStatusesResponse = z.infer<typeof tenantStatusesResponseSchema>;

/** The tenant types, with the badge each one is painted with. */
export const tenantTypesResponseSchema = z.object({
    types: z.array(tenantTypeSchema),
});
export type TenantTypesResponse = z.infer<typeof tenantTypesResponseSchema>;

/**
 * The database the deployment stores into, as the login answer stated it.
 *
 * The row travels with the build the answer already states: one reader of
 * =ores_database_info_tbl= fills it at login, the BFF keeps it on the session,
 * and the versions panel reads it from the answer that opened the session
 * rather than asking again. An empty row is a database whose record could not
 * be read, which the screen states as unknown.
 */
export const databaseInfoSchema = z.object({
    /** The hash of the SQL scripts the database was built from. */
    fingerprint: z.string(),
    /** The build environment the database was built in. */
    environment: z.string(),
    /** The git commit the database was built from. */
    commit: z.string(),
    /** When the database was created or recreated. */
    created: z.string(),
});
export type DatabaseInfo = z.infer<typeof databaseInfoSchema>;

/** The signed-in session as the browser sees it. */
export const sessionViewSchema = z.object({
    username: z.string(),
    email: z.string(),
    accountId: z.string(),
    tenantId: z.string(),
    tenantName: z.string(),
    /** The context the session runs in. Stated by the server, never inferred. */
    mode: sessionModeSchema,
    /** The build the session was opened against, as the server stated it. */
    version: z.string(),
    /** The database the login answer carried, beside the build it states. */
    database: databaseInfoSchema,
    party: partySummarySchema,
    availableParties: z.array(partySummarySchema),
    /** Seconds the token remains valid for, so the browser can renew early. */
    accessLifetimeSeconds: z.int().positive(),
    passwordResetRequired: z.boolean(),
});
export type SessionView = z.infer<typeof sessionViewSchema>;

/**
 * Returned when the credential was accepted but a party is still required.
 *
 * A distinct outcome rather than an error, because the next step is a normal
 * action rather than a failure to recover from.
 */
export const partyChoiceSchema = z.object({
    outcome: z.literal('party-required'),
    username: z.string(),
    email: z.string(),
    accountId: z.string(),
    tenantName: z.string(),
    /** The build the login was answered by. */
    version: z.string(),
    /** The database the login answer carried, beside the build it states. */
    database: databaseInfoSchema,
    availableParties: z.array(partySummarySchema),
    defaultPartyId: z.string().nullable(),
    passwordResetRequired: z.boolean(),
});
export type PartyChoice = z.infer<typeof partyChoiceSchema>;

export const loginSuccessSchema = z.object({
    outcome: z.literal('active'),
    session: sessionViewSchema,
});
export type LoginSuccess = z.infer<typeof loginSuccessSchema>;

/** The union a login attempt resolves to. */
export const loginResultSchema = z.discriminatedUnion('outcome', [
    loginSuccessSchema,
    partyChoiceSchema,
]);
export type LoginResult = z.infer<typeof loginResultSchema>;

/**
 * Whether the deployment is still waiting to be set up.
 *
 * The interface asks this before it offers a sign-in, because a deployment in
 * bootstrap mode has no accounts to sign in with and a rejected credential
 * would send somebody hunting for a password that cannot exist. Two further
 * facts say whether the setup job is finished: whether the deployment has a
 * tenant of its own, and whether the system provisioner wizard recorded that it
 * finished. A first-run installation may keep only the system tenant, so the
 * tenant answer alone would hold it on the setup screen forever. The message is
 * the server's; the interface may state the situation in its own words.
 */
export const bootstrapStatusSchema = z.object({
    isInBootstrapMode: z.boolean(),
    /** Whether the deployment has a tenant of its own, the system one aside. */
    hasTenant: z.boolean(),
    /** Whether the system provisioner wizard recorded that it finished. */
    onboardingComplete: z.boolean(),
    message: z.string(),
    /** The build the deployment runs, as the deployment states it. */
    version: z.string(),
});
export type BootstrapStatus = z.infer<typeof bootstrapStatusSchema>;

/**
 * Creating the first administrator.
 *
 * The one request the browser sends with no session, because the deployment has
 * no account to sign in with. What comes back is the account, not a session:
 * the person signs in with it next, which is the point of the setup screen.
 */
export const createAdministratorRequestSchema = z.object({
    principal: z.string().min(1),
    password: z.string().min(1),
    email: z.string().min(1),
});
export type CreateAdministratorRequest = z.infer<typeof createAdministratorRequestSchema>;

export const initialAdministratorSchema = z.object({
    accountId: z.string(),
    tenantId: z.string(),
});
export type InitialAdministrator = z.infer<typeof initialAdministratorSchema>;

/**
 * Where to connect, and who to connect as.
 *
 * The endpoint comes from the connections store rather than from this server's
 * configuration, because a person chooses an environment on the sign-in screen.
 * The password is omitted when a saved connection is used, in which case the
 * server resolves the stored credential and the browser never handles it.
 */
export const loginRequestSchema = z.object({
    username: z.string().min(1),
    password: z.string().default(''),
    /** The NATS server to sign in to. */
    server: z.string().min(1),
    port: z.int().min(1).max(65535),
    /** The subject namespace, which isolates one environment on a shared broker. */
    subjectPrefix: z.string().default(''),
    /** The saved connection being used, when one was chosen. */
    connectionId: z.string().default(''),
});
export type LoginRequest = z.infer<typeof loginRequestSchema>;

export const selectPartyRequestSchema = z.object({
    partyId: z.string().min(1),
});
export type SelectPartyRequest = z.infer<typeof selectPartyRequestSchema>;

/**
 * A failure the browser can show.
 *
 * `code` is the stable part a caller branches on; `message` is for a human and
 * may change.
 */
export const apiErrorSchema = z.object({
    code: z.enum([
        'invalid-credentials',
        'not-authenticated',
        'session-expired',
        'forbidden',
        'conflict',
        'invalid-request',
        'not-found',
        'bootstrap-mode',
        'bootstrap-complete',
        'signups-disabled',
        'signup-requires-authorization',
        'no-registration-destination',
        'no-default-role',
        'username-taken',
        'email-taken',
        'weak-password',
        'signup-refused',
        'too-many-requests',
        'upstream-unavailable',
        'upstream-timeout',
        'internal',
    ]),
    message: z.string(),
});
export type ApiError = z.infer<typeof apiErrorSchema>;

/**
 * What the deployment offers somebody who is not in it yet.
 *
 * The door reads this before it offers a form, so the answer is a value rather
 * than an error: a deployment that refuses registrations is a state the screen
 * states, not a failure it recovers from. `errorCode` carries the refusal when
 * there is one, and `usableNow` says whether a registration can sign in without
 * an administrator.
 */
export const registrationPolicyViewSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    errorCode: z.string(),
    signupsEnabled: z.boolean(),
    authorizationRequired: z.boolean(),
    tenantId: z.string(),
    tenantName: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    roleId: z.string(),
    roleName: z.string(),
    usableNow: z.boolean(),
});
export type RegistrationPolicyView = z.infer<typeof registrationPolicyViewSchema>;

/**
 * A registration.
 *
 * The address the person arrived at is not a field here: the BFF forwards the
 * hostname the request was served at, because the tenant is resolved from the
 * address rather than typed into the form.
 */
export const signupRequestSchema = z.object({
    principal: z.string().min(1),
    password: z.string().min(1),
    email: z.string().min(1),
});
export type SignupRequest = z.infer<typeof signupRequestSchema>;

/** What a registration produced: the account, and what it waits for. */
export const signupResultSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    errorCode: z.string(),
    accountId: z.string(),
    /** `active` when it can sign in at once, `pending` when it cannot. */
    accountStatus: z.string(),
    partyId: z.string(),
    roleId: z.string(),
});
export type SignupResult = z.infer<typeof signupResultSchema>;

/** The account's active party, as the handover carries it. */
export { activePartySchema };

export const sseEnvelopeSchema = z.object({
    event: z.enum(['connected', 'party-changed', 'session-expired', 'account-changed']),
    data: z.unknown(),
});
export type SseEnvelope = z.infer<typeof sseEnvelopeSchema>;
