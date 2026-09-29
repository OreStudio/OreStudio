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
import { activePartySchema, partySummarySchema } from './domain.js';

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

/** The signed-in session as the browser sees it. */
export const sessionViewSchema = z.object({
    username: z.string(),
    email: z.string(),
    accountId: z.string(),
    tenantId: z.string(),
    tenantName: z.string(),
    /** The build the session was opened against, as the server stated it. */
    version: z.string(),
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
 * Whether the deployment is still waiting for its first administrator.
 *
 * The interface asks this before it offers a sign-in, because a deployment in
 * bootstrap mode has no accounts to sign in with and a rejected credential
 * would send somebody hunting for a password that cannot exist. The message is
 * the server's; the interface may state the situation in its own words.
 */
export const bootstrapStatusSchema = z.object({
    isInBootstrapMode: z.boolean(),
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
        'invalid-request',
        'bootstrap-mode',
        'bootstrap-complete',
        'upstream-unavailable',
        'upstream-timeout',
        'internal',
    ]),
    message: z.string(),
});
export type ApiError = z.infer<typeof apiErrorSchema>;

/** The account's active party, as the handover carries it. */
export { activePartySchema };

export const sseEnvelopeSchema = z.object({
    event: z.enum(['connected', 'party-changed', 'session-expired', 'account-changed']),
    data: z.unknown(),
});
export type SseEnvelope = z.infer<typeof sseEnvelopeSchema>;
