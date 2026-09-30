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
    bootstrapStatusSchema,
    initialAdministratorSchema,
    loginResultSchema,
    leiEntitiesResponseSchema,
    passwordPolicySchema,
    provisionPartyResultSchema,
    provisionTenantResultSchema,
    registrationPolicyViewSchema,
    retryWorkflowInstanceResultSchema,
    seedProfilesResponseSchema,
    sessionViewSchema,
    signupResultSchema,
    workflowProgressSchema,
    type BootstrapStatus,
    type CreateAdministratorRequest,
    type InitialAdministrator,
    type LeiEntityChoice,
    type LoginResult,
    type PasswordPolicy,
    type ProvisionPartyRequest,
    type ProvisionPartyResult,
    type ProvisionTenantRequest,
    type ProvisionTenantResult,
    type RegistrationPolicyView,
    type RetryWorkflowInstanceResult,
    type SeedProfileChoice,
    type SessionView,
    type SignupRequest,
    type SignupResult,
    type WorkflowProgress,
} from '@ores/wire-protocol/browser';
import { ApiFailure, request } from './transport.js';

/**
 * The session and account calls.
 *
 * These run after a connection has been chosen and a credential accepted. The
 * browser holds an opaque cookie rather than a token, so it cannot decide
 * locally whether it is signed in and asks instead.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

export interface Credentials {
    readonly username: string;
    readonly password: string;
}

export const api = {
    /**
     * Whether the deployment still needs its first administrator.
     *
     * Asked before a session exists, because it decides what the interface can
     * offer: while the flag is set there are no accounts, so a sign-in form
     * would only be a door with nothing behind it.
     */
    async bootstrapStatus(): Promise<BootstrapStatus> {
        return bootstrapStatusSchema.parse(await request('/api/bootstrap', { method: 'GET' }));
    },

    /**
     * Creates the first administrator, which closes bootstrap mode.
     *
     * The one write sent with no session, because the deployment has no account
     * to sign in with. What comes back is the account rather than a session:
     * the person signs in with it next.
     */
    async createAdministrator(request_: CreateAdministratorRequest): Promise<InitialAdministrator> {
        const payload = await request('/api/bootstrap/administrator', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(request_),
        });
        return initialAdministratorSchema.parse(payload);
    },

    async login(credentials: Credentials): Promise<LoginResult> {
        const payload = await request('/api/session', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(credentials),
        });
        return loginResultSchema.parse(payload);
    },

    async selectParty(partyId: string): Promise<SessionView> {
        const payload = await request('/api/session/party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ partyId }),
        });
        return sessionViewSchema.parse(payload);
    },

    /** Returns null when no session is open, which is not an error. */
    async session(): Promise<SessionView | null> {
        try {
            return sessionViewSchema.parse(await request('/api/session', { method: 'GET' }));
        } catch (error) {
            if (error instanceof ApiFailure && error.status === 401) {
                return null;
            }
            throw error;
        }
    },

    async logout(): Promise<void> {
        await request('/api/session', { method: 'DELETE' });
    },

    /**
     * The rules a password must satisfy.
     *
     * Asked for before a session exists, because the screens that show the
     * rules are the ones a person signs in on and the one that creates the
     * first administrator. The answer is the server's own policy, so a screen
     * that shows it shows the rules the server applies.
     */
    async passwordPolicy(): Promise<PasswordPolicy> {
        return passwordPolicySchema.parse(await request('/api/password-policy', { method: 'GET' }));
    },

    /**
     * What the deployment offers somebody who is not in it yet.
     *
     * Asked before the door offers a form, because a deployment that refuses
     * registrations should say so rather than accept a form and refuse it. The
     * answer is a value rather than an error: a closed door is a state the
     * screen renders, and the code beside it says which closure it is.
     */
    async registrationPolicy(): Promise<RegistrationPolicyView> {
        return registrationPolicyViewSchema.parse(
            await request('/api/registration-policy', { method: 'GET' }),
        );
    },

    /**
     * Registers an account.
     *
     * The address the person arrived at is not sent: the BFF forwards the
     * hostname the request was served at, because the tenant is resolved from
     * the address rather than typed into the form.
     */
    async signup(input: SignupRequest): Promise<SignupResult> {
        const payload = await request('/api/signup', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return signupResultSchema.parse(payload);
    },

    /**
     * The root legal entities a tenant can be started from, matched against
     * what a person typed.
     *
     * The matching is the read's, because the deployment holds far more
     * entities than one answer can carry: a screen sends the text rather than
     * fetching a page and filtering it here.
     */
    async leiEntities(search: string): Promise<readonly LeiEntityChoice[]> {
        const query = new URLSearchParams({ search });
        const payload = leiEntitiesResponseSchema.parse(
            await request(`/api/lei-entities?${query.toString()}`, { method: 'GET' }),
        );
        return payload.entities;
    },

    /** The starting points a new tenant may be provisioned from. */
    async seedProfiles(): Promise<readonly SeedProfileChoice[]> {
        const payload = seedProfilesResponseSchema.parse(
            await request('/api/seed-profiles', { method: 'GET' }),
        );
        return payload.profiles;
    },

    /**
     * Creates a tenant from a starting point.
     *
     * The tenant and its administrator exist when this answers, and the run
     * that provisions the rest is followed by the id it carries rather than by
     * waiting here.
     */
    async provisionTenant(input: ProvisionTenantRequest): Promise<ProvisionTenantResult> {
        const payload = await request('/api/provision-tenant', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return provisionTenantResultSchema.parse(payload);
    },

    /**
     * Re-scopes the open session to another of the account's parties.
     *
     * A party added during this session is not in the list the login answered
     * with, so the server reads the tenant's parties and states the chosen one
     * in the session rather than taking the caller's word for it.
     */
    async switchParty(partyId: string): Promise<SessionView> {
        const payload = await request('/api/session/switch-party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ partyId }),
        });
        return sessionViewSchema.parse(payload);
    },

    /**
     * Adds a party to the tenant the person works in.
     *
     * The party exists when this answers, and the run that publishes its data,
     * activates it and joins the person to it is followed by the id it carries.
     */
    async provisionParty(input: ProvisionPartyRequest): Promise<ProvisionPartyResult> {
        const payload = await request('/api/provision-party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return provisionPartyResultSchema.parse(payload);
    },

    /** The state of a provisioning run, as its journey's rail renders it. */
    async provisionTenantProgress(instanceId: string): Promise<WorkflowProgress> {
        return workflowProgressSchema.parse(
            await request(`/api/workflow/${encodeURIComponent(instanceId)}`, {
                method: 'GET',
            }),
        );
    },

    /**
     * Resumes a stopped run from the step that failed.
     *
     * A refusal is an answer rather than a failure: a run that has not stopped,
     * or a step it does not hold, is something the person asking can see.
     */
    async retryProvisionTenant(
        instanceId: string,
        stepName = '',
    ): Promise<RetryWorkflowInstanceResult> {
        const payload = await request(`/api/workflow/${encodeURIComponent(instanceId)}/retry`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ stepName }),
        });
        return retryWorkflowInstanceResultSchema.parse(payload);
    },

    /**
     * Sets a password of the signed-in account's own.
     *
     * The current password travels with the request, because the account is
     * changing a credential it holds rather than one an administrator issued.
     */
    async changePassword(currentPassword: string, newPassword: string): Promise<void> {
        await request('/api/account/password', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ currentPassword, newPassword }),
        });
    },
};

export { ApiFailure };
