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
    accountAccessSchema,
    permissionEntrySchema,
    roleSummarySchema,
    type AccountAccess,
    type PermissionEntry,
    type RoleSummary,
    accountSchema,
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
    partyPageSchema,
    tenantDetailResponseSchema,
    tenantPageSchema,
    deploymentOverviewSchema,
    tenantStatusesResponseSchema,
    tenantTypesResponseSchema,
    workflowProgressSchema,
    loginInfoSchema,
    sessionSchema,
    type Account,
    type BootstrapStatus,
    type CreateAdministratorRequest,
    type InitialAdministrator,
    type LeiEntityChoice,
    type LoginInfo,
    type LoginResult,
    type Session,
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
    type PartyPage,
    type TenantDetailResponse,
    type TenantPage,
    type DeploymentOverview,
    type TenantStatus,
    type TenantType,
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
    /**
     * The accounts the tenant's administrator may see.
     *
     * The administrator's screen opens on this list, and the count travels with
     * it so the screen can say how many the tenant holds without reading them.
     */
    async accounts(): Promise<{
        readonly accounts: readonly Account[];
        readonly totalCount: number;
    }> {
        return z
            .object({ accounts: z.array(accountSchema), totalCount: z.int().nonnegative() })
            .parse(await request('/api/accounts', { method: 'GET' }));
    },

    /** The reasons a role may be given for: the access category, in display order. */
    async accessReasons(): Promise<
        readonly { readonly code: string; readonly description: string }[]
    > {
        const answer = z
            .object({
                reasons: z.array(
                    z.object({
                        code: z.string(),
                        description: z.string(),
                        categoryCode: z.string(),
                        appliesToNew: z.boolean(),
                        displayOrder: z.number(),
                    }),
                ),
            })
            .parse(await request('/api/change-reasons', { method: 'GET' }));
        return answer.reasons
            .filter((reason) => reason.categoryCode === 'access' && reason.appliesToNew)
            .sort((a, b) => a.displayOrder - b.displayOrder)
            .map(({ code, description }) => ({ code, description }));
    },

    /** The roles the signed-in person holds, with who gave each one and why. */
    async myAccess(): Promise<AccountAccess> {
        return accountAccessSchema.parse(await request('/api/me/access', { method: 'GET' }));
    },

    /** The roles one account holds. */
    async accountAccess(accountId: string): Promise<AccountAccess> {
        return accountAccessSchema.parse(
            await request(`/api/accounts/${encodeURIComponent(accountId)}/access`, {
                method: 'GET',
            }),
        );
    },

    /** Gives a role to an account, for a reason from the access category. */
    async giveRole(
        accountId: string,
        input: { readonly roleId: string; readonly reasonCode: string; readonly note: string },
    ): Promise<void> {
        await request(`/api/accounts/${encodeURIComponent(accountId)}/roles`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Takes a role away from an account. */
    async takeRoleAway(accountId: string, roleId: string): Promise<void> {
        await request(
            `/api/accounts/${encodeURIComponent(accountId)}/roles/${encodeURIComponent(roleId)}`,
            { method: 'DELETE' },
        );
    },

    /** The tenant's roles, each with the permissions it grants. */
    async roles(): Promise<readonly RoleSummary[]> {
        return z
            .object({ roles: z.array(roleSummarySchema) })
            .parse(await request('/api/roles', { method: 'GET' })).roles;
    },

    /** Every permission the platform defines. */
    async permissions(): Promise<readonly PermissionEntry[]> {
        return z
            .object({ permissions: z.array(permissionEntrySchema) })
            .parse(await request('/api/permissions', { method: 'GET' })).permissions;
    },

    /** Creates a role that grants nothing yet, and answers its identifier. */
    async createRole(input: {
        readonly name: string;
        readonly description: string;
    }): Promise<string> {
        return z.object({ id: z.string() }).parse(
            await request('/api/roles', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        ).id;
    },

    /** Renames or redescribes a role, against the version the screen read. */
    async updateRole(
        roleId: string,
        input: { readonly name: string; readonly description: string; readonly version: number },
    ): Promise<void> {
        await request(`/api/roles/${encodeURIComponent(roleId)}`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Replaces what a role grants, and answers the codes as stored. */
    async saveRolePermissions(
        roleId: string,
        codes: readonly string[],
        note: string,
    ): Promise<readonly string[]> {
        return z.object({ codes: z.array(z.string()) }).parse(
            await request(`/api/roles/${encodeURIComponent(roleId)}/permissions`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ codes, note }),
            }),
        ).codes;
    },

    /** Deletes a role by its name. */
    async deleteRole(name: string): Promise<void> {
        await request(`/api/roles/${encodeURIComponent(name)}`, { method: 'DELETE' });
    },

    /**
     * One account by username, or nothing when the tenant has none with it.
     *
     * The caller names a row it has just read out of the list, so the row
     * having gone is an expected answer and not a failure.
     */
    async account(username: string): Promise<Account | null> {
        const answer = z
            .object({ account: accountSchema.nullable() })
            .parse(
                await request(`/api/accounts/${encodeURIComponent(username)}`, { method: 'GET' }),
            );
        return answer.account;
    },

    /**
     * One account's login record, or nothing when it has never signed in.
     *
     * A record is written by signing in, so an account that has never done so
     * has none, and the screen says that rather than showing zeros.
     */
    async loginInfo(accountId: string): Promise<LoginInfo | null> {
        const answer = z.object({ loginInfo: loginInfoSchema.nullable() }).parse(
            await request(`/api/login-info/${encodeURIComponent(accountId)}`, {
                method: 'GET',
            }),
        );
        return answer.loginInfo;
    },

    /** The tenant's login records, one page at a time. */
    async loginInfoPage(): Promise<{
        readonly loginInfo: readonly LoginInfo[];
        readonly totalCount: number;
    }> {
        return z
            .object({ loginInfo: z.array(loginInfoSchema), totalCount: z.int().nonnegative() })
            .parse(await request('/api/login-info', { method: 'GET' }));
    },

    /** The tenant's sessions, one page at a time. */
    async sessions(): Promise<{
        readonly sessions: readonly Session[];
        readonly totalCount: number;
    }> {
        return z
            .object({ sessions: z.array(sessionSchema), totalCount: z.int().nonnegative() })
            .parse(await request('/api/sessions', { method: 'GET' }));
    },

    /**
     * The sessions with no end time.
     *
     * The server answers an empty list while its own read is incomplete, and
     * that is an answer rather than a failure: the screen states that the list
     * may be short rather than pretending the tenant has no sessions.
     */
    async activeSessions(): Promise<readonly Session[]> {
        const answer = z
            .object({ sessions: z.array(sessionSchema) })
            .parse(await request('/api/sessions/active', { method: 'GET' }));
        return answer.sessions;
    },

    /**
     * Locks or unlocks one account.
     *
     * The administrator acts on the account the screen is showing, so the id
     * travels and the server's answer is a refusal the screen renders rather
     * than a list of per-account results it would have to collapse itself.
     */
    async setAccountLocked(accountId: string, locked: boolean): Promise<void> {
        await request(
            `/api/accounts/${encodeURIComponent(accountId)}/${locked ? 'lock' : 'unlock'}`,
            { method: 'POST', headers: JSON_HEADERS },
        );
    },

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
     * The tenant lifecycle statuses, with the badge each one is painted with.
     *
     * The words are the status's own and the colours are the badge's, so a
     * screen showing a tenant's status reads one list rather than joining two.
     * A status whose badge has left the catalogue carries no colours.
     */
    async tenantStatuses(): Promise<readonly TenantStatus[]> {
        const payload = tenantStatusesResponseSchema.parse(
            await request('/api/tenant-statuses', { method: 'GET' }),
        );
        return payload.statuses;
    },

    /** The tenant types, each with the badge its row names. */
    async tenantTypes(): Promise<readonly TenantType[]> {
        const payload = tenantTypesResponseSchema.parse(
            await request('/api/tenant-types', { method: 'GET' }),
        );
        return payload.types;
    },

    /**
     * The tenants this deployment holds.
     *
     * The system tenant is not among them: it is the deployment's own
     * bookkeeping rather than a tenant somebody set up, and the answer has
     * already dropped it.
     */
    async tenants(
        query: {
            readonly search?: string;
            readonly type?: string;
            readonly status?: string;
            readonly includeTest?: boolean;
            readonly offset?: number;
            readonly limit?: number;
        } = {},
    ): Promise<TenantPage> {
        const params = new URLSearchParams();
        for (const key of ['search', 'type', 'status'] as const) {
            const value = query[key];
            if (value !== undefined && value !== '') {
                params.set(key, value);
            }
        }
        if (query.includeTest === true) {
            params.set('includeTest', 'true');
        }
        if (query.offset !== undefined) {
            params.set('offset', String(query.offset));
        }
        if (query.limit !== undefined) {
            params.set('limit', String(query.limit));
        }
        const suffix = params.size === 0 ? '' : `?${params.toString()}`;
        return tenantPageSchema.parse(await request(`/api/tenants${suffix}`, { method: 'GET' }));
    },

    /**
     * The state of the deployment's tenants, for the system administrator's
     * home: the counts, what needs attention, the first tenants and the newest
     * setups.
     */
    async overview(): Promise<DeploymentOverview> {
        return deploymentOverviewSchema.parse(await request('/api/overview', { method: 'GET' }));
    },

    /** One page of the parties of the session's own tenant. */
    async parties(query: { readonly offset: number; readonly limit: number }): Promise<PartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return partyPageSchema.parse(
            await request(`/api/parties?${params.toString()}`, { method: 'GET' }),
        );
    },

    /**
     * Removes a tenant. The code is typed again by the person, and the server
     * checks it against the tenant the address names.
     */
    async removeTenant(code: string, confirmCode: string): Promise<void> {
        await request(`/api/tenants/${encodeURIComponent(code)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify({ confirmCode }),
        });
    },

    /** One page of a tenant's parties, read inside it from system administration. */
    async tenantParties(
        code: string,
        query: { readonly offset: number; readonly limit: number },
    ): Promise<PartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return partyPageSchema.parse(
            await request(`/api/tenants/${encodeURIComponent(code)}/parties?${params.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** One page of a tenant's people, read inside it from system administration. */
    async tenantPeople(
        code: string,
        query: { readonly offset: number; readonly limit: number },
    ): Promise<{ readonly accounts: readonly Account[]; readonly totalCount: number }> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return z
            .object({ accounts: z.array(accountSchema), totalCount: z.int().nonnegative() })
            .parse(
                await request(
                    `/api/tenants/${encodeURIComponent(code)}/people?${params.toString()}`,
                    { method: 'GET' },
                ),
            );
    },

    /** One tenant by its code: the tenant and its setup run. */
    async tenant(code: string): Promise<TenantDetailResponse> {
        return tenantDetailResponseSchema.parse(
            await request(`/api/tenants/${encodeURIComponent(code)}`, { method: 'GET' }),
        );
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
