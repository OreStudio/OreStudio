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

import type { ListPartiesRequest } from './generated/refdata/protocol/party_protocol.js';
import type { ServiceRosterSlot } from './generated/telemetry/protocol/service_samples_protocol.js';
import type { Host } from './generated/compute/domain/host.js';
import { z } from 'zod';
import type { WireFormat } from './codec.js';
import { WireCodec } from './codec.js';
import type { DatabaseInfo } from './contracts.js';
import type { PartySummary } from './domain.js';
import {
    subjects as tenantSessionSubjects,
    type EnterTenantRequest,
    type EnterTenantResponse,
    type LeaveTenantRequest,
    type LeaveTenantResponse,
} from './generated/iam/protocol/tenant_session_protocol.js';
import type { AuthenticatedCaller } from './account-operations.js';
import {
    NotAuthenticatedError,
    OperationFailedError,
    ServerError,
    SessionExpiredError,
    type ProtocolError,
    type ServerErrorCode,
} from './errors.js';
import {
    SUBJECTS,
    bootstrapStatusResponseSchema,
    createInitialAdminRequestSchema,
    createInitialAdminResponseSchema,
    emptyRequestSchema,
    listSeedProfileChildrenRequestSchema,
    listSeedProfilesRequestSchema,
    seedProfilePageSchema,
    seedProfileParameterPageSchema,
    seedProfileStepPageSchema,
    toSeedProfileChoice,
    loginRequestSchema,
    loginResponseSchema,
    logoutResponseSchema,
    partyRequestSchema,
    partyResponseSchema,
    listPartiesReplySchema,
    provisionPartyReplySchema,
    putPartyReplySchema,
    toProvisionPartyCommand,
    toProvisionPartyResult,
    toPutPartyChange,
    toPutPartyResult,
    provisionTenantReplySchema,
    provisionTenantRequestSchema,
    refreshResponseSchema,
    accountPageSchema,
    httpInfoResponseSchema,
    listAccountsRequestSchema,
    retryWorkflowInstanceReplySchema,
    retryWorkflowInstanceRequestSchema,
    toProvisionTenantCommand,
    toProvisionTenantResult,
    toRetryWorkflowInstanceCommand,
    toRetryWorkflowInstanceResult,
    passwordPolicyReplySchema,
    toPasswordPolicy,
    serviceRosterReplySchema,
    serviceRosterRequestSchema,
    gridStatsReplySchema,
    gridStatsRequestSchema,
    listHostsReplySchema,
    listHostsRequestSchema,
    registrationPolicyRequestSchema,
    registrationPolicyReplySchema,
    toRegistrationPolicy,
    signupCommandSchema,
    signupReplySchema,
    toSignupOutcome,
    type RegistrationPolicy,
    type SignupOutcome,
    workflowProgressSchema,
    toGetWorkflowStepsRequest,
    type LoginResponse,
    type PasswordPolicy,
    type PartyRow,
    type ProvisionPartyResult,
    type ProvisionTenantRequest,
    type ProvisionTenantResult,
    type PutPartyResult,
    type RetryWorkflowInstanceRequest,
    type RetryWorkflowInstanceResult,
    type SeedProfileChoice,
    type WireAccountPage,
    type WorkflowProgress,
    type GridStatsReply,
} from './operations.js';
import { subjects as bootstrapSubjects } from './generated/iam/protocol/bootstrap_protocol.js';
import type { Transport } from './transport.js';
import { resolveHeaders } from './headers.js';
import type { HeaderSource } from './headers.js';
import { nodeIdGenerator, portableIdGenerator, tracingHeaders, type IdGenerator } from './ids.js';
import { uuid, type Uuid } from './primitives.js';

/** How long the client waits for each kind of call. */
export interface Timeouts {
    /** Quick reads: lookups, list pages. */
    readonly fastMs: number;
    /** Server-side work such as provisioning or imports. */
    readonly slowMs: number;
}

export const DEFAULT_TIMEOUTS: Timeouts = {
    fastMs: 30_000,
    slowMs: 120_000,
};

/** Everything needed to open a login session. */
export interface LoginCredentials {
    readonly principal: string;
    readonly password: string;
}

/** A session that a party has been chosen for, and that can be used for calls. */
export interface ActiveSession {
    readonly kind: 'active';
    readonly token: string;
    readonly accountId: string;
    readonly tenantId: string;
    readonly tenantName: string;
    /** The build the session was opened against. */
    readonly version: string;
    /** The database the login answer carried, beside the build it states. */
    readonly database: DatabaseInfo;
    readonly username: string;
    readonly email: string;
    readonly party: PartySummary;
    readonly availableParties: readonly PartySummary[];
    readonly accessLifetimeSeconds: number;
    readonly passwordResetRequired: boolean;
    readonly sessionId: string;
}

/** A login that the server accepted but that still needs a party. */
export interface PartySelectionRequired {
    readonly kind: 'party-selection-required';
    readonly accountId: string;
    readonly tenantId: string;
    readonly tenantName: string;
    /** The build the login was answered by. */
    readonly version: string;
    /** The database the login answer carried, beside the build it states. */
    readonly database: DatabaseInfo;
    readonly username: string;
    readonly email: string;
    readonly availableParties: readonly PartySummary[];
    readonly defaultPartyId: string | null;
    readonly passwordResetRequired: boolean;
    readonly accessLifetimeSeconds: number;
    readonly sessionId: string;
}

/** A login the server rejected. */
export interface LoginRejected {
    readonly kind: 'rejected';
    readonly message: string;
    /** The build that refused, which a caller can still state. */
    readonly version: string;
    /** The database the refusal carried, beside the build it states. */
    readonly database: DatabaseInfo;
}

export type LoginOutcome = ActiveSession | PartySelectionRequired | LoginRejected;

export interface OresClientOptions {
    readonly transport: Transport;
    readonly format?: WireFormat;
    readonly timeouts?: Partial<Timeouts>;
    /** Injected for tests; defaults to the real clock. */
    readonly now?: () => Date;
    /** Injected for tests and for hosts without Node's crypto. */
    readonly generateId?: IdGenerator;
}

const enterTenantResponseSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    token: z.string().default(''),
    tenant_id: z.string().default(''),
    tenant_code: z.string().default(''),
    tenant_name: z.string().default(''),
    party_id: z.string().default(''),
    party_name: z.string().default(''),
    access_lifetime_s: z.int().default(0),
}) satisfies z.ZodType<EnterTenantResponse>;

const leaveTenantResponseSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
}) satisfies z.ZodType<LeaveTenantResponse>;

interface SessionState {
    token: string;
    /** IAM session id, forwarded as `Nats-Session-Id` on every authenticated call. */
    sessionId: string;
    refreshInFlight: Promise<string> | undefined;
}

/**
 * The typed client for the ORE Studio bus.
 *
 * It owns the login lifecycle and the session token, and it maps every reply
 * through a schema before handing it to a caller. It never exposes the token
 * to a caller that does not need it.
 */
export class OresClient {
    readonly #transport: Transport;
    readonly #codec: WireCodec;
    readonly #timeouts: Timeouts;
    readonly #now: () => Date;
    readonly #generateId: IdGenerator;
    #session: SessionState | undefined;

    constructor(options: OresClientOptions) {
        this.#transport = options.transport;
        this.#codec = new WireCodec(options.format ?? 'msgpack');
        this.#timeouts = { ...DEFAULT_TIMEOUTS, ...options.timeouts };
        this.#now = options.now ?? (() => new Date());
        this.#generateId = options.generateId ?? nodeIdGenerator;
    }

    get transport(): Transport {
        return this.#transport;
    }

    /** True once a token exists, whether or not a party has been chosen. */
    get hasToken(): boolean {
        return this.#session !== undefined;
    }

    /**
     * The IAM session id for the current login, or an empty string.
     *
     * The server wants this on every authenticated call as `Nats-Session-Id`,
     * and a caller that manages a session across processes needs it to carry a
     * pending party selection forward. It is available as soon as `login`
     * returns, before any party has been chosen.
     */
    get currentSessionId(): string {
        return this.#session?.sessionId ?? '';
    }

    /**
     * Whether the deployment still needs provisioning.
     *
     * Asked before a login, not after it. A deployment in bootstrap mode has no
     * accounts to sign in with, so the answer decides whether a login is worth
     * attempting at all: the Qt client checked this first and never reached the
     * credential form. The tenant answer travels with it because both describe
     * the same thing from two sides: an installation is set up when it has an
     * administrator and a tenant of its own.
     */
    async bootstrapStatus(): Promise<{
        isInBootstrapMode: boolean;
        hasTenant: boolean;
        message: string;
        version: string;
    }> {
        const reply = await this.#call(
            bootstrapSubjects.bootstrap_status_request,
            emptyRequestSchema.parse({}),
            bootstrapStatusResponseSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        return {
            isInBootstrapMode: reply.is_in_bootstrap_mode,
            hasTenant: reply.has_tenant,
            message: reply.message,
            version: reply.version,
        };
    }

    /**
     * Creates the first administrator, which closes bootstrap mode.
     *
     * The one write that needs no session, because bootstrap mode has no
     * accounts to present: the account this creates is the first one. The
     * function behind it clears the bootstrap flag, so a deployment stops
     * answering the setup screen the moment this succeeds.
     *
     * It is a slow call: the server hashes the password and provisions the
     * account, its role and its party.
     */
    async createInitialAdmin(input: {
        readonly principal: string;
        readonly password: string;
        readonly email: string;
    }): Promise<{
        readonly success: boolean;
        readonly errorMessage: string;
        readonly accountId: string;
        readonly tenantId: string;
    }> {
        const reply = await this.#call(
            bootstrapSubjects.create_initial_admin_request,
            createInitialAdminRequestSchema.parse({
                principal: input.principal,
                password: input.password,
                email: input.email,
            }),
            createInitialAdminResponseSchema,
            { timeoutMs: this.#timeouts.slowMs },
        );
        return {
            success: reply.success,
            errorMessage: reply.error_message,
            accountId: reply.account_id,
            tenantId: reply.tenant_id,
        };
    }

    /**
     * The starting points tenant provisioning offers.
     *
     * Three reads joined into one answer: the profile page carries no
     * children, and the screen that chooses a starting point shows each
     * profile, the step kinds it orders and the form it declares together.
     * The join belongs here rather than in the browser, which cannot name a
     * subject at all.
     *
     * The profile page is ordered by the key, so the answer is sorted by the
     * order each profile declares: that column exists to order the cards, and
     * two deployments' profiles need not be created in that order. Each
     * profile's children are already in the order the server read them.
     */
    async seedProfiles(
        input: { readonly limit?: number } = {},
    ): Promise<readonly SeedProfileChoice[]> {
        const page = await this.#authenticatedCall(
            SUBJECTS.listSeedProfiles,
            listSeedProfilesRequestSchema.parse({ limit: input.limit ?? 50 }),
            seedProfilePageSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        if (page.result.outcome !== 'ok') {
            throw new OperationFailedError(SUBJECTS.listSeedProfiles, page.result.message);
        }

        const choices: SeedProfileChoice[] = [];
        for (const profile of page.seed_profiles) {
            const request = listSeedProfileChildrenRequestSchema.parse({
                seed_profile_id: profile.id,
            });
            const steps = await this.#authenticatedCall(
                SUBJECTS.listSeedProfileSteps,
                request,
                seedProfileStepPageSchema,
                { timeoutMs: this.#timeouts.fastMs },
            );
            if (steps.result.outcome !== 'ok') {
                throw new OperationFailedError(SUBJECTS.listSeedProfileSteps, steps.result.message);
            }
            const parameters = await this.#authenticatedCall(
                SUBJECTS.listSeedProfileParameters,
                request,
                seedProfileParameterPageSchema,
                { timeoutMs: this.#timeouts.fastMs },
            );
            if (parameters.result.outcome !== 'ok') {
                throw new OperationFailedError(
                    SUBJECTS.listSeedProfileParameters,
                    parameters.result.message,
                );
            }
            choices.push(
                toSeedProfileChoice(
                    profile,
                    steps.seed_profile_steps,
                    parameters.seed_profile_parameters,
                ),
            );
        }

        return choices.sort((left, right) =>
            left.order === right.order
                ? left.code.localeCompare(right.code)
                : left.order - right.order,
        );
    }

    /**
     * Provisions a tenant from a starting point.
     *
     * The tenant and its administrator exist by the time this answers, and the
     * answer carries the id of the workflow instance that runs the steps the
     * profile orders. Those steps take minutes, so the run is followed by its
     * id rather than by waiting here: the call itself is as slow as creating
     * the tenant and its account, which is why it is not a fast call.
     *
     * A refusal the server states, such as an unknown profile or a parameter
     * its schema does not accept, comes back as a result with `success` false
     * rather than as a thrown error: it is an answer to the request, not a
     * failure of the call.
     */
    async provisionTenant(input: ProvisionTenantRequest): Promise<ProvisionTenantResult> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.provisionTenant,
            toProvisionTenantCommand(provisionTenantRequestSchema.parse(input)),
            provisionTenantReplySchema,
            { timeoutMs: this.#timeouts.slowMs },
        );
        return toProvisionTenantResult(reply);
    }

    /**
     * Every party of the caller's own tenant.
     *
     * The page is read to the end rather than once, because what this answers
     * is a question about all of them: which party sits at the top of the
     * tenant's hierarchy, and whether the tenant holds the one a session is
     * being scoped to. A tenant's parties are its own structure and not its
     * trading data, so reading them all is the small read it looks like.
     */
    async listParties(): Promise<readonly PartyRow[]> {
        const PAGE_SIZE = 500;
        const parties: PartyRow[] = [];
        for (let offset = 0; ; offset += PAGE_SIZE) {
            const reply = await this.#authenticatedCall(
                SUBJECTS.listParties,
                {
                    offset,
                    limit: PAGE_SIZE,
                    order: { field: '', descending: false },
                    as_of: null,
                    filter: null,
                } satisfies ListPartiesRequest,
                listPartiesReplySchema,
                { timeoutMs: this.#timeouts.fastMs },
            );
            if (reply.result.outcome !== 'ok') {
                throw new OperationFailedError(SUBJECTS.listParties, reply.result.message);
            }
            parties.push(...reply.parties);
            if (reply.parties.length === 0 || parties.length >= reply.total) {
                return parties;
            }
        }
    }

    /**
     * Adds one party to the caller's own tenant.
     *
     * The identifier is minted here, because the row is written with it and
     * the caller needs it before the answer: the party stage that follows names
     * the party by that identifier.
     *
     * A refusal the server states, such as a short code the tenant already
     * uses, comes back as a result with `success` false rather than as a thrown
     * error: it is an answer to the write, and the person who asked can act on
     * it.
     */
    async createParty(input: {
        readonly shortCode: string;
        readonly fullName: string;
        readonly parentPartyId: string | null;
    }): Promise<PutPartyResult> {
        const id = portableIdGenerator();
        const reply = await this.#authenticatedCall(
            SUBJECTS.putParty,
            {
                change: toPutPartyChange({
                    id,
                    shortCode: input.shortCode,
                    fullName: input.fullName,
                    parentPartyId: input.parentPartyId,
                }),
                intent: { reason_code: '', commentary: '' },
            },
            putPartyReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        return toPutPartyResult(reply, id);
    }

    /**
     * Runs the party stage of a starting point against one party.
     *
     * The party exists by the time this is called, and the answer carries the
     * id of the run that publishes its data, activates it and joins the caller
     * to it. The run is followed by that id rather than by waiting here.
     */
    async provisionParty(input: {
        readonly party: string;
        readonly profileCode: string;
        readonly lei: string;
    }): Promise<ProvisionPartyResult> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.provisionParty,
            toProvisionPartyCommand(input),
            provisionPartyReplySchema,
            { timeoutMs: this.#timeouts.slowMs },
        );
        return toProvisionPartyResult(reply);
    }

    /**
     * The rules a password must satisfy.
     *
     * Asked for before anybody has signed in, because the sign-in screen is
     * where the rules are shown. The answer is the server's own policy, so the
     * screen never keeps a copy of rules the server could change.
     */
    async passwordPolicy(): Promise<PasswordPolicy> {
        const reply = await this.#call(
            SUBJECTS.passwordPolicy,
            emptyRequestSchema.parse({}),
            passwordPolicyReplySchema,
            {
                timeoutMs: this.#timeouts.fastMs,
            },
        );
        return toPasswordPolicy(reply);
    }

    /**
     * What the deployment offers somebody who is not in it yet.
     *
     * `hostname` is the address the browser arrived at, and it is how the
     * service resolves the tenant: the person types a username and not a
     * `user@hostname`, and a registration that resolves to no tenant is
     * refused rather than landing in the system tenant by omission.
     */
    async registrationPolicy(hostname: string): Promise<RegistrationPolicy> {
        const reply = await this.#call(
            SUBJECTS.registrationPolicy,
            registrationPolicyRequestSchema.parse({ hostname }),
            registrationPolicyReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        return toRegistrationPolicy(reply);
    }

    /**
     * Registers an account, and answers with the state it was created in.
     *
     * The answer is not a session: a pending account cannot sign in, and an
     * active one arrives at the door deliberately. A refusal is an answer
     * rather than an error here, because the code it carries is what the
     * screen branches on.
     */
    async signup(input: {
        readonly principal: string;
        readonly password: string;
        readonly email: string;
        readonly hostname: string;
    }): Promise<SignupOutcome> {
        const reply = await this.#call(
            SUBJECTS.signup,
            signupCommandSchema.parse(input),
            signupReplySchema,
            { timeoutMs: this.#timeouts.slowMs },
        );
        return toSignupOutcome(reply);
    }

    /**
     * Follows a run through the progress read.
     *
     * The page's rail comes from here: the run's status, how many steps it
     * declared, the step it is executing, and one summary per step. The read
     * is the progress contract, so a page that asks again sees the run move
     * without subscribing to anything.
     */
    async workflowProgress(instanceId: string): Promise<WorkflowProgress> {
        return this.#authenticatedCall(
            SUBJECTS.workflowInstanceSteps,
            toGetWorkflowStepsRequest(instanceId),
            workflowProgressSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
    }

    /**
     * Asks the engine to resume a stopped run.
     *
     * The answer names the step that was re-dispatched, or why the engine
     * refused. A refusal is an answer rather than a thrown error: a run that
     * has not stopped, or a step the run does not hold, is something the
     * person asking can see and act on.
     */
    async retryWorkflowInstance(
        input: RetryWorkflowInstanceRequest,
    ): Promise<RetryWorkflowInstanceResult> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.retryWorkflowInstance,
            toRetryWorkflowInstanceCommand(retryWorkflowInstanceRequestSchema.parse(input)),
            retryWorkflowInstanceReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        return toRetryWorkflowInstanceResult(reply);
    }

    /**
     * Sends `login` and classifies the reply.
     *
     * The server uses a nil `selected_party_id` to mean "party picker
     * outstanding", so the caller must resolve that before calling anything
     * authenticated.
     */
    async login(credentials: LoginCredentials): Promise<LoginOutcome> {
        const body = loginRequestSchema.parse(credentials);
        const reply = await this.#call(SUBJECTS.login, body, loginResponseSchema, {
            timeoutMs: this.#timeouts.fastMs,
        });

        if (!reply.success || reply.token.length === 0) {
            return {
                kind: 'rejected',
                message: reply.errorMessage.length > 0 ? reply.errorMessage : reply.message,
                version: reply.version,
                database: reply.database,
            };
        }

        this.#session = {
            token: reply.token,
            sessionId: reply.sessionId,
            refreshInFlight: undefined,
        };

        if (reply.selectedPartyId.length > 0) {
            const party = reply.availableParties.find(
                (candidate) => candidate.id === reply.selectedPartyId,
            );
            if (party !== undefined) {
                return toActive(reply, party);
            }
        }

        return {
            kind: 'party-selection-required',
            accountId: reply.accountId,
            tenantId: reply.tenantId,
            tenantName: reply.tenantName,
            version: reply.version,
            database: reply.database,
            username: reply.username,
            email: reply.email,
            availableParties: reply.availableParties,
            defaultPartyId: reply.defaultPartyId.length > 0 ? reply.defaultPartyId : null,
            passwordResetRequired: reply.passwordResetRequired,
            accessLifetimeSeconds: reply.accessLifetimeSeconds,
            sessionId: reply.sessionId,
        };
    }

    /**
     * Chooses the party for the session the login opened.
     *
     * Only the single-use token from `login` is accepted here; a token that
     * already has a party must use {@link switchParty}.
     */
    async selectParty(input: {
        readonly partyId: string;
        readonly expected: PartySelectionRequired;
    }): Promise<ActiveSession> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.selectParty,
            partyRequestSchema.parse({ party_id: input.partyId }),
            partyResponseSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );

        if (!reply.success || reply.token.length === 0) {
            throw new SessionExpiredError('token_expired', SUBJECTS.selectParty);
        }
        this.#replaceToken(reply.token);

        const party = input.expected.availableParties.find(
            (candidate) => candidate.id === input.partyId,
        );
        if (party === undefined) {
            throw new NotAuthenticatedError(
                `Server accepted party ${input.partyId} but did not list it`,
            );
        }

        return {
            kind: 'active',
            token: reply.token,
            accountId: input.expected.accountId,
            tenantId: input.expected.tenantId,
            tenantName: reply.tenantName.length > 0 ? reply.tenantName : input.expected.tenantName,
            version: input.expected.version,
            database: input.expected.database,
            username: reply.username.length > 0 ? reply.username : input.expected.username,
            email: input.expected.email,
            party,
            availableParties: input.expected.availableParties,
            accessLifetimeSeconds: reply.accessLifetimeSeconds,
            passwordResetRequired: input.expected.passwordResetRequired,
            sessionId: input.expected.sessionId,
        };
    }

    /** Re-scopes an already-active session to another party. */
    async switchParty(input: {
        readonly partyId: string;
        readonly availableParties: readonly PartySummary[];
    }): Promise<{
        readonly token: string;
        readonly party: PartySummary;
        readonly accessLifetimeSeconds: number;
    }> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.switchParty,
            partyRequestSchema.parse({ party_id: input.partyId }),
            partyResponseSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        if (!reply.success || reply.token.length === 0) {
            throw new SessionExpiredError('token_expired', SUBJECTS.switchParty);
        }
        this.#replaceToken(reply.token);

        const party = input.availableParties.find((candidate) => candidate.id === input.partyId);
        if (party === undefined) {
            throw new NotAuthenticatedError(
                `Server accepted party ${input.partyId} but did not list it`,
            );
        }
        return {
            token: reply.token,
            party,
            accessLifetimeSeconds: reply.accessLifetimeSeconds,
        };
    }

    /** Ends the session, best effort. The local state is cleared either way. */
    async logout(): Promise<void> {
        if (this.#session !== undefined) {
            await this.#authenticatedCall(SUBJECTS.logout, {}, logoutResponseSchema, {
                timeoutMs: 3_000,
            }).catch(() => undefined);
        }
        this.#session = undefined;
    }

    /**
     * Exchanges the current token for a fresh one.
     *
     * Concurrent callers share one refresh, so a burst of expired requests does
     * not storm the server.
     *
     * @throws {NotAuthenticatedError} when there is no session to refresh.
     * @throws {SessionExpiredError} when the server refuses the refresh.
     */
    async refresh(): Promise<string> {
        const session = this.#requireSession();
        session.refreshInFlight ??= this.#performRefresh(session).finally(() => {
            session.refreshInFlight = undefined;
        });
        return session.refreshInFlight;
    }

    /**
     * Reads inside one tenant from system administration, for one read.
     *
     * The tenant session lasts as long as `read`: it is entered before the read
     * and left after it, whether or not the read succeeds. Its token is never
     * put on the session, so a call the session makes meanwhile keeps the
     * session's own token, and every call the read makes carries the tenant's.
     * A tenant session is not refreshed, so a read that outlives it fails as
     * an ordinary failed read, and the session it was entered from goes on.
     * `onExitFailure` hears of an exit that could not be recorded.
     *
     * @throws {OperationFailedError} when the server refuses the entry, or the
     * tenant session ended during the read.
     */
    async readInsideTenant<T>(
        tenantId: string,
        read: (caller: AuthenticatedCaller) => Promise<T>,
        onExitFailure: (error: unknown) => void = () => undefined,
    ): Promise<T> {
        const subject = tenantSessionSubjects.enter_tenant_request;
        const request: EnterTenantRequest = { tenant_id: tenantId };
        const entered = await this.#authenticatedCall(subject, request, enterTenantResponseSchema, {
            timeoutMs: this.#timeouts.fastMs,
        });
        if (!entered.success || entered.token.length === 0) {
            throw new OperationFailedError(subject, entered.message);
        }
        const caller: AuthenticatedCaller = {
            callAuthenticated: (callSubject, body, schema) =>
                this.#callAs(entered.token, callSubject, body, schema),
        };
        try {
            return await read(caller);
        } finally {
            /*
             * The exit is recorded when it can be. One that cannot be leaves a
             * tenant session nobody holds, which ends at its own expiry.
             */
            const leave: LeaveTenantRequest = {};
            await this.#callAs(
                entered.token,
                tenantSessionSubjects.leave_tenant_request,
                leave,
                leaveTenantResponseSchema,
            ).catch(onExitFailure);
        }
    }

    /** One call made with a token other than the session's own. */
    async #callAs<Schema extends z.ZodType>(
        token: string,
        subject: string,
        body: unknown,
        schema: Schema,
    ): Promise<z.infer<Schema>> {
        const session = this.#requireSession();
        const reply = await this.#transport.request(
            subject,
            this.#codec.encode(body),
            { ...this.#authenticatedHeaders(session), Authorization: `Bearer ${token}` },
            this.#timeouts.fastMs,
        );
        const serverError = serverErrorCode(reply.headers);
        if (serverError === 'token_expired') {
            /*
             * The tenant token lapsed, not the session's own: reporting it as
             * an expired session would sign the person out of a session that
             * is still good.
             */
            throw new OperationFailedError(
                subject,
                'The time inside the tenant ended. Read again.',
            );
        }
        if (serverError !== undefined) {
            throw errorForServerCode(serverError, subject);
        }
        return this.#codec.decodeAs(reply.body, schema);
    }

    /**
     * Listens for changes to an entity.
     *
     * The payload is a change notification: when it happened, which records, and
     * whose they are. The time and the records are handed on. The time says whether
     * what is on screen is older than what exists; the records say how many, so a
     * screen can say so, and which, so it can badge them without comparing
     * timestamps.
     *
     * Nothing is authenticated here. These are published events, not replies, so
     * there is no session to present and no failure to report: a subscription that
     * cannot be made is a screen that does not hear about changes, which is the
     * behaviour it had before.
     */
    subscribeToEvents(
        relative: string,
        onEvent: (change: { readonly at: string; readonly ids: readonly string[] }) => void,
    ): () => void {
        const subscribe = this.#transport.subscribe;
        if (subscribe === undefined) {
            return () => undefined;
        }
        return subscribe.call(this.#transport, relative, (payload) => {
            try {
                const decoded = this.#codec.decodeAs(payload, changeEventSchema);
                onEvent({ at: decoded.timestamp, ids: decoded.alpha2_codes });
            } catch {
                // An event this build does not understand is one it cannot act on.
            }
        });
    }

    /** Lists one page of accounts. */
    async listAccounts(
        input: {
            readonly offset?: number;
            readonly limit?: number;
        } = {},
    ): Promise<WireAccountPage> {
        return this.#authenticatedCall(
            SUBJECTS.listAccounts,
            listAccountsRequestSchema.parse(input),
            accountPageSchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
    }

    /**
     * The services roster: every expected instance and its last report.
     *
     * The reply is one slot per expected instance, ordered by service name and
     * then slot, so a caller renders it as it arrives. A refusal the read
     * states in its body is an error rather than an empty list: an empty list
     * would read as an installation with no services.
     */
    async serviceRoster(): Promise<readonly ServiceRosterSlot[]> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.serviceRoster,
            serviceRosterRequestSchema.parse({}),
            serviceRosterReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        if (!reply.success) {
            throw new OperationFailedError(SUBJECTS.serviceRoster, reply.message);
        }
        return reply.slots;
    }

    /**
     * The newest stored grid sample, and the most recent sample of every node.
     *
     * The read takes no fields, because the caller's session decides what the
     * summary covers, and it carries the counters as they were stored rather
     * than computing a live count. A refusal the read states in its body is an
     * error rather than a zeroed summary: zeros would read as an idle grid.
     */
    async gridStats(): Promise<GridStatsReply> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.gridStats,
            gridStatsRequestSchema.parse({}),
            gridStatsReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        if (!reply.success) {
            throw new OperationFailedError(SUBJECTS.gridStats, reply.message);
        }
        return reply;
    }

    /**
     * The host registry, one page of every host.
     *
     * The grid joins this page onto its node rows to name them: a node sample
     * carries a host id and no hostname, so the name a person reads arrives
     * from here. A page result that did not end ok is an error rather than an
     * empty page, because an empty page would name no node at all.
     */
    async listHosts(): Promise<readonly Host[]> {
        const reply = await this.#authenticatedCall(
            SUBJECTS.listHosts,
            listHostsRequestSchema.parse({}),
            listHostsReplySchema,
            { timeoutMs: this.#timeouts.fastMs },
        );
        if (reply.result.outcome !== 'ok') {
            throw new OperationFailedError(SUBJECTS.listHosts, reply.result.message);
        }
        return reply.hosts;
    }

    /**
     * Issues an authenticated call against an explicit subject.
     *
     * Exposed so the account operations in `account-operations.ts` can be plain
     * functions over a narrow interface instead of methods that grow the client
     * one subject at a time.
     */
    callAuthenticated<Schema extends z.ZodType>(
        subject: string,
        body: unknown,
        schema: Schema,
    ): Promise<z.infer<Schema>> {
        return this.#authenticatedCall(subject, body, schema, {
            timeoutMs: this.#timeouts.fastMs,
        });
    }

    /**
     * Discovers the companion HTTP server address.
     *
     * The Qt client makes this call immediately after login. It is best effort:
     * a deployment without an HTTP server simply has no base URL.
     */
    async discoverHttpBaseUrl(): Promise<string | null> {
        const reply = await this.#authenticatedCall(SUBJECTS.httpInfo, {}, httpInfoResponseSchema, {
            timeoutMs: this.#timeouts.fastMs,
        }).catch(() => null);
        if (reply === null || !reply.success || reply.base_url.length === 0) {
            return null;
        }
        return reply.base_url;
    }

    async close(): Promise<void> {
        this.#session = undefined;
        await this.#transport.close();
    }

    /** Exposed so callers can build a proactive refresh timer. */
    get token(): string {
        return this.#requireSession().token;
    }

    async #performRefresh(session: SessionState): Promise<string> {
        const reply = await this.#call(SUBJECTS.refresh, {}, refreshResponseSchema, {
            timeoutMs: this.#timeouts.fastMs,
            headers: { Authorization: `Bearer ${session.token}` },
        });
        if (!reply.success || reply.token.length === 0) {
            // The server reports a session that outlived its maximum as a body
            // field, never as X-Error. Both spellings mean re-authentication.
            throw new SessionExpiredError('max_session_exceeded', SUBJECTS.refresh);
        }
        session.token = reply.token;
        return reply.token;
    }

    async #call<Schema extends z.ZodType>(
        subject: string,
        body: unknown,
        schema: Schema,
        options: {
            readonly timeoutMs: number;
            readonly headers?: HeaderSource;
        },
    ): Promise<z.infer<Schema>> {
        const headers = resolveHeaders(options.headers);
        const reply = await this.#transport.request(
            subject,
            this.#codec.encode(body),
            headers,
            options.timeoutMs,
        );
        return this.#codec.decodeAs(reply.body, schema);
    }

    async #authenticatedCall<Schema extends z.ZodType>(
        subject: string,
        body: unknown,
        schema: Schema,
        options: { readonly timeoutMs: number },
    ): Promise<z.infer<Schema>> {
        const session = this.#requireSession();
        const reply = await this.#transport.request(
            subject,
            this.#codec.encode(body),
            this.#authenticatedHeaders(session),
            options.timeoutMs,
        );

        const serverError = serverErrorCode(reply.headers);
        if (serverError === undefined) {
            return this.#codec.decodeAs(reply.body, schema);
        }
        if (serverError === 'token_expired') {
            await this.refresh();
            return this.#retryAuthenticated(subject, body, schema, options.timeoutMs);
        }
        throw errorForServerCode(serverError, subject);
    }

    async #retryAuthenticated<Schema extends z.ZodType>(
        subject: string,
        body: unknown,
        schema: Schema,
        timeoutMs: number,
    ): Promise<z.infer<Schema>> {
        const session = this.#requireSession();
        const reply = await this.#transport.request(
            subject,
            this.#codec.encode(body),
            this.#authenticatedHeaders(session),
            timeoutMs,
        );
        const serverError = serverErrorCode(reply.headers);
        if (serverError !== undefined) {
            throw errorForServerCode(serverError, subject);
        }
        return this.#codec.decodeAs(reply.body, schema);
    }

    #authenticatedHeaders(session: SessionState): Record<string, string> {
        // A fresh trace key per top-level operation, matching ClientManager.
        return {
            ...tracingHeaders(session.sessionId, this.#generateId),
            Authorization: `Bearer ${session.token}`,
        };
    }

    #requireSession(): SessionState {
        if (this.#session === undefined) {
            throw new NotAuthenticatedError('No session: call login() first');
        }
        return this.#session;
    }

    #replaceToken(token: string): void {
        this.#requireSession().token = token;
    }
}

/**
 * A change notification, as the services publish it.
 *
 * Every member has a default: an event is news rather than a contract, and a
 * notification that cannot be read is worth less than one that can be read
 * partially. A missing time means the screen cannot tell whether it is stale, so
 * it does nothing, which is the safe direction.
 */
const changeEventSchema = z.object({
    timestamp: z.string().default(''),
    alpha2_codes: z.array(z.string()).default([]),
    tenant_id: z.string().default(''),
});

/**
 * The failure a server error code means.
 *
 * Every code used to be reported as an expired session, including a refusal to
 * authorise. That is a costly lie: an expired session is something a person can
 * act on by signing in again, and a refusal is not, so the one thing the message
 * told them to do was the one thing that could not help. It also cost real time
 * here, where a permission problem read as a session problem for hours.
 *
 * The codes the server can send are enumerated, so a code with no case here is a
 * protocol change rather than a runtime surprise, and it is carried through as
 * itself rather than renamed.
 */
function errorForServerCode(code: ServerErrorCode, subject: string): ProtocolError {
    switch (code) {
        case 'token_expired':
        case 'max_session_exceeded':
            return new SessionExpiredError(code, subject);
        case 'unauthorized':
            return new NotAuthenticatedError(`The server requires authentication for ${subject}`);
        default:
            // `forbidden` and `bad_request` are the server understanding the request
            // and declining it, which is not a session problem.
            return new ServerError(code, subject);
    }
}

/** Reads the `X-Error` header, if the server set one. */
function serverErrorCode(headers: Readonly<Record<string, string>>): ServerErrorCode | undefined {
    const code = headers['X-Error'];
    return code === undefined ? undefined : (code as ServerErrorCode);
}

function toActive(reply: LoginResponse, party: PartySummary): ActiveSession {
    return {
        kind: 'active',
        token: reply.token,
        accountId: reply.accountId,
        tenantId: reply.tenantId,
        tenantName: reply.tenantName,
        version: reply.version,
        database: reply.database,
        username: reply.username,
        email: reply.email,
        party,
        availableParties: reply.availableParties,
        accessLifetimeSeconds: reply.accessLifetimeSeconds,
        passwordResetRequired: reply.passwordResetRequired,
        sessionId: reply.sessionId,
    };
}

/** Convenience for callers that only need the error type. */
export type { ProtocolError };
export { emptyRequestSchema };
