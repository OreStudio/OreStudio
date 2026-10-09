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

import { randomUUID } from 'node:crypto';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import Fastify, { type FastifyInstance, type FastifyReply, type FastifyRequest } from 'fastify';
import cookie from '@fastify/cookie';
import fastifyStatic from '@fastify/static';
import { z } from 'zod';
import { ChangeEventRegistry, type Watch } from './change-events.js';
import { registerClassificationRoutes } from './classifications.js';
import { registerAuditRoutes } from './audit.js';
import { registerTimelineRoutes } from './timeline.js';
import { registerInboxRoutes } from './inbox.js';
import { registerOperationsRoutes } from './operations.js';
import { registerRecordRoutes } from './records.js';
import {
    NatsTransport,
    OresClient,
    SUBJECTS,
    SYSTEM_TENANT_ID,
    bootstrapStatusSchema,
    isUuid,
    changeOwnPassword,
    changeOwnPasswordRequestSchema,
    changeReasonPageSchema,
    createAdministratorRequestSchema,
    initialAdministratorSchema,
    searchLeiEntitiesResponseSchema,
    listImageSummaries,
    readImageMap,
    readImages,
    removeTenant,
    accountAccessSchema,
    accountSchema,
    readReportingTree,
    reportingLineRequestSchema,
    reportingTreeSchema,
    setReportingLine,
    deleteRole,
    giveRole,
    permissionEntrySchema,
    readAccountAccess,
    readMyAccess,
    readPermissionCatalogue,
    readRoles,
    roleSummarySchema,
    saveRole,
    saveRolePermissions,
    takeRoleAway,
    toWireTimestamp,
    accountWriteViewSchema,
    claimedContactWriteSchema,
    contactViewSchema,
    contactWriteSchema,
    contactWriteViewSchema,
    imageUploadPolicyViewSchema,
    imageUploadViewSchema,
    defaultPartyRequestSchema,
    myPartiesSchema,
    profileWriteSchema,
    putContactInformation,
    readContactInformation,
    readMyContactInformation,
    readMyParties,
    setMyDefaultParty,
    readImageUploadPolicy,
    updateAccount,
    updateSelfAccount,
    updateSelfContactInformation,
    uploadImage,
    loginResultSchema,
    loginInfoKeyRequestSchema,
    listAccountsRequestSchema,
    listLoginInfoRequestSchema,
    listSessionsRequestSchema,
    passwordPolicySchema,
    provisionPartyRequestSchema,
    provisionPartyResultSchema,
    provisionTenantRequestSchema,
    provisionTenantResultSchema,
    registrationPolicyViewSchema,
    readAccount,
    readAccountsPage,
    readAccountSignIns,
    accountSignInsSchema,
    readActiveSessions,
    readLoginInfo,
    readLoginInfoPage,
    readSessionsPage,
    readTenantStatuses,
    readTenantTypes,
    readTenant,
    readPartiesPage,
    readProvisioningRuns,
    readTenantSetupRun,
    readTenantSetups,
    listTenantsPage,
    retryWorkflowInstanceResultSchema,
    selectPartyRequestSchema,
    seedProfilesResponseSchema,
    sessionViewSchema,
    setAccountLocked,
    signupRequestSchema,
    signupResultSchema,
    partyPageSchema,
    tenantDetailResponseSchema,
    tenantPageSchema,
    tenantSetupRunSchema,
    deploymentOverviewSchema,
    tenantStatusesResponseSchema,
    tenantTypesResponseSchema,
    workflowProgressSchema,
    NotAuthenticatedError,
    OperationFailedError,
    type LoginOutcome,
    type PartySummary,
    type AuthenticatedCaller,
    type ImageContent,
    type ProvisioningRun,
    type SetupActivity,
    type TenantSetup,
    type TenantSummary,
} from '@ores/wire-protocol';
import { credentialsSchema, deploymentViewSchema, siteStateSchema } from '@ores/contracts';
import type { ListChangeReasonsRequest } from '@ores/wire-protocol/generated/dq/protocol/change_reason_protocol';
import type { LoadedSiteConfiguration } from './site-config.js';
import { resolveBroker } from './broker.js';
import type { Config } from './config.js';
import { createImageCache } from './image-cache.js';
import { createRateLimiter, type RateLimiter } from './rate-limit.js';
import { createSessionStore, type LiveSession, type SessionStore } from './sessions.js';
import { sessionModeFor } from './session-mode.js';
import {
    bootstrapComplete,
    bootstrapRequired,
    invalidCredentials,
    invalidRequest,
    notAuthenticated,
    notFound,
    notPermitted,
    signupRefused,
    toHttpFailure,
    tooManyRequests,
    HttpFailure,
} from './errors.js';

/**
 * The browser-facing HTTP server.
 *
 * The browser speaks JSON and knows nothing about how the application reaches
 * ORE Studio. It is told which environment it is signed in to so it can say so
 * on screen, and that is all: not the host, not the port, not the namespace.
 * A browser that can name a host can ask the server to connect to it, and this
 * one cannot.
 *
 * The environment is chosen when the process starts and never changes, so every
 * route here talks to the same place for as long as the process runs.
 */

/** A page of the people list: the order is one the account model declares sortable. */
const accountPageQuerySchema = z.object({
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(1000)).default(100),
    search: z.string().trim().max(256).default(''),
    sort: z.enum(['', 'username', 'full_name']).default(''),
    descending: z
        .enum(['true', 'false'])
        .default('false')
        .transform((value) => value === 'true'),
});

const imagePageQuerySchema = z.object({
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(500)).default(100),
    search: z.string().trim().max(256).default(''),
});

/**
 * The session cookie name for one environment.
 *
 * A cookie's identity is its name, domain and path, and the port is not part of
 * it. Environments are served from the same host on different ports, so a fixed
 * name lets one environment overwrite another's session. Naming the cookie after
 * the environment keeps the sessions apart, because each environment then reads
 * only the cookie it wrote.
 *
 * The environment id is reduced to the RFC 6265 token characters, so the name is
 * always a valid cookie name whatever the id contains.
 */
export function sessionCookieName(environmentId: string): string {
    const token = environmentId.replace(/[^A-Za-z0-9_-]/g, '_');
    return `ores_web_session_${token}`;
}

/**
 * Policy reads allowed per client per minute.
 *
 * A read that opens a connection to the broker with no session, so a stranger
 * can drive it. It is a page view rather than an attempt, so the budget is far
 * larger than the sign-in one and it is a separate limiter: a person reloading
 * the door must not spend the attempts they need to sign in with.
 */
const POLICY_READS_PER_MINUTE = 120;

/**
 * How many bytes of tenant pictures and flags the BFF keeps.
 *
 * A staff photo is about a hundred kilobytes and a flag is a few, so this holds
 * several hundred pages' worth while staying a small part of the process.
 */
const TENANT_IMAGE_CACHE_BYTES = 64 * 1024 * 1024;

/** One page of parties, as the browser asks for it: a search, an order and the bounds. */
const partyPageQuerySchema = z.object({
    search: z.string().max(200),
    sort: z.string().max(100),
    descending: z.boolean(),
    offset: z.int().nonnegative(),
    limit: z.int().min(1).max(1000),
});

/** The step states that count as finished, as the run rail states them. */
const FINISHED_STEP_STATUSES = new Set(['completed', 'completed_with_warnings']);

/**
 * How many of a run's steps are finished, read from the run's own steps.
 *
 * A graph run may have several steps in flight, so the run summary carries no
 * current step and the count comes from the progress read. A read that fails
 * counts as none done, so one run whose progress is unavailable does not fail
 * the roster or the activity list that names it.
 */
async function readStepsDone(client: OresClient, instanceId: string): Promise<number> {
    try {
        const progress = await client.workflowProgress(instanceId);
        return progress.steps.filter((step) => FINISHED_STEP_STATUSES.has(step.status)).length;
    } catch {
        return 0;
    }
}

export interface ServerDependencies {
    readonly config: Config;
    readonly site: LoadedSiteConfiguration;
    readonly sessions?: SessionStore;
    readonly loginLimiter?: RateLimiter;
    readonly policyLimiter?: RateLimiter;
    /** Injected in tests so no broker is needed. */
    readonly createClient?: () => { client: OresClient; connect: () => Promise<void> };
}

export function buildServer(dependencies: ServerDependencies): FastifyInstance {
    const { config, site } = dependencies;
    const sessionCookie = sessionCookieName(site.environment.id);
    const sessions =
        dependencies.sessions ?? createSessionStore({ ttlSeconds: config.session.ttlSeconds });
    const loginLimiter =
        dependencies.loginLimiter ??
        createRateLimiter({ maxAttempts: config.loginAttemptsPerMinute, windowSeconds: 60 });
    const policyLimiter =
        dependencies.policyLimiter ??
        createRateLimiter({ maxAttempts: POLICY_READS_PER_MINUTE, windowSeconds: 60 });
    const tenantImages = createImageCache(TENANT_IMAGE_CACHE_BYTES);

    const injectedClient = dependencies.createClient;
    const createClient = (): { client: OresClient; connect: () => Promise<void> } => {
        if (injectedClient !== undefined) {
            return injectedClient();
        }
        {
            const broker = resolveBroker(site.configuration, site.environment);
            const transport = new NatsTransport({
                server: broker.server,
                subjectPrefix: broker.subjectPrefix,
                tls: {
                    ca: readPem(broker.tls.ca, 'broker tls.ca'),
                    cert: readPem(broker.tls.cert, 'broker tls.cert'),
                    key: readPem(broker.tls.key, 'broker tls.key'),
                },
                name: 'ores.web.bff',
                // The C++ client's library defaults, made explicit so the behaviour is
                // visible here rather than inherited silently.
                reconnectWaitMs: 2_000,
                maxReconnectAttempts: 60,
            });
            return {
                client: new OresClient({ transport }),
                connect: () => transport.connect(),
            };
        }
    };

    const server = Fastify({
        logger: {
            level: config.logLevel,
            // Never log a credential or a token.
            redact: ['req.headers.cookie', 'req.headers.authorization'],
        },
        genReqId: () => randomUUID(),
    });

    function readSessionId(request: FastifyRequest): string | undefined {
        return request.cookies[sessionCookie];
    }

    function setSessionCookie(reply: FastifyReply, id: string): void {
        reply.setCookie(sessionCookie, id, {
            path: '/',
            httpOnly: true,
            sameSite: 'lax',
            secure: config.session.cookieSecure,
            maxAge: config.session.ttlSeconds,
        });
    }

    function clearSessionCookie(reply: FastifyReply): void {
        reply.clearCookie(sessionCookie, { path: '/' });
    }

    function requireSession(request: FastifyRequest): LiveSession {
        const id = readSessionId(request);
        const session = id === undefined ? undefined : sessions.get(id);
        if (session === undefined) {
            throw notAuthenticated();
        }
        return session;
    }

    /**
     * Whether the system provisioner wizard recorded that it finished.
     *
     * The settings table is not readable without a session, and the status
     * route is unauthenticated, so the read goes through the session the browser
     * presents when it has one. A browser with no session, or a read the server
     * refuses, answers false: an installation that cannot prove the wizard
     * finished stays where it is, which is what a deployment that never ran it
     * does today.
     */
    async function onboardingComplete(session: LiveSession | undefined): Promise<boolean> {
        if (session === undefined) {
            return false;
        }
        try {
            return await session.client.onboardingSystemComplete();
        } catch {
            return false;
        }
    }

    /**
     * Whether the caller's tenant recorded that its own setup run finished.
     *
     * The tenant setting is read through the session the caller presents, and a
     * browser with no session answers false: a visitor is not a tenant
     * administrator, and a tenant whose flag cannot be read is held on its setup
     * screen rather than let through. The read is refused for a session that is
     * not the tenant's, which the refusal already covers.
     */
    async function onboardingTenantComplete(session: LiveSession | undefined): Promise<boolean> {
        if (session === undefined) {
            return false;
        }
        try {
            return await session.client.onboardingTenantComplete();
        } catch {
            return false;
        }
    }

    /**
     * Whether the tenant's own setup run has finished, read through the session.
     *
     * The status the session carries is a snapshot the login took, so a session
     * opened while its tenant was bootstrapping still reports it that way after
     * the run ends. The tenant's setup run is the run the rail follows, and its
     * completing step is what makes the tenant active, so the run's own finish
     * is the fact that ends the rail. A read that fails, or a tenant with no
     * run, leaves the snapshot as it stands: nothing is claimed that was not
     * read.
     */
    async function tenantSetupFinished(session: LiveSession): Promise<boolean> {
        try {
            const run = await readTenantSetupRun(session.client);
            return run.status === 'completed';
        } catch {
            return false;
        }
    }

    /**
     * Ends the tenant setup rail for a session whose tenant has finished.
     *
     * Read wherever the session view is served: the person who finished the run
     * is the one still holding the rail, and the run's record is what releases
     * them. A session that is not bootstrapping is left alone, so the read
     * happens only while a rail is up.
     */
    async function refreshTenantBootstrapping(session: LiveSession): Promise<LiveSession> {
        if (!session.tenantBootstrapping) {
            return session;
        }
        if (!(await tenantSetupFinished(session))) {
            return session;
        }
        return sessions.tenantBootstrapped(session.id) ?? session;
    }

    /**
     * The session the browser presents, or nothing when it presents none.
     *
     * Two of the bootstrap answer's fields are read through a session while the
     * rest are read without one, because a deployment in bootstrap mode has
     * nobody to sign in as. So the answer states which account it was read for,
     * and a screen can tell an answer about the visitor from one about the
     * account that has just signed in: the first is what the deployment says to
     * somebody standing outside it, and only the second is a fact about the
     * session in hand.
     */
    function liveSession(request: FastifyRequest): LiveSession | undefined {
        const id = readSessionId(request);
        return id === undefined ? undefined : sessions.get(id);
    }

    function sessionResponse(session: LiveSession): unknown {
        return sessionViewSchema.parse({
            username: session.username,
            email: session.email,
            accountId: session.accountId,
            tenantId: session.tenantId,
            tenantName: session.tenantName,
            tenantBootstrapping: session.tenantBootstrapping,
            mode: session.mode,
            version: session.version,
            database: session.database,
            party: session.party,
            availableParties: session.availableParties,
            accessLifetimeSeconds: session.accessLifetimeSeconds,
            passwordResetRequired: session.passwordResetRequired,
        });
    }

    function loginResult(outcome: LoginOutcome): unknown {
        if (outcome.kind === 'party-selection-required') {
            return loginResultSchema.parse({
                outcome: 'party-required',
                username: outcome.username,
                email: outcome.email,
                accountId: outcome.accountId,
                tenantName: outcome.tenantName,
                tenantBootstrapping: outcome.tenantBootstrapping,
                version: outcome.version,
                database: outcome.database,
                availableParties: outcome.availableParties,
                defaultPartyId: outcome.defaultPartyId,
                passwordResetRequired: outcome.passwordResetRequired,
            });
        }
        if (outcome.kind === 'active') {
            return loginResultSchema.parse({
                outcome: 'active',
                session: {
                    username: outcome.username,
                    email: outcome.email,
                    accountId: outcome.accountId,
                    tenantId: outcome.tenantId,
                    tenantName: outcome.tenantName,
                    tenantBootstrapping: outcome.tenantBootstrapping,
                    mode: sessionModeFor(outcome.tenantId),
                    version: outcome.version,
                    database: outcome.database,
                    party: outcome.party,
                    availableParties: outcome.availableParties,
                    accessLifetimeSeconds: outcome.accessLifetimeSeconds,
                    passwordResetRequired: outcome.passwordResetRequired,
                },
            });
        }
        throw invalidCredentials(outcome.message);
    }

    /**
     * The allow-list exists for a development server on another port, and it
     * works because that server is a same-site origin: the session cookie is
     * `sameSite: 'lax'`, so a genuinely cross-site caller would pass this hook
     * and then arrive at the route without a cookie. Widen `sameSite` before
     * adding an origin that is not a subdomain of the one serving the cookie.
     */
    server.addHook('onRequest', async (request, reply) => {
        const origin = request.headers.origin;
        if (origin !== undefined && config.allowedOrigins.includes(origin)) {
            reply.header('Access-Control-Allow-Origin', origin);
            reply.header('Access-Control-Allow-Credentials', 'true');
            reply.header('Vary', 'Origin');
        }
        if (request.method === 'OPTIONS') {
            reply
                .header('Access-Control-Allow-Methods', 'GET,POST,PUT,DELETE,OPTIONS')
                .header('Access-Control-Allow-Headers', 'Content-Type');
            await reply.status(204).send();
        }
    });

    server.setErrorHandler(async (error, request, reply) => {
        const failure = error instanceof HttpFailure ? error : toHttpFailure(error);
        if (failure.status >= 500) {
            request.log.error({ err: error }, 'request failed');
        }
        await reply.status(failure.status).send(failure.body);
    });

    void server.register(cookie);

    server.get('/api/health', async () => ({ status: 'ok' }));

    /**
     * Whether the deployment still needs its first administrator, and whether
     * the setup job is finished.
     *
     * Unauthenticated on purpose: the interface has to decide what to render
     * before it can offer a sign-in, and a deployment in bootstrap mode has
     * nobody to sign in as. The sign-in route refuses in the same situation, so
     * this is what lets the interface not offer the form at all rather than
     * answer a credential with a refusal.
     *
     * The setup answer is composed here from two sources: IAM says whether the
     * first administrator exists and whether the deployment has a tenant of its
     * own, and a variability settings read says whether the system provisioner
     * wizard recorded that it finished. A first-run installation may keep only
     * the system tenant, so the wizard's flag is the fact that lets it leave the
     * setup screen. One connection per request, closed in both paths. It
     * carries no session, so there is nothing to keep alive.
     *
     * The two settings answers and the account they were read for travel
     * together: without the account a screen cannot tell an answer about the
     * visitor from one about the account that has just signed in, and would act
     * on a flag that was read from outside the deployment.
     */
    server.get('/api/bootstrap', async (request) => {
        const { client, connect } = createClient();
        const session = liveSession(request);
        try {
            await connect();
            const status = await client.bootstrapStatus();
            return bootstrapStatusSchema.parse({
                isInBootstrapMode: status.isInBootstrapMode,
                hasTenant: status.hasTenant,
                onboardingComplete: await onboardingComplete(session),
                onboardingTenantComplete: await onboardingTenantComplete(session),
                accountId: session?.accountId ?? '',
                sessionPresent: readSessionId(request) !== undefined,
                message: status.message,
                version: status.version,
            });
        } finally {
            await client.close().catch(() => undefined);
        }
    });

    /**
     * Creates the first administrator, which closes bootstrap mode.
     *
     * Unauthenticated for the same reason the status read is: the deployment has
     * no account to sign in with, and this is the request that makes one. It
     * grants SuperAdmin, so it is refused unless the deployment says it is still
     * in bootstrap mode. The function behind the subject does not check that
     * itself, and an unauthenticated request that could run it twice would be a
     * way to make a second super user with no session at all.
     */
    server.post('/api/bootstrap/administrator', async (request) => {
        const parsed = createAdministratorRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('A username, an email address and a password are required.');
        }
        const { client, connect } = createClient();
        try {
            await connect();
            const status = await client.bootstrapStatus();
            if (!status.isInBootstrapMode) {
                throw bootstrapComplete();
            }
            const created = await client.createInitialAdmin(parsed.data);
            if (!created.success) {
                throw invalidRequest(
                    created.errorMessage === ''
                        ? 'The administrator could not be created.'
                        : created.errorMessage,
                );
            }
            return initialAdministratorSchema.parse({
                accountId: created.accountId,
                tenantId: created.tenantId,
            });
        } finally {
            await client.close().catch(() => undefined);
        }
    });

    /**
     * Records that the first-run journey finished.
     *
     * The journey runs signed in, and it completes the setup job for the tenant
     * it is working in, so the session is the whole context and the route takes
     * no body. It exists for the installation that keeps only the system tenant:
     * the status read above would otherwise hold the browser on the setup rail
     * forever, because that installation has no tenant of its own. The
     * variability operation behind it is the component's, and its reply carries
     * a result the client turns into the same refusal every other write uses.
     */
    server.post('/api/bootstrap/complete', async (request) => {
        const session = requireSession(request);
        await session.client.completeSystemOnboarding();
        return { success: true };
    });

    /**
     * The tenant administrator's own setup run.
     *
     * The session names the tenant, so the request carries nothing and the run
     * cannot be another tenant's: the read is answered as this tenant's
     * administrator, and the engine returns only the runs that tenant owns. An
     * empty instance id is an answer rather than a failure, because a tenant
     * whose setup has not been started is a state the screen states plainly.
     */
    server.get('/api/tenant-setup', async (request) => {
        const session = requireSession(request);
        return tenantSetupRunSchema.parse(await readTenantSetupRun(session.client));
    });

    /**
     * What the deployment offers somebody who is not in it yet.
     *
     * Unauthenticated on purpose: this is the read the door makes before it
     * offers a form, and it answers the switch, the destination and whether a
     * registration can be used at once. The address the request arrived at
     * travels with it, because the tenant is resolved from the address rather
     * than typed into the form, and a registration that resolves to no tenant
     * is refused rather than landing in the system tenant by omission.
     *
     * A refusal is an answer rather than an error here: the screen states why
     * the door is closed, which is a state it renders rather than a failure it
     * recovers from.
     */
    server.get('/api/registration-policy', async (request) => {
        if (!policyLimiter.allow(request.ip)) {
            throw tooManyRequests('policy reads');
        }
        const { client, connect } = createClient();
        try {
            await connect();
            const policy = await client.registrationPolicy(request.hostname);
            return registrationPolicyViewSchema.parse({
                success: policy.success,
                message: policy.message,
                errorCode: policy.errorCode,
                signupsEnabled: policy.signupsEnabled,
                authorizationRequired: policy.authorizationRequired,
                tenantId: policy.tenantId,
                tenantName: policy.tenantName,
                partyId: policy.partyId,
                partyName: policy.partyName,
                roleId: policy.roleId,
                roleName: policy.roleName,
                usableNow: policy.usableNow,
            });
        } finally {
            await client.close().catch(() => undefined);
        }
    });

    /**
     * Registers an account.
     *
     * Unauthenticated, because the person has no account yet: this is the
     * request that makes one. It shares the sign-in limiter, because both are
     * unauthenticated writes a stranger can drive, and the thing being
     * protected is the same account.
     *
     * The answer is the account and its state, not a session: a pending account
     * cannot sign in, and an active one arrives at the door deliberately.
     */
    server.post('/api/signup', async (request) => {
        const parsed = signupRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('A username, an email address and a password are required.');
        }
        if (!loginLimiter.allow(request.ip)) {
            throw tooManyRequests('registration attempts');
        }

        const { client, connect } = createClient();
        try {
            await connect();
            const outcome = await client.signup({
                principal: parsed.data.principal,
                password: parsed.data.password,
                email: parsed.data.email,
                hostname: request.hostname,
            });
            if (!outcome.success) {
                throw signupRefused(outcome.errorCode, outcome.message);
            }
            return signupResultSchema.parse({
                success: outcome.success,
                message: outcome.message,
                errorCode: outcome.errorCode,
                accountId: outcome.accountId,
                accountStatus: outcome.accountStatus,
                partyId: outcome.partyId,
                roleId: outcome.roleId,
            });
        } finally {
            await client.close().catch(() => undefined);
        }
    });

    /**
     * What the interface needs to render itself.
     *
     * The environment is named so the interface can say so, and the developer
     * accounts are offered only when the deployment says so. Note what is absent:
     * the host, the port, the namespace and the certificates.
     */
    server.get('/api/site', async () =>
        siteStateSchema.parse({
            appName: 'ORE Studio',
            environment: {
                id: site.environment.id,
                displayName: site.environment.displayName,
                description: site.environment.description,
                nonProduction: site.environment.nonProduction,
            },
            developerTools: site.configuration.developerTools,
            developerAccounts: site.configuration.developerTools
                ? site.configuration.developerAccounts
                : [],
        }),
    );

    /**
     * The deployment's plumbing, for the developer page.
     *
     * Absent unless the deployment offers the developer surface, because it names
     * the host, the port and the namespace, and there is no reason for an
     * ordinary deployment to expose any of that to a browser.
     */
    server.get('/api/site/deployment', async (_request, reply) => {
        if (!site.configuration.developerTools) {
            return reply.status(404).send({
                code: 'invalid-request',
                message: 'No developer surface on this deployment.',
            });
        }
        return deploymentViewSchema.parse({
            environment: site.environment,
            configFile: site.source,
            developerTools: site.configuration.developerTools,
            available: site.configuration.environments.map((environment) => ({
                id: environment.id,
                displayName: environment.displayName,
                nonProduction: environment.nonProduction,
            })),
        });
    });

    server.post('/api/session', async (request, reply) => {
        const parsed = credentialsSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('A username and password are required.');
        }
        if (!loginLimiter.allow(request.ip)) {
            throw invalidCredentials('Too many attempts. Wait a minute and try again.');
        }

        const { client, connect } = createClient();
        try {
            await connect();

            /*
             * Asked before the credentials are used, because a deployment in
             * bootstrap mode has no accounts and a rejected login would send somebody
             * hunting for a password that cannot exist. The Qt client checked the
             * same thing in the same place: before the form, not after it.
             */
            const bootstrap = await client.bootstrapStatus();
            if (bootstrap.isInBootstrapMode) {
                await client.close().catch(() => undefined);
                throw bootstrapRequired();
            }

            const outcome = await client.login({
                principal: parsed.data.username,
                password: parsed.data.password,
            });

            if (outcome.kind === 'rejected') {
                await client.close().catch(() => undefined);
                throw invalidCredentials(outcome.message);
            }

            const session = sessions.create({
                client,
                session: outcome.kind === 'active' ? outcome : null,
                username: outcome.username,
                email: outcome.email,
                accountId: outcome.accountId,
                /*
                 * The tenant from the login, whether or not a party has been chosen yet.
                 *
                 * It is on both outcomes, and discarding it for the party-choice case
                 * left the session with no tenant at all until one was picked — and
                 * picking one did not put it back. Every write from such an account then
                 * carried an empty tenant, which the service cannot even decode, so the
                 * failure arrived as a bad request with nothing to say what was wrong.
                 */
                tenantId: outcome.tenantId,
                tenantName: outcome.tenantName,
                tenantBootstrapping: outcome.tenantBootstrapping,
                mode: sessionModeFor(outcome.tenantId),
                version: outcome.version,
                database: outcome.database,
                availableParties: outcome.availableParties,
                accessLifetimeSeconds: outcome.accessLifetimeSeconds,
                passwordResetRequired: outcome.passwordResetRequired,
                sessionId: client.currentSessionId,
            });
            setSessionCookie(reply, session.id);
            return loginResult(outcome);
        } catch (error) {
            await client.close().catch(() => undefined);
            throw error;
        }
    });

    server.get('/api/session', async (request) =>
        sessionResponse(await refreshTenantBootstrapping(requireSession(request))),
    );

    server.delete('/api/session', async (request, reply) => {
        const id = readSessionId(request);
        if (id !== undefined) {
            await sessions.destroy(id);
        }
        clearSessionCookie(reply);
        return { ok: true };
    });

    server.post('/api/session/party', async (request) => {
        const session = requireSession(request);
        const parsed = selectPartyRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('A partyId is required.');
        }

        const outcome = await session.client.selectParty({
            partyId: parsed.data.partyId,
            expected: {
                kind: 'party-selection-required',
                accountId: session.accountId,
                tenantId: session.tenantId,
                tenantName: session.tenantName,
                tenantBootstrapping: session.tenantBootstrapping,
                version: session.version,
                database: session.database,
                username: session.username,
                email: session.email,
                availableParties: session.availableParties as readonly PartySummary[],
                defaultPartyId: null,
                passwordResetRequired: session.passwordResetRequired,
                accessLifetimeSeconds: session.accessLifetimeSeconds,
                sessionId: session.sessionId,
            },
        });

        const activated = sessions.activate(session.id, outcome);
        if (activated === undefined) {
            throw new NotAuthenticatedError('Session ended during party selection');
        }
        return sessionResponse(activated);
    });

    /**
     * Re-scopes an open session to another of the account's parties.
     *
     * The session's list of parties is the login's answer, so a party added
     * since then is not in it. This route reads the tenant's parties, states
     * the chosen one in the session, and asks the server for a token scoped to
     * it. The server decides whether the account may work in the party: an
     * account that is not a member is refused here, which is what a party
     * journey that has not finished its run will see.
     */
    server.post('/api/session/switch-party', async (request) => {
        const session = requireSession(request);
        const parsed = selectPartyRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('A partyId is required.');
        }

        const parties = await session.client.listParties();
        const wanted = parties.find((party) => party.id === parsed.data.partyId);
        if (wanted === undefined) {
            throw invalidRequest('The tenant holds no such party.');
        }
        const summary: PartySummary = {
            id: wanted.id,
            name: wanted.full_name,
            partyCategory: wanted.party_category,
            businessCenterCode: wanted.business_center_code,
        };
        const availableParties = [
            ...session.availableParties.filter((party) => party.id !== summary.id),
            summary,
        ];

        const outcome = await session.client.switchParty({
            partyId: summary.id,
            availableParties,
        });
        const switched = sessions.switchParty(
            session.id,
            summary,
            availableParties,
            outcome.accessLifetimeSeconds,
        );
        if (switched === undefined) {
            throw new NotAuthenticatedError('Session ended during party switch');
        }
        return sessionResponse(switched);
    });

    /**
     * The signed-in account's own password.
     *
     * A person who must change their password has a session already: they
     * signed in with the password the deployment gave them, and this is what
     * replaces it with one only they know. Nobody else's password is reachable
     * here, and the account is the session's own, so the route takes no
     * account id.
     */
    server.post('/api/account/password', async (request) => {
        const session = requireSession(request);
        const parsed = changeOwnPasswordRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('The current password and the new password are required.');
        }
        await changeOwnPassword(session.client, parsed.data);
        /*
         * The session's copy of the flag is a snapshot of the login, so it is
         * cleared here rather than read again: the account has just done what
         * the flag asked for.
         */
        sessions.passwordChanged(session.id);
        return { success: true };
    });

    /**
     * The rules a password must satisfy.
     *
     * Served without a session, like the bootstrap read beside it, because the
     * screen that shows the rules is the one a person signs in on. The rules
     * come from the server's validator, so the screen states them rather than
     * keeping a copy of them.
     */
    server.get('/api/password-policy', async () => {
        const { client, connect } = createClient();
        try {
            await connect();
            return passwordPolicySchema.parse(await client.passwordPolicy());
        } finally {
            await client.close().catch(() => undefined);
        }
    });

    /**
     * The accounts the tenant's administrator may see.
     *
     * The administrator's screen opens on this list and finds the colleague the
     * support call is about, so the read is the screen's first step. It answers
     * a page rather than everything, because the number of accounts a tenant
     * holds is not a number a screen has any use for.
     */
    server.get('/api/accounts', async (request) => {
        const session = requireSession(request);
        const page = accountPageQuerySchema.safeParse(request.query);
        if (!page.success) {
            throw invalidRequest(
                'A page of people names an offset, a limit of 1 to 1000, a search, and an order by username or name.',
            );
        }
        return readAccountsPage(session.client, page.data);
    });

    /*
     * Access: the roles a person holds, the tenant's role catalogue, and the
     * writes that change either. The server checks every permission; the BFF
     * parses the browser's input and turns a refusal into words.
     */

    /**
     * The picture of one account of the session's own tenant, by username.
     *
     * Wherever a screen names an account it shows its picture, and most of
     * those places know a username rather than an image: the signed-in person,
     * who gave a role. An account with no picture answers 404, and the screen
     * shows initials. Cached briefly, because a person may change their picture.
     */
    server.get('/api/accounts/:username/picture', async (request, reply) => {
        const session = requireSession(request);
        const { username } = request.params as { username: string };
        const account = await readAccount(session.client, username);
        const [image] =
            account === null || account.imageId === null
                ? []
                : await readImages(session.client, [account.imageId]);
        if (image === undefined) {
            throw notFound('This account has no picture.');
        }
        return reply
            .header('content-type', image.mimeType)
            .header('cache-control', 'private, max-age=300')
            .header('x-content-type-options', 'nosniff')
            .header('content-security-policy', "default-src 'none'; sandbox")
            .send(image.bytes);
    });

    /** The roles the signed-in person holds, with who gave each one and why. */
    server.get('/api/me/access', async (request) => {
        const session = requireSession(request);
        return accountAccessSchema.parse(await readMyAccess(session.client));
    });

    /**
     * The parties the signed-in person works in, each with its name, and the
     * party quick sign-in uses. The association read carries an identifier
     * alone, so the server names them.
     */
    server.get('/api/me/parties', async (request) => {
        const session = requireSession(request);
        return myPartiesSchema.parse(await readMyParties(session.client));
    });

    /**
     * Sets or clears the party quick sign-in uses. An empty partyId clears it.
     * The server refuses a party the account does not work in.
     */
    server.post('/api/me/default-party', async (request) => {
        const session = requireSession(request);
        const parsed = defaultPartyRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest(
                'A partyId is required. Send an empty string to clear the default.',
            );
        }
        await setMyDefaultParty(session.client, parsed.data.partyId);
        return { defaultPartyId: parsed.data.partyId };
    });

    /**
     * Sets or clears who one account reports to, and nothing else. The server
     * allows it to a holder of iam::accounts:update and refuses the write as a
     * conflict when the account has moved on since the screen read it.
     */
    server.put('/api/accounts/:accountId/reporting-line', async (request) => {
        const session = requireSession(request);
        const { accountId } = request.params as { accountId: string };
        const parsed = reportingLineRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest(
                'A reportsToAccountId is required. Send an empty string to clear the line.',
            );
        }
        return accountSchema.parse(
            await setReportingLine(session.client, accountId, parsed.data),
        );
    });

    /**
     * The tenant's reporting shape in one read, or one account's branch of it
     * when a root is named. The server needs iam::accounts:read.
     */
    server.get('/api/reporting-tree', async (request) => {
        const session = requireSession(request);
        const { root } = request.query as { root?: string };
        return reportingTreeSchema.parse(await readReportingTree(session.client, root ?? ''));
    });

    /** The roles one account holds. The server allows it to a holder of iam::roles:read. */
    server.get('/api/accounts/:accountId/access', async (request) => {
        const session = requireSession(request);
        const { accountId } = request.params as { accountId: string };
        if (!isUuid(accountId)) {
            throw notFound('No account has this identifier.');
        }
        return accountAccessSchema.parse(await readAccountAccess(session.client, accountId));
    });

    /** What the browser sends to give a role: the role, the reason and a note. */
    const giveRoleBodySchema = z.object({
        roleId: z.string().refine(isUuid, 'A role is an identifier.'),
        reasonCode: z.string().min(1).max(100),
        note: z.string().max(2000).default(''),
    });

    /** Gives a role to an account, for a reason. A refusal is the server's words. */
    server.post('/api/accounts/:accountId/roles', async (request, reply) => {
        const session = requireSession(request);
        const { accountId } = request.params as { accountId: string };
        const body = giveRoleBodySchema.safeParse(request.body);
        if (!isUuid(accountId) || !body.success) {
            throw invalidRequest('Choose a role and a reason.');
        }
        const outcome = await giveRole(session.client, { accountId, ...body.data });
        if (!outcome.done) {
            throw new HttpFailure(409, { code: 'conflict', message: outcome.message });
        }
        return reply.code(204).send();
    });

    /** Takes a role away from an account. The server refuses one's own account. */
    server.delete('/api/accounts/:accountId/roles/:roleId', async (request, reply) => {
        const session = requireSession(request);
        const { accountId, roleId } = request.params as { accountId: string; roleId: string };
        if (!isUuid(accountId) || !isUuid(roleId)) {
            throw notFound('No such role on this account.');
        }
        const outcome = await takeRoleAway(session.client, { accountId, roleId });
        if (!outcome.done) {
            throw new HttpFailure(409, { code: 'conflict', message: outcome.message });
        }
        return reply.code(204).send();
    });

    /** The tenant's roles, each with the permissions it grants. */
    server.get('/api/roles', async (request) => {
        const session = requireSession(request);
        return { roles: z.array(roleSummarySchema).parse(await readRoles(session.client)) };
    });

    /** Every permission the platform defines. */
    server.get('/api/permissions', async (request) => {
        const session = requireSession(request);
        return {
            permissions: z
                .array(permissionEntrySchema)
                .parse(await readPermissionCatalogue(session.client)),
        };
    });

    /** What the browser sends to create a role: its name and description. */
    const newRoleBodySchema = z.object({
        name: z.string().trim().min(1).max(100),
        description: z.string().max(1000).default(''),
    });

    /** What the browser sends to rename a role, against the version it read. */
    const roleBodySchema = newRoleBodySchema.extend({
        version: z.int().nonnegative(),
        registrationDefault: z.boolean(),
        requestable: z.boolean(),
    });

    /** Creates a role. It starts granting nothing. */
    server.post('/api/roles', async (request) => {
        const session = requireSession(request);
        const body = newRoleBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A role needs a name.');
        }
        const id = randomUUID();
        const outcome = await saveRole(session.client, {
            ...body.data,
            id,
            version: null,
            registrationDefault: false,
            requestable: true,
        });
        if (!outcome.done) {
            throw new HttpFailure(409, { code: 'conflict', message: outcome.message });
        }
        return { id };
    });

    /** Renames or redescribes a role, against the version the screen read. */
    server.put('/api/roles/:roleId', async (request, reply) => {
        const session = requireSession(request);
        const { roleId } = request.params as { roleId: string };
        const body = roleBodySchema.safeParse(request.body);
        if (!isUuid(roleId) || !body.success) {
            throw invalidRequest('A role needs a name and the version it was read at.');
        }
        const outcome = await saveRole(session.client, { ...body.data, id: roleId });
        if (!outcome.done) {
            throw new HttpFailure(409, { code: 'conflict', message: outcome.message });
        }
        return reply.code(204).send();
    });

    /** What the browser sends to save what a role grants: the whole set, and a note. */
    const bundleBodySchema = z.object({
        codes: z.array(z.string().min(1).max(200)).max(2000),
        note: z.string().max(2000).default(''),
    });

    /** Replaces what a role grants with exactly the set sent, and answers what was stored. */
    server.put('/api/roles/:roleId/permissions', async (request) => {
        const session = requireSession(request);
        const { roleId } = request.params as { roleId: string };
        const body = bundleBodySchema.safeParse(request.body);
        if (!isUuid(roleId) || !body.success) {
            throw invalidRequest('Send the permissions the role grants.');
        }
        return {
            codes: await saveRolePermissions(
                session.client,
                roleId,
                body.data.codes,
                body.data.note,
            ),
        };
    });

    /** Deletes a role by its name. */
    server.delete('/api/roles/:name', async (request, reply) => {
        const session = requireSession(request);
        const { name } = request.params as { name: string };
        const outcome = await deleteRole(session.client, name);
        if (!outcome.done) {
            throw new HttpFailure(409, { code: 'conflict', message: outcome.message });
        }
        return reply.code(204).send();
    });

    /**
     * One account of the session's own tenant and its sign-ins: its sign-in
     * state and its sessions, newest first. The server allows the session
     * read to a holder of iam::sessions:read.
     */
    server.get('/api/accounts/:username/sign-ins', async (request) => {
        const session = requireSession(request);
        const page = signInsPage(request);
        const { username } = request.params as { username: string };
        const answer = await readAccountSignIns(session.client, username, page);
        if (answer === null) {
            throw notFound('No account has this username.');
        }
        return accountSignInsSchema.parse(answer);
    });

    /**
     * One account by username.
     *
     * A username that is not there answers with nothing rather than with a 404.
     * The caller names a row it has just read out of the list, so the row having
     * gone since is an expected state and not a failed call, and it is the same
     * state as an account that has never signed in: the screen shows it and the
     * person asks again.
     */
    server.get('/api/accounts/:username', async (request) => {
        const session = requireSession(request);
        const params = request.params as { username?: string };
        const username = params.username ?? '';
        if (username.length === 0) {
            throw invalidRequest('A username is required.');
        }
        return { account: await readAccount(session.client, username) };
    });

    /**
     * Locks one account.
     *
     * A lock refuses the next sign-in and leaves every session the account
     * already holds open, which is why the screen says so beside the control: an
     * administrator who expects a lock to end a stolen session has to know that
     * it does not.
     */
    server.post('/api/accounts/:accountId/lock', async (request) => {
        const session = requireSession(request);
        const params = request.params as { accountId?: string };
        const accountId = params.accountId ?? '';
        if (accountId.length === 0) {
            throw invalidRequest('An account id is required.');
        }
        await setAccountLocked(session.client, { accountId, locked: true });
        return { success: true };
    });

    /**
     * Unlocks one account.
     *
     * The server clears the failed attempt count as part of the write, so an
     * account unlocked after a run of failed attempts starts the count again
     * rather than one attempt from locking itself.
     */
    server.post('/api/accounts/:accountId/unlock', async (request) => {
        const session = requireSession(request);
        const params = request.params as { accountId?: string };
        const accountId = params.accountId ?? '';
        if (accountId.length === 0) {
            throw invalidRequest('An account id is required.');
        }
        await setAccountLocked(session.client, { accountId, locked: false });
        return { success: true };
    });

    /*
     * The profile: the account and the contact record a person owns, and the
     * two writes a tenant administrator makes on somebody else's. The self
     * writes name no account, because the session names it; the administered
     * writes name the account the screen shows, and the server refuses a
     * caller who does not hold the permission. A refusal a panel must draw --
     * a field a member does not own, a record that moved under a save --
     * travels in the answer body rather than as a failed call.
     */

    /**
     * Writes the signed-in person's own profile fields.
     *
     * The three fields a member does not own -- the sign-in address, the
     * default party, the reporting line -- are sent empty, which the protocol
     * reads as not stated. The session names the account, so the body carries
     * no account id and cannot name another one.
     */
    server.put('/api/me/profile', async (request) => {
        const session = requireSession(request);
        const body = profileWriteSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('The profile fields must be text.');
        }
        return accountWriteViewSchema.parse(await updateSelfAccount(session.client, body.data));
    });

    /**
     * The signed-in person's own contact record.
     *
     * The session names the account, so the route takes no id, and an account
     * with no record yet answers with nothing rather than with an error: the
     * first write creates it.
     */
    server.get('/api/me/contact-information', async (request) => {
        const session = requireSession(request);
        return contactViewSchema.parse({
            contact: await readMyContactInformation(session.client),
        });
    });

    /**
     * Writes the signed-in person's own contact record.
     *
     * It names no record; an account without one gets it on the first write.
     */
    server.put('/api/me/contact-information', async (request) => {
        const session = requireSession(request);
        const body = contactWriteSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('The contact fields must be text.');
        }
        return contactWriteViewSchema.parse(
            await updateSelfContactInformation(session.client, body.data),
        );
    });

    /**
     * Writes one account's profile fields, as a tenant administrator.
     *
     * The server's write replaces the record whole, so the route reads the
     * account inside the request and applies the three fields to that read:
     * the fields this screen does not set are echoed, and an empty string
     * would clear them. The server asks for iam::accounts:update.
     */
    server.put('/api/accounts/:username/profile', async (request) => {
        const session = requireSession(request);
        const { username } = request.params as { username: string };
        const body = profileWriteSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('The profile fields must be text.');
        }
        const account = await readAccount(session.client, username);
        if (account === null) {
            throw notFound('No account has this username.');
        }
        await updateAccount(session.client, account, body.data);
        return { success: true };
    });

    /**
     * One account's contact record, as a tenant administrator reads it.
     *
     * The server's read needs iam::account_contact_informations:read, and this
     * mirrors the access pair: the member's own record is the route without an
     * id, served by the self read, and this is the administrator's.
     */
    server.get('/api/accounts/:accountId/contact-information', async (request) => {
        const session = requireSession(request);
        const { accountId } = request.params as { accountId: string };
        if (!isUuid(accountId)) {
            throw notFound('No account has this identifier.');
        }
        return contactViewSchema.parse({
            contact: await readContactInformation(session.client, accountId),
        });
    });

    /**
     * Writes one account's contact record, as a tenant administrator.
     *
     * The browser states the claim its panel read: the version of the record
     * it showed, or nothing at all. The id is the route's business, never the
     * browser's: the store checks the claim against the row that id names, so
     * the route reuses the identifier of the account's current record, and
     * mints one only when the account has none. A claim of no record then
     * finds the row the panel did not see and is refused, rather than
     * inserting a second record for the account.
     */
    server.put('/api/accounts/:accountId/contact-information', async (request) => {
        const session = requireSession(request);
        const { accountId } = request.params as { accountId: string };
        if (!isUuid(accountId)) {
            throw notFound('No account has this identifier.');
        }
        const body = claimedContactWriteSchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('Send the contact fields and the version the panel read.');
        }
        const current = await readContactInformation(session.client, accountId);
        const { version, ...write } = body.data;
        return contactWriteViewSchema.parse(
            await putContactInformation(session.client, {
                accountId,
                recordId: current?.id ?? randomUUID(),
                claim:
                    version === null
                        ? { kind: 'must_not_exist', version: null }
                        : { kind: 'must_match_version', version },
                write,
            }),
        );
    });

    /**
     * The tenant's login records, one page at a time.
     *
     * The audit screen reads the failed attempts from this list, and a locked
     * account explains the support call the administrator is on.
     */
    server.get('/api/login-info', async (request) => {
        const session = requireSession(request);
        const query = request.query as Record<string, string | undefined>;
        const page = listLoginInfoRequestSchema.safeParse({
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 100),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return readLoginInfoPage(session.client, page.data);
    });

    /**
     * One account's login record.
     *
     * A record is written by signing in, so an account that has never signed in
     * has none, and that is an answer rather than an error. The record carries
     * no credential column, so nothing secret travels with it.
     */
    server.get('/api/login-info/:accountId', async (request) => {
        const session = requireSession(request);
        const params = request.params as { accountId?: string };
        const key = loginInfoKeyRequestSchema.safeParse({
            key: { account_id: params.accountId ?? '' },
        });
        if (!key.success) {
            throw invalidRequest('An account id is required.');
        }
        return { loginInfo: await readLoginInfo(session.client, key.data.key.account_id) };
    });

    /**
     * The tenant's sessions, one page at a time.
     *
     * The audit screen reads the sign-in record from here: who is signed in,
     * from where, and how much they have moved. A session with no end time is
     * an open one, which is what makes a row the kind an administrator acts on.
     */
    server.get('/api/sessions', async (request) => {
        const session = requireSession(request);
        const query = request.query as Record<string, string | undefined>;
        const page = listSessionsRequestSchema.safeParse({
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 100),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return readSessionsPage(session.client, page.data);
    });

    /**
     * The tenant's sessions with no end time, every account's.
     *
     * The administrator's audit of sign-ins reads this. A person's own screen
     * reads the route below, which keeps only their sessions.
     */
    server.get('/api/sessions/active', async (request) => {
        const session = requireSession(request);
        return readActiveSessions(session.client);
    });

    /**
     * The signed-in person's own sessions with no end time.
     *
     * The server answers the tenant's open sessions, the platform's own
     * services' among them in the system tenant, so the read keeps the rows
     * of the session's account and no other.
     */
    server.get('/api/me/sessions', async (request) => {
        const session = requireSession(request);
        const answer = await readActiveSessions(session.client);
        return {
            ...answer,
            sessions: answer.sessions.filter((row) => row.accountId === session.accountId),
        };
    });

    /** What the roster may ask for: a search, an order and one page of the matches. */
    const tenantRosterQuerySchema = z.object({
        search: z.string().max(200),
        type: z.string().max(100),
        status: z.string().max(100),
        includeTest: z.boolean(),
        sort: z.string().max(100),
        descending: z.boolean(),
        offset: z.int().nonnegative(),
        limit: z.int().positive().max(1000),
    });

    /** The tenant type that marks test infrastructure, hidden unless asked for. */
    const TEST_TENANT_TYPE = 'automation';

    /** The deployment's own tenant, which the home page does not count. */
    const SYSTEM_TENANT_TYPE = 'system';

    /**
     * The tenants this deployment holds, one page of the ones that match.
     *
     * The roster the system administration area reads, and the list a screen
     * picks a tenant from before it retires or resets one. The search matches
     * code, name and hostname. The system tenant is listed like any other, so
     * a system administrator can open the deployment's own tenant. The read
     * names the types a row may have, so the server leaves out the test
     * tenants unless they are asked for; the page and its total then agree.
     */
    server.get('/api/tenants', async (request) => {
        const session = requireSession(request);
        /*
         * The roster is the deployment's, so it belongs to the context that
         * acts on the deployment. The mode is what the server stated about this
         * session, so refusing here is not a second opinion about the caller:
         * it is the same fact, used as a boundary. The list subject checks no
         * permission of its own, and until it does this is what keeps a
         * tenant's administrator from reading every tenant's registry row.
         */
        if (session.mode !== 'system-administration') {
            throw notPermitted('The tenants of a deployment are read in system administration.');
        }
        const query = request.query as Record<string, string | undefined>;
        const page = tenantRosterQuerySchema.safeParse({
            search: query['search'] ?? '',
            type: query['type'] ?? '',
            status: query['status'] ?? '',
            includeTest: query['includeTest'] === 'true',
            sort: query['sort'] ?? '',
            descending: query['descending'] === 'true',
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 100),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        /*
         * Test infrastructure is hidden unless the person asks for it or asks
         * for that type by name, and a second count says how many were hidden.
         * The types come from the deployment's own rows, so a new type shows
         * without a change here.
         */
        const hideTest = !page.data.includeTest && page.data.type !== TEST_TENANT_TYPE;
        const shown = (await readTenantTypes(session.client))
            .map((type) => type.code)
            .filter((code) => !(hideTest && code === TEST_TENANT_TYPE));
        const read = await listTenantsPage(session.client, {
            search: page.data.search,
            type: page.data.type,
            status: page.data.status,
            types: shown,
            sort: page.data.sort,
            descending: page.data.descending,
            offset: page.data.offset,
            limit: page.data.limit,
        });
        const hiddenTestCount =
            hideTest && page.data.type === ''
                ? (
                      await listTenantsPage(session.client, {
                          search: page.data.search,
                          status: page.data.status,
                          types: [TEST_TENANT_TYPE],
                          limit: 1,
                      })
                  ).totalCount
                : 0;
        /*
         * Each tenant is joined with the run that provisioned it, which is how a
         * person who left the journey finds the work again. The runs belong to
         * the session's own tenant and name the tenant they act on as their
         * target, so one read naming the tenants on the page answers every row.
         * A failure to read them is not a
         * failure to read the roster: the rows go out without a run, and the
         * page is told the runs are missing rather than that there are none.
         */
        let setups: ReadonlyMap<string, TenantSetup> = new Map();
        let setupUnavailable = false;
        try {
            const runs = await readTenantSetups(
                session.client,
                read.tenants.map((tenant) => tenant.id),
                (instanceId) => readStepsDone(session.client, instanceId),
            );
            setups = runs.setups;
            if (!runs.complete) {
                request.log.warn(
                    'The provisioning run read reached its limit; the oldest runs are not shown.',
                );
            }
        } catch (error) {
            request.log.warn({ err: error }, 'The provisioning runs were not read.');
            setupUnavailable = true;
        }
        return tenantPageSchema.parse({
            tenants: read.tenants.map((tenant) => ({
                ...tenant,
                setup: setups.get(tenant.id) ?? null,
            })),
            totalCount: read.totalCount,
            setupUnavailable,
            hiddenTestCount,
        });
    });

    /** How many tenants and runs the system administrator's home shows. */
    const OVERVIEW_TENANTS = 5;
    const OVERVIEW_ACTIVITY = 5;
    const OVERVIEW_ATTENTION = 10;

    /**
     * The state of the deployment's tenants, for the system administrator's
     * home: how many are in service, on evaluation and setting up, which need
     * attention and why, the first page of the roster, and the newest setups.
     *
     * Every count is a list read that asks for its total, so the counts are
     * the registry's own and need no page of rows. The counts leave out the
     * system tenant and test tenants, as the roster does. The runs are a
     * second source: a failure to read them is not a failure to read the
     * tenants, so the page goes out without activity and says so.
     */
    server.get('/api/overview', async (request) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('The deployment overview is read in system administration.');
        }
        const shown = (await readTenantTypes(session.client))
            .map((type) => type.code)
            .filter((code) => code !== SYSTEM_TENANT_TYPE && code !== TEST_TENANT_TYPE);
        const count = async (query: { status?: string; type?: string }) =>
            (await listTenantsPage(session.client, { ...query, types: shown, limit: 1 }))
                .totalCount;
        const [inService, onEvaluation, bootstrapping, suspended, first] = await Promise.all([
            count({ status: 'active' }),
            count({ type: 'evaluation' }),
            count({ status: 'bootstrapping' }),
            listTenantsPage(session.client, {
                status: 'suspended',
                types: shown,
                limit: OVERVIEW_ATTENTION,
            }),
            listTenantsPage(session.client, { types: shown, limit: OVERVIEW_TENANTS }),
        ]);

        let activity: SetupActivity[] = [];
        let failedSetups: TenantSummary[] = [];
        let setups: ReadonlyMap<string, TenantSetup> = new Map();
        let activityUnavailable = false;
        try {
            const [recent, failed] = await Promise.all([
                readProvisioningRuns(session.client, { limit: OVERVIEW_ACTIVITY }),
                readProvisioningRuns(session.client, {
                    status: 'failed',
                    limit: OVERVIEW_ATTENTION,
                }),
            ]);
            const ids = [...new Set([...recent, ...failed].map((run) => run.tenantId))];
            const named =
                ids.length === 0
                    ? []
                    : (
                          await listTenantsPage(session.client, {
                              ids,
                              types: shown,
                              limit: ids.length,
                          })
                      ).tenants;
            const byId = new Map<string, TenantSummary>(named.map((tenant) => [tenant.id, tenant]));
            const activityEntries = recent
                .map((run) => ({ run, tenant: byId.get(run.tenantId) }))
                .filter(
                    (entry): entry is { run: ProvisioningRun; tenant: TenantSummary } =>
                        entry.tenant !== undefined,
                );
            /*
             * A failed run stays in the engine after a second attempt sets the
             * tenant up, so a failed run needs attention only while its tenant
             * is still bootstrapping.
             */
            const seen = new Set<string>();
            const failedEntries: { run: ProvisioningRun; tenant: TenantSummary }[] = [];
            for (const run of failed) {
                const tenant = byId.get(run.tenantId);
                if (
                    tenant === undefined ||
                    tenant.status !== 'bootstrapping' ||
                    seen.has(tenant.id)
                ) {
                    continue;
                }
                seen.add(tenant.id);
                failedEntries.push({ run, tenant });
            }
            /*
             * The finished count comes from each run's own steps, read once for
             * the runs that became an entry and no others.
             */
            const describedIds = [
                ...new Set(
                    [...activityEntries, ...failedEntries].map((entry) => entry.run.instanceId),
                ),
            ];
            const doneById = new Map(
                await Promise.all(
                    describedIds.map(
                        async (instanceId) =>
                            [instanceId, await readStepsDone(session.client, instanceId)] as const,
                    ),
                ),
            );
            activity = activityEntries.map(({ run, tenant }) => ({
                instanceId: run.instanceId,
                tenantName: tenant.name,
                status: run.status,
                stepsDone: doneById.get(run.instanceId) ?? 0,
                stepCount: run.stepCount,
                error: run.error,
                at: run.at,
            }));
            failedSetups = failedEntries.map(({ run, tenant }) => ({
                ...tenant,
                setup: {
                    instanceId: run.instanceId,
                    status: run.status,
                    stepsDone: doneById.get(run.instanceId) ?? 0,
                    stepCount: run.stepCount,
                    error: run.error,
                },
            }));
        } catch (error) {
            request.log.warn({ err: error }, 'The provisioning runs were not read.');
            activityUnavailable = true;
        }
        try {
            setups = (
                await readTenantSetups(
                    session.client,
                    first.tenants.map((tenant) => tenant.id),
                    (instanceId) => readStepsDone(session.client, instanceId),
                )
            ).setups;
        } catch (error) {
            request.log.warn({ err: error }, "The first tenants' setups were not read.");
        }

        /*
         * Setting up is the bootstrapping tenants less the ones whose setup
         * failed. When the runs cannot be read, no failure can be named, so
         * the figure counts the stalled setups too, and the page says the
         * activity is unavailable.
         */
        const stalled = failedSetups.filter((tenant) => tenant.status === 'bootstrapping').length;
        return deploymentOverviewSchema.parse({
            inService,
            onEvaluation,
            settingUp: Math.max(0, bootstrapping - stalled),
            attention: [
                ...failedSetups.map((tenant) => ({ tenant, reason: 'setup-failed' })),
                ...suspended.tenants.map((tenant) => ({ tenant, reason: 'suspended' })),
            ],
            tenants: first.tenants.map((tenant) => ({
                ...tenant,
                setup: setups.get(tenant.id) ?? null,
            })),
            totalCount: first.totalCount,
            activity,
            activityUnavailable,
        });
    });

    /** What the browser sends to remove a tenant: its code, typed again. */
    const removeTenantBodySchema = z.object({ confirmCode: z.string() });

    /**
     * Removes a tenant.
     *
     * The person types the tenant's code to confirm, and the route checks it
     * against the address, so a request that names one tenant cannot remove
     * another. The server closes the tenant's row, marks it terminated and
     * keeps its data; sign-in then refuses it, and token refresh refuses a
     * session already open in it. The system tenant holds the deployment
     * itself, so it is refused here with a reason, and the database refuses it
     * too.
     */
    server.delete('/api/tenants/:code', async (request, reply) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('A tenant of the deployment is removed in system administration.');
        }
        const { code } = request.params as { code: string };
        const body = removeTenantBodySchema.safeParse(request.body);
        if (!body.success || body.data.confirmCode !== code) {
            throw invalidRequest('Type the tenant code to confirm the removal.');
        }
        const tenant = await readTenant(session.client, code);
        if (tenant === null) {
            throw notFound('No tenant has this code.');
        }
        if (tenant.id === SYSTEM_TENANT_ID) {
            throw new HttpFailure(409, {
                code: 'conflict',
                message: 'The system tenant holds the deployment itself and cannot be removed.',
            });
        }
        const outcome = await removeTenant(session.client, code);
        if (!outcome.removed) {
            if (outcome.outcome === 'missing') {
                throw notFound('No tenant has this code.');
            }
            if (outcome.outcome === 'denied') {
                throw notPermitted(outcome.message || 'You may not remove tenants.');
            }
            throw new HttpFailure(409, {
                code: 'conflict',
                message: outcome.message || 'The tenant was not removed.',
            });
        }
        request.log.info({ code }, 'Tenant removed.');
        return reply.code(204).send();
    });

    /**
     * One tenant's screen: the tenant and the run that set it up.
     *
     * The tenant is read by its code, which is how the registry's get keys it.
     * The system tenant opens like any other. The run is read beside the
     * tenant, and a failure to read it is not a failure to read the tenant.
     * The tenant's own data is read from inside the tenant, not from here.
     */
    server.get('/api/tenants/:code', async (request) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('A tenant of the deployment is read in system administration.');
        }
        const { code } = request.params as { code: string };
        const tenant = await readTenant(session.client, code);
        if (tenant === null) {
            throw notFound('No tenant has this code.');
        }

        let setup: TenantSetup | null = null;
        let setupUnavailable = false;
        try {
            setup =
                (
                    await readTenantSetups(session.client, [tenant.id], (instanceId) =>
                        readStepsDone(session.client, instanceId),
                    )
                ).setups.get(tenant.id) ?? null;
        } catch (error) {
            request.log.warn({ err: error }, 'The provisioning runs were not read.');
            setupUnavailable = true;
        }

        return tenantDetailResponseSchema.parse({
            tenant: { ...tenant, setup },
            setupUnavailable,
        });
    });

    /**
     * Reads one tenant's data from system administration, behind the screen.
     *
     * The tenant is named by its code, and the read runs inside it for as long
     * as the read lasts and no longer: the person reads a tenant's parties and
     * people as they read its details, and never enters or leaves anything.
     * The session's own token is untouched, so any other request the browser
     * makes meanwhile is answered as system administration.
     */
    async function tenantToRead(request: FastifyRequest): Promise<{
        readonly session: LiveSession;
        readonly tenantId: string;
    }> {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('A tenant of the deployment is read in system administration.');
        }
        const { code } = request.params as { code: string };
        const tenant = await readTenant(session.client, code);
        if (tenant === null) {
            throw notFound('No tenant has this code.');
        }
        return { session, tenantId: tenant.id };
    }

    async function readInside<T>(
        request: FastifyRequest,
        read: (caller: AuthenticatedCaller, tenantId: string) => Promise<T>,
    ): Promise<T> {
        const { session, tenantId } = await tenantToRead(request);
        return readInsideKnownTenant(request, session, tenantId, read);
    }

    async function readInsideKnownTenant<T>(
        request: FastifyRequest,
        session: LiveSession,
        tenantId: string,
        read: (caller: AuthenticatedCaller, tenantId: string) => Promise<T>,
    ): Promise<T> {
        /*
         * The session's own tenant, the system tenant for a system
         * administrator, is read as the session: there is nothing to enter.
         */
        if (tenantId === session.tenantId) {
            return read(session.client, tenantId);
        }
        /*
         * Only a refused entry is a refusal. A read that fails inside the
         * tenant is carried out as itself, so the browser is not told it may
         * not look when the server simply failed to answer.
         */
        let failedRead: { readonly error: unknown } | undefined;
        try {
            return await session.client.readInsideTenant(
                tenantId,
                async (caller) => {
                    try {
                        return await read(caller, tenantId);
                    } catch (error) {
                        failedRead = { error };
                        throw error;
                    }
                },
                (error) =>
                    request.log.warn({ err: error }, 'The exit from the tenant was not recorded.'),
            );
        } catch (error) {
            if (failedRead === undefined && error instanceof OperationFailedError) {
                throw notPermitted(error.message);
            }
            throw error;
        }
    }

    /** One page of a tenant's parties, read inside it. */
    server.get('/api/tenants/:code/parties', async (request) => {
        const query = request.query as Record<string, string | undefined>;
        const page = partyPageQuerySchema.safeParse({
            search: query['search'] ?? '',
            sort: query['sort'] ?? '',
            descending: query['descending'] === 'true',
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 20),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return partyPageSchema.parse(
            await readInside(request, async (caller, tenantId) => {
                const parties = await readPartiesPage(caller, page.data);
                await keepPageImages(
                    request,
                    caller,
                    tenantId,
                    parties.parties.map((party) => party.flagImageId),
                );
                return parties;
            }),
        );
    });

    /** One page of a tenant's people, read inside it. */
    server.get('/api/tenants/:code/people', async (request) => {
        const query = request.query as Record<string, string | undefined>;
        const page = listAccountsRequestSchema.safeParse({
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 100),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return readInside(request, async (caller, tenantId) => {
            const people = await readAccountsPage(caller, page.data);
            await keepPageImages(
                request,
                caller,
                tenantId,
                people.accounts.map((account) => account.imageId),
            );
            return people;
        });
    });

    /** A page of an account's sessions, as the browser asks for it. */
    const signInsPageSchema = z.object({
        offset: z.int().nonnegative(),
        limit: z.int().positive().max(200),
    });

    function signInsPage(request: FastifyRequest): z.infer<typeof signInsPageSchema> {
        const query = request.query as Record<string, string | undefined>;
        const page = signInsPageSchema.safeParse({
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 20),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return page.data;
    }

    /**
     * One account of a tenant and its sign-ins, read inside the tenant: its
     * sign-in state and its sessions, newest first. The system tenant is the
     * system administrator's own, so its accounts, the platform's services
     * among them, are read as the session.
     */
    server.get('/api/tenants/:code/accounts/:username/sign-ins', async (request) => {
        const page = signInsPage(request);
        const { username } = request.params as { username: string };
        const answer = await readInside(request, async (caller, tenantId) => {
            const signIns = await readAccountSignIns(caller, username, page);
            if (signIns !== null) {
                await keepPageImages(request, caller, tenantId, [signIns.account.imageId]);
            }
            return signIns;
        });
        if (answer === null) {
            throw notFound('No account has this username.');
        }
        return accountSignInsSchema.parse(answer);
    });

    /**
     * One picture or flag of a tenant, as the tenant's pages named it.
     *
     * The page that names an image reads it in the same visit, so this answers
     * from what that visit kept. An image the cache has let go is read inside
     * the tenant again. Cached hard in the browser, because an image's
     * identifier is its identity and its bytes never change.
     */
    server.get('/api/tenants/:code/images/:id', async (request, reply) => {
        const { id } = request.params as { id: string };
        if (!isUuid(id)) {
            throw notFound('No image has this identifier.');
        }
        const { session, tenantId } = await tenantToRead(request);
        let image = tenantImages.get(tenantId, id);
        if (image === undefined) {
            [image] = await readInsideKnownTenant(request, session, tenantId, (caller) =>
                keepImages(caller, tenantId, [id]),
            );
        }
        if (image === undefined) {
            throw notFound('No image has this identifier.');
        }
        return sendImage(reply, image);
    });

    /**
     * Keeps the images a page names, without failing the page.
     *
     * A page without its pictures is still the page: an image the cache does
     * not hold is read again when the browser asks for it.
     */
    async function keepPageImages(
        request: FastifyRequest,
        caller: AuthenticatedCaller,
        tenantId: string,
        imageIds: readonly (string | null)[],
    ): Promise<void> {
        try {
            await keepImages(caller, tenantId, imageIds);
        } catch (error) {
            request.log.warn({ err: error }, 'The images a page names could not be read.');
        }
    }

    /**
     * Reads the images named that the cache does not hold yet, keeps them, and
     * answers what it read, so an image too large to keep is still served.
     */
    async function keepImages(
        caller: AuthenticatedCaller,
        tenantId: string,
        imageIds: readonly (string | null)[],
    ): Promise<ImageContent[]> {
        const missing = tenantImages.missing(
            tenantId,
            imageIds.filter((id): id is string => id !== null),
        );
        const images = await readImages(caller, missing);
        for (const image of images) {
            tenantImages.put(tenantId, image);
        }
        return images;
    }

    /**
     * One page of the parties of the session's own tenant.
     *
     * Row-level security scopes the read from the session's token, so the
     * route names no tenant. A system administrator's own tenant is the system
     * tenant, which the party policy lets read every tenant's parties, so the
     * route refuses system administration: a tenant's parties are read from
     * inside it.
     */
    server.get('/api/parties', async (request) => {
        const session = requireSession(request);
        if (session.mode === 'system-administration') {
            throw notPermitted("A tenant's parties are read from inside the tenant.");
        }
        const query = request.query as Record<string, string | undefined>;
        const page = partyPageQuerySchema.safeParse({
            search: query['search'] ?? '',
            sort: query['sort'] ?? '',
            descending: query['descending'] === 'true',
            offset: Number(query['offset'] ?? 0),
            limit: Number(query['limit'] ?? 20),
        });
        if (!page.success) {
            throw invalidRequest('The page offset and limit must be whole numbers.');
        }
        return partyPageSchema.parse(await readPartiesPage(session.client, page.data));
    });

    /**
     * The tenant lifecycle statuses, and the badge each one is painted with.
     *
     * The status row carries the badge's code, so this is a lookup and not a
     * mapping: nothing says which badge a status gets, because the status
     * already names it. The words in the answer are the status's own and the
     * colours are the badge's, which is what stops a deployment's vocabulary
     * from being replaced by a generic one.
     */
    server.get('/api/tenant-statuses', async (request) => {
        const session = requireSession(request);
        return tenantStatusesResponseSchema.parse({
            statuses: await readTenantStatuses(session.client),
        });
    });

    /**
     * The tenant types, and the badge each one is painted with.
     *
     * The roster filters by type and paints the type column, so it reads the
     * deployment's own type rows rather than a list written into the screen.
     */
    server.get('/api/tenant-types', async (request) => {
        const session = requireSession(request);
        return tenantTypesResponseSchema.parse({
            types: await readTenantTypes(session.client),
        });
    });

    /**
     * The starting points a new tenant may be provisioned from.
     *
     * The tenant journey opens on them, and they are data: the two that exist
     * today are seeded rows, and a third profile is a row rather than a screen
     * change. Each one travels with the step kinds it orders and the parameters
     * its form declares, because the card counts them and the form is built
     * from the schema.
     */
    server.get('/api/seed-profiles', async (request) => {
        const session = requireSession(request);
        return seedProfilesResponseSchema.parse({
            profiles: await session.client.seedProfiles(),
        });
    });

    /**
     * The root legal entities a tenant can be started from, matched against a
     * search.
     *
     * The list a screen searches when it builds a tenant around a real legal
     * entity. The deployment holds tens of thousands of them, so the matching
     * and the size of the answer are the read's: a screen sends what the person
     * typed rather than fetching a page and filtering it, because the entity
     * somebody is looking for is not on any one page. Each match states how
     * many parties its hierarchy would create, which is the work the choice
     * starts.
     */
    server.get('/api/lei-entities', async (request) => {
        const session = requireSession(request);
        const query = request.query as Record<string, string | undefined>;
        const response = searchLeiEntitiesResponseSchema.parse(
            await session.client.callAuthenticated(
                SUBJECTS.leiEntitiesSearch,
                {
                    search: query['search'] ?? '',
                    country_filter: query['country'] ?? '',
                    offset: 0,
                    limit: Number(query['limit'] ?? 20),
                },
                searchLeiEntitiesResponseSchema,
            ),
        );
        if (!response.success) {
            throw invalidRequest(response.error_message);
        }
        return {
            entities: response.entities.map((entity) => ({
                lei: entity.lei,
                legalName: entity.entity_legal_name,
                country: entity.country,
                partyCount: entity.party_count,
            })),
        };
    });

    /**
     * Adds one party to the tenant the caller works in, and starts the run that
     * brings it to life.
     *
     * Two writes in one act, in the order they depend on each other: the party
     * row, and the run that publishes the party's data, activates it, records
     * the legal entity it was built from and joins the caller to it. Nothing is
     * created until the person confirms, and the answer states the run rather
     * than waiting for it, because the stage takes minutes.
     *
     * Where the party sits is the deployment's answer and not the person's. A
     * party hangs under another and exactly one of a tenant's parties sits at
     * the top, so the row whose parent is unset and which is not the system
     * party is what a new party is placed under; a tenant that has none yet
     * gets the party it has just added as its root.
     */
    server.post('/api/provision-party', async (request) => {
        const session = requireSession(request);
        const parsed = provisionPartyRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest('The legal name and a short code are required.');
        }

        const parties = await session.client.listParties();
        const root = parties.find(
            (party) => party.parent_party_id === null && party.party_category !== 'System',
        );
        const created = await session.client.createParty({
            shortCode: parsed.data.shortCode,
            fullName: parsed.data.fullName,
            parentPartyId: root?.id ?? null,
        });
        if (!created.success) {
            return provisionPartyResultSchema.parse({
                success: false,
                message: created.message,
                instanceId: '',
                partyId: '',
            });
        }

        return provisionPartyResultSchema.parse(
            await session.client.provisionParty({
                party: created.partyId,
                /*
                 * No starting point: the profiles are the system tenant's rows
                 * and this caller reads only its own, so the service that holds
                 * both chooses the deployment's party stage.
                 */
                profileCode: '',
                /*
                 * The entity the person chose, which the run records against
                 * the party: a party identifier carries the party its writing
                 * session acts in, and this session works in another one.
                 */
                lei: parsed.data.lei,
            }),
        );
    });

    /**
     * Provisions a tenant from a starting point.
     *
     * The tenant and its administrator exist by the time this answers, and the
     * answer carries the id of the run that provisions the rest of the profile.
     * The browser follows that run by its id rather than waiting here, because
     * the steps it orders take minutes.
     */
    server.post('/api/provision-tenant', async (request) => {
        const session = requireSession(request);
        const parsed = provisionTenantRequestSchema.safeParse(request.body);
        if (!parsed.success) {
            throw invalidRequest(
                'A profile code, the tenant fields and the administrator fields are required.',
            );
        }
        return provisionTenantResultSchema.parse(await session.client.provisionTenant(parsed.data));
    });

    /**
     * A run's progress, as a journey's rail renders it.
     *
     * The page follows the run by asking this again while it is open: the
     * answer carries the run's status, the step it is executing and one
     * summary per step, so the rail is a rendering of the answer rather than a
     * state the page keeps. The engine's change event is not the progress
     * contract, which is why this read is what a journey follows.
     *
     * The route names the run and not the journey that started it: a tenant's
     * stages and a party's are the same record, and a route named for one of
     * them would say a party's run was a tenant's.
     */
    server.get('/api/workflow/:instanceId', async (request) => {
        const session = requireSession(request);
        const { instanceId } = request.params as { instanceId: string };
        return workflowProgressSchema.parse(await session.client.workflowProgress(instanceId));
    });

    /**
     * Resumes a stopped run from the step that failed.
     *
     * The body names the step only when a person resumes somewhere other than
     * where the run stopped. The engine decides, so the answer says which step
     * it re-dispatched or why it refused, and the page re-reads the progress
     * either way.
     */
    server.post('/api/workflow/:instanceId/retry', async (request) => {
        const session = requireSession(request);
        const { instanceId } = request.params as { instanceId: string };
        const body = z.object({ stepName: z.string().default('') }).parse(request.body ?? {});
        return retryWorkflowInstanceResultSchema.parse(
            await session.client.retryWorkflowInstance({
                workflowInstanceId: instanceId,
                stepName: body.stepName,
            }),
        );
    });

    registerClassificationRoutes(server, requireSession);
    registerRecordRoutes(server, requireSession);
    registerInboxRoutes(server, requireSession);
    registerAuditRoutes(server, requireSession);
    registerTimelineRoutes(server, requireSession);
    registerOperationsRoutes(
        server,
        requireSession,
        resolveBroker(site.configuration, site.environment).subjectPrefix,
    );

    /**
     * The reasons a write may carry.
     *
     * Fetched from the server rather than declared in the interface, because the
     * set is data: it differs per deployment, and one reason means "changed
     * nothing material" while the rest mean the opposite.
     */
    server.get('/api/change-reasons', async (request) => {
        const session = requireSession(request);
        const query = request.query as Record<string, string | undefined>;
        /*
         * The request is the generated type, so every field the server's
         * decoder requires is present or the typecheck fails; it refused this
         * request for want of an order.
         */
        const listRequest: ListChangeReasonsRequest = {
            offset: query['offset'] === undefined ? 0 : Number(query['offset']),
            limit: query['limit'] === undefined ? 200 : Number(query['limit']),
            order: { field: '', descending: false },
            filter: null,
            as_of: null,
        };
        const response = await session.client.callAuthenticated(
            SUBJECTS.listChangeReasons,
            listRequest,
            changeReasonPageSchema,
        );
        return {
            reasons: response.reasons.map((reason) => ({
                code: reason.code,
                description: reason.description,
                categoryCode: reason.category_code,
                appliesToNew: reason.applies_to_new,
                appliesToAmend: reason.applies_to_amend,
                appliesToDelete: reason.applies_to_delete,
                requiresCommentary: reason.requires_commentary,
                displayOrder: reason.display_order,
            })),
        };
    });

    /**
     * How many bytes an upload may carry.
     *
     * Fastify reads one megabyte of body by default, and an image arrives
     * base64-encoded, which is about a third larger than the bytes it carries.
     * Four megabytes hold the largest image the validator accepts with room
     * to spare.
     */
    const IMAGE_UPLOAD_BODY_BYTES = 4 * 1024 * 1024;

    /** What the browser sends to upload: the media type and the base64 bytes. */
    const uploadBodySchema = z.object({
        mimeType: z.string().min(1).max(100),
        data: z.string().min(1),
    });

    /**
     * Uploads one image and answers its identifier.
     *
     * The upload sets no photo: the id it answers rides into the write that
     * references it, so an upload that nobody saves changes nothing. A refused
     * image is an answer rather than a failed call, and the picker states
     * which rule the image broke.
     */
    server.post('/api/images', { bodyLimit: IMAGE_UPLOAD_BODY_BYTES }, async (request) => {
        const session = requireSession(request);
        const body = uploadBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('An upload needs a media type and the image bytes.');
        }
        return imageUploadViewSchema.parse(await uploadImage(session.client, body.data));
    });

    /**
     * The rule an uploaded image must satisfy.
     *
     * Read from the validator that enforces it, so the picker states the rule
     * the server applies rather than a copy of it.
     */
    server.get('/api/image-upload-policy', async (request) => {
        const session = requireSession(request);
        return imageUploadPolicyViewSchema.parse(await readImageUploadPolicy(session.client));
    });

    /**
     * Which image every flagged record uses: currencies, countries, calendars
     * and business centres, and the placeholder for a code with none. The one
     * read a screen needs to draw any flag; the browser keeps it for a few
     * minutes and fetches each image once from its own address.
     */
    server.get('/api/image-map', async (request, reply) => {
        const session = requireSession(request);
        const map = await readImageMap(session.client);
        return reply.header('cache-control', 'private, max-age=300').send(map);
    });

    /**
     * One page of the session's images, for a chooser: identifier, code and
     * words, searched on the server by code and description. The bytes stay
     * behind; each image is drawn from its own address.
     */
    server.get('/api/images', async (request) => {
        const session = requireSession(request);
        const query = imagePageQuerySchema.safeParse(request.query);
        if (!query.success) {
            throw invalidRequest(
                'A page of images names an offset, a limit of 1 to 500, and a search.',
            );
        }
        return await listImageSummaries(session.client, query.data);
    });

    /**
     * One image of the session's own tenant, by its identifier.
     *
     * Flags and pictures live in the assets service as ordinary images, so this
     * is what a flag cell points at. Fetched through the BFF because the
     * browser never reaches NATS, which is the same reason every other read
     * goes through here. Kept in the tenant image cache and cached hard by the
     * browser, because an image's identifier is its identity and its bytes
     * never change, so a flag shown on every row is read from the service once.
     */
    server.get('/api/images/:id', async (request, reply) => {
        const session = requireSession(request);
        const { id } = request.params as { id: string };
        if (!isUuid(id)) {
            throw notFound('No image has this identifier.');
        }
        const cached = tenantImages.get(session.tenantId, id);
        if (cached !== undefined) {
            return sendImage(reply, cached);
        }
        const [image] = await readImages(session.client, [id]);
        if (image === undefined) {
            throw notFound('No image has this identifier.');
        }
        tenantImages.put(session.tenantId, image);
        return sendImage(reply, image);
    });

    /*
     * The cache is minutes, not a year. An image keeps its identifier when a
     * seed or a migration rewrites its bytes, so a long immutable cache would
     * hide the new picture until the browser's site data was cleared.
     */
    function sendImage(reply: FastifyReply, image: ImageContent): FastifyReply {
        return reply
            .header('content-type', image.mimeType)
            .header('cache-control', 'private, max-age=300')
            .header('x-content-type-options', 'nosniff')
            .header('content-security-policy', "default-src 'none'; sandbox")
            .send(image.bytes);
    }

    /*
     * One stream per session, carrying everything the interface hears about.
     *
     * One rather than one per screen, because a screen opening and closing must not
     * churn connections and a person with six lists open wants one stream. The
     * kinds already defined for it — the session ending, the party changing — are
     * the same channel's business, because they are the same question asked by
     * different parts of the interface.
     */
    /*
     * A connection of its own for listening.
     *
     * Not a session's, because a subscription is shared and would die with
     * whichever session happened to open it first, and not authenticated, because
     * these are published events rather than replies: there is no session to
     * present. Opened once for the process, beside the per-session clients rather
     * than among them.
     */
    const eventClient = createClient();
    void eventClient.connect().catch(() => undefined);
    const events = new ChangeEventRegistry(eventClient.client);

    server.get('/api/events', async (request, reply) => {
        const session = requireSession(request);

        /*
         * The stream is written by hand rather than through a plugin.
         *
         * It is four headers and a formatted line, and the format is the contract;
         * taking a dependency to produce it would be a dependency to keep in step
         * with a format that does not change.
         */
        reply.hijack();
        reply.raw.writeHead(200, {
            'content-type': 'text/event-stream',
            'cache-control': 'no-cache, no-transform',
            connection: 'keep-alive',
            // Proxies buffer by default, which turns a stream into a delivery at the
            // end of the response.
            'x-accel-buffering': 'no',
        });

        const send = (event: string, data: unknown): void => {
            reply.raw.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`);
        };

        send('connected', { at: new Date().toISOString() });
        events.attach(session.id, (change) => send('entity-changed', change));

        // A person who navigates away, closes the tab or loses the network is a
        // watcher who has gone, and the subscriptions they were holding have to go
        // with them or the registry grows for the life of the process.
        request.raw.on('close', () => {
            events.forget(session.id);
        });
    });

    /**
     * Declares what a session is watching.
     *
     * Sent when the screen changes rather than carried on the stream, because a
     * stream is one-way and reconnecting to change what is watched would be churn
     * for something that changes on every navigation.
     */
    server.post('/api/events/watch', async (request) => {
        const session = requireSession(request);
        const body = z
            .object({
                watches: z
                    .array(z.object({ component: z.string(), entity: z.string() }))
                    .max(50)
                    .default([]),
            })
            .parse(request.body);
        events.watch(session.id, session.tenantId, body.watches as readonly Watch[]);
        return { ok: true };
    });

    server.addHook('onClose', async () => {
        await sessions.destroyAll();
    });

    /*
     * The built interface is served by this process, so one port carries the API
     * and the bundle. Vite is a development tool only. The directory is resolved
     * from this module rather than the working directory, so the server may be
     * started from anywhere.
     */
    const browserDirectory = browserBundleDirectory();
    const browserBundle = existsSync(browserDirectory) ? browserDirectory : undefined;
    if (browserBundle !== undefined) {
        void server.register(fastifyStatic, { root: browserBundle });
        server.log.info({ directory: browserBundle }, 'serving the browser bundle');
    } else {
        server.log.info(
            { directory: browserDirectory },
            'no browser bundle built, serving the API only',
        );
    }

    // A screen's own URL is not a file, so anything the API does not own is
    // answered with the bundle's entry point and the router takes it from there.
    server.setNotFoundHandler(async (request, reply) => {
        if (
            browserBundle !== undefined &&
            (request.method === 'GET' || request.method === 'HEAD') &&
            !isApiPath(request.url)
        ) {
            return reply.sendFile('index.html');
        }
        return reply.status(404).send({
            code: 'not-found',
            message: `Route ${request.method}:${request.url} not found`,
        });
    });

    return server;
}

/**
 * Reads certificate material.
 *
 * A value naming an existing file is read from disk; anything else is treated
 * as inline PEM, so a deployment can supply either.
 */
function readPem(value: string, label: string): string {
    if (value.includes('-----BEGIN')) {
        return value;
    }
    try {
        return readFileSync(value, 'utf8');
    } catch (cause) {
        throw new Error(`Cannot read ${label} at ${value}`, { cause });
    }
}

/**
 * Where the browser bundle is built.
 *
 * Resolved from this module, which sits one level below the package in both the
 * source tree and the build output, so the same relative path works either way.
 */
export function browserBundleDirectory(): string {
    return resolve(dirname(fileURLToPath(import.meta.url)), '..', '..', 'web', 'dist');
}

/** Whether a request belongs to the API rather than the interface's own routes. */
function isApiPath(url: string): boolean {
    const path = url.split('?')[0] ?? '';
    return path === '/api' || path.startsWith('/api/');
}
