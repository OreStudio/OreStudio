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

/**
 * End-to-end verification of the First run journey against the running system.
 *
 * It walks the journey the way the browser walks it: the same client
 * (`packages/web/src/api/client.ts`), the same request-building
 * (`journeys/state.ts`), the same password rules (`ui/passwordPolicy.ts`), in
 * the same order, against the real BFF and the real services behind it. The two
 * things a browser provides and Node does not -- a base address for the
 * relative paths, and a jar for the session cookie -- are the only parts
 * supplied here, so an answer the browser could not parse fails this run.
 *
 * The first assertion is that the deployment still needs its administrator,
 * because the first run is the one path that can only happen on an empty
 * installation. A deployment that is already set up is refused rather than
 * half-walked: the journey is not re-runnable, so there is nothing to prove on
 * one.
 *
 * Prerequisites:
 *   compass services stop
 *   compass db recreate -y -k       (wipes this environment's data)
 *   compass services start
 *   npm run build                   (the browser parses with the built packages)
 *
 * Run:
 *   npx tsx scripts/verify-first-run.ts
 *
 * Exit code 0 means every assertion held.
 */

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
    administratorPassword,
    detailsFor,
    provisionRequest,
    tenantPrincipal,
    type TenantDetails,
} from '../packages/web/src/journeys/state.js';
import { assessPassword } from '../packages/web/src/ui/passwordPolicy.js';
import { api } from '../packages/web/src/api/client.js';
import {
    SYSTEM_TENANT_ID,
    isUuid,
    type PasswordPolicy,
    type SeedProfileChoice,
    type SessionView,
    type WorkflowProgress,
} from '@ores/wire-protocol/browser';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..', '..');

// The checkout's environment file is the authority. A variable already in the
// process environment wins, as it does for the BFF started with --env-file.
try {
    process.loadEnvFile(resolve(REPO_ROOT, '.env'));
} catch {
    // A checkout without one falls back to the defaults below.
}

const BASE_URL =
    setting('ORES_WEB_URL') ?? `http://127.0.0.1:${setting('ORES_WEB_PORT') ?? '8080'}`;

const PROFILE_CODE = setting('ORES_FIRST_RUN_PROFILE') ?? 'empty_operational';

const ADMIN_PRINCIPAL = 'super_admin';
const ADMIN_EMAIL = 'super_admin@system.ores';
const ADMIN_PASSWORD = 'Super-Admin-Pass-1!';

const TENANT_CODE = 'northwind';
const TENANT_NAME = 'Northwind Capital';
const TENANT_HOSTNAME = 'northwind.example.com';
const TENANT_ADMIN = 'northwind_admin';
const TENANT_ADMIN_EMAIL = 'admin@northwind.example.com';
const TENANT_ADMIN_PASSWORD = 'Tenant-Admin-Pass-2!';
const TENANT_ADMIN_NEW_PASSWORD = 'Chosen-Tenant-Pass-3!';

/** How often an open run is asked about again, as the rail's own interval does. */
const POLL_INTERVAL_MS = 1500;

/** How long a provisioning run may take before the walk gives up on it. */
const RUN_TIMEOUT_MS = 20 * 60 * 1000;

/** The run states a person has to wait through, as the rail states them. */
const OPEN_RUN = new Set(['', 'pending', 'in_progress', 'compensating']);

/** The step states a finished run may rest in. */
const SETTLED_STEP = new Set(['completed', 'completed_with_warnings']);

let failures = 0;

function check(label: string, condition: boolean, detail = ''): boolean {
    const status = condition ? 'PASS' : 'FAIL';
    if (!condition) {
        failures += 1;
    }
    console.log(`  [${status}] ${label}${detail.length > 0 ? ` (${detail})` : ''}`);
    return condition;
}

function section(title: string): void {
    console.log(`\n${title}:`);
}

/** Reads a variable, treating whitespace as absent. */
function setting(name: string): string | undefined {
    const value = process.env[name];
    return value !== undefined && value.trim().length > 0 ? value : undefined;
}

/**
 * The browser's transport, in Node.
 *
 * The client sends relative paths and expects the session cookie to ride along.
 * That is the browser's job in a browser, so it is this script's job here:
 * resolve the path against the deployment, keep what the deployment set, and
 * send it back. Nothing else about a request is touched.
 */
function installBrowserFetch(): void {
    const nativeFetch = globalThis.fetch;
    const cookies = new Map<string, string>();

    globalThis.fetch = async (input, init) => {
        const target = typeof input === 'string' ? new URL(input, BASE_URL) : input;
        const headers = new Headers(init?.headers);
        if (cookies.size > 0) {
            const jar = [...cookies].map(([name, value]) => `${name}=${value}`).join('; ');
            headers.set('cookie', jar);
        }
        const response = await nativeFetch(target, { ...init, headers });
        for (const cookie of response.headers.getSetCookie()) {
            const [pair = ''] = cookie.split(';');
            const separator = pair.indexOf('=');
            if (separator > 0) {
                cookies.set(pair.slice(0, separator).trim(), pair.slice(separator + 1));
            }
        }
        return response;
    };
}

function sleep(ms: number): Promise<void> {
    return new Promise((done) => setTimeout(done, ms));
}

/** The starting point the walk provisions from, which it must find. */
function profileOf(profiles: readonly SeedProfileChoice[]): SeedProfileChoice {
    const found = profiles.find((profile) => profile.code === PROFILE_CODE);
    if (found === undefined) {
        throw new Error(`the deployment holds no starting point '${PROFILE_CODE}'`);
    }
    return found;
}

/** Signs in and resolves the party choice a login might still be waiting on. */
async function enterAs(principal: string, password: string): Promise<SessionView> {
    const outcome = await api.login({ username: principal, password });
    if (outcome.outcome === 'active') {
        return outcome.session;
    }
    const chosen =
        outcome.availableParties.find((party) => party.id === outcome.defaultPartyId) ??
        outcome.availableParties[0];
    if (chosen === undefined) {
        throw new Error(`the deployment offered ${principal} no party to work in`);
    }
    return api.selectParty(chosen.id);
}

/** Ends the open session, which the hand-over does between the two people. */
async function signOut(): Promise<void> {
    await api.logout();
    check('the session ended', (await api.session()) === null);
}

function describeRun(progress: WorkflowProgress): string {
    const current = progress.steps[progress.current_step_index];
    const name = current === undefined ? '' : ` ${current.name}`;
    return `status=${progress.status} current=${progress.current_step_index}${name}`;
}

function printRail(progress: WorkflowProgress): void {
    for (const step of progress.steps) {
        const error = step.error === '' ? '' : ` -- ${step.error}`;
        console.log(`  ${step.name.padEnd(22)} ${step.status}${error}`);
    }
}

/** Follows a run until it rests, the way the rail's own interval does. */
async function followRun(instanceId: string): Promise<WorkflowProgress> {
    const deadline = Date.now() + RUN_TIMEOUT_MS;
    let progress = await api.provisionTenantProgress(instanceId);
    let seen = describeRun(progress);
    console.log(`  ${seen}`);
    while (OPEN_RUN.has(progress.status)) {
        if (Date.now() > deadline) {
            throw new Error(`the run did not rest within ${RUN_TIMEOUT_MS / 60000} minutes`);
        }
        await sleep(POLL_INTERVAL_MS);
        progress = await api.provisionTenantProgress(instanceId);
        const state = describeRun(progress);
        if (state !== seen) {
            seen = state;
            console.log(`  ${seen}`);
        }
    }
    return progress;
}

/** Asserts the deployment still needs its first administrator. */
async function verifyEmptyInstallation(): Promise<void> {
    section('1. the deployment is empty');
    const status = await api.bootstrapStatus();
    check('the deployment still needs its first administrator', status.isInBootstrapMode);
    check('it has no tenant of its own', !status.hasTenant);
    if (!status.isInBootstrapMode) {
        throw new Error(
            'this deployment is already set up, so the first run cannot be walked. ' +
                'Recreate its database first: compass db recreate -y -k',
        );
    }
}

/** Asserts the rules the screen states are the rules the server enforces. */
async function verifyPasswordPolicy(): Promise<PasswordPolicy> {
    section('2. the rules a password must satisfy, read before anybody signed in');
    const policy = await api.passwordPolicy();
    check('the policy states a minimum length', policy.minLength > 0, String(policy.minLength));
    check(
        'the creating administrator\u2019s password meets the policy',
        assessPassword(ADMIN_PASSWORD, policy).valid,
    );
    return policy;
}

/** Creates the system administrator, the one write the browser sends with no session. */
async function createAdministrator(): Promise<void> {
    section('3. create the administrator');
    const created = await api.createAdministrator({
        principal: ADMIN_PRINCIPAL,
        password: ADMIN_PASSWORD,
        email: ADMIN_EMAIL,
    });
    check('the deployment answered with the account it created', isUuid(created.accountId));
    check(
        'the account belongs to the system tenant',
        created.tenantId === SYSTEM_TENANT_ID,
        created.tenantId,
    );

    section('4. the deployment is no longer waiting for one');
    const status = await api.bootstrapStatus();
    check('bootstrap mode closed when the administrator was created', !status.isInBootstrapMode);
    check('the deployment still has no tenant of its own', !status.hasTenant);
}

/** Signs in as the administrator the journey created. */
async function enterAsAdministrator(): Promise<SessionView> {
    section('5. sign in as the administrator the journey created');
    const session = await enterAs(ADMIN_PRINCIPAL, ADMIN_PASSWORD);
    check('the session names the account that was created', session.username === ADMIN_PRINCIPAL);
    check('the session works in a party', session.party.name.length > 0, session.party.name);
    check(
        'the session belongs to the system tenant',
        session.tenantId === SYSTEM_TENANT_ID,
        session.tenantName,
    );
    return session;
}

/** Reads the starting points and describes the tenant from one of them. */
async function chooseStartingPoint(policy: PasswordPolicy): Promise<{
    readonly profile: SeedProfileChoice;
    readonly details: TenantDetails;
}> {
    section('6. the starting points the deployment holds');
    const profiles = await api.seedProfiles();
    check('at least one starting point came back', profiles.length > 0, String(profiles.length));
    for (const profile of profiles) {
        console.log(
            `  ${profile.code} (${profile.name}): ${profile.steps.length} step(s), ` +
                `${profile.parameters.length} parameter(s)`,
        );
    }
    const profile = profileOf(profiles);
    check('the walked starting point orders steps', profile.steps.length > 0);
    check(
        'every required parameter comes with the value the form proposes',
        profile.parameters.every(
            (parameter) => !parameter.required || parameter.defaultValue !== '',
        ),
    );

    section('7. describe the tenant');
    // What the person types, on top of what the starting point proposes.
    const details: TenantDetails = {
        ...detailsFor(profile),
        name: TENANT_NAME,
        code: TENANT_CODE,
        hostname: TENANT_HOSTNAME,
        adminUsername: TENANT_ADMIN,
        adminEmail: TENANT_ADMIN_EMAIL,
        adminPassword: TENANT_ADMIN_PASSWORD,
    };
    check(
        'the tenant administrator\u2019s password meets the policy',
        assessPassword(administratorPassword(details, ADMIN_PASSWORD), policy).valid,
    );
    return { profile, details };
}

/** Starts the run and follows it to its end. */
async function provisionTenant(
    profile: SeedProfileChoice,
    details: TenantDetails,
): Promise<WorkflowProgress> {
    section('8. review, and create the first tenant');
    const request = provisionRequest(profile, details, ADMIN_PASSWORD);
    check(
        'the request names the starting point that was chosen',
        request.profileCode === PROFILE_CODE,
    );
    const provisioned = await api.provisionTenant(request);
    check('the deployment accepted the request', provisioned.success, provisioned.message);
    check('the run it started has an id', isUuid(provisioned.instanceId), provisioned.instanceId);
    check('the tenant exists', isUuid(provisioned.tenantId));
    check('its administrator exists', isUuid(provisioned.accountId));

    section('9. follow the run until it rests');
    const progress = await followRun(provisioned.instanceId);
    printRail(progress);
    check('the run completed', progress.status === 'completed', progress.status);
    check('it materialised every step it counted', progress.steps.length === progress.step_count);
    check(
        'no step failed',
        progress.steps.every((step) => SETTLED_STEP.has(step.status)),
        progress.steps
            .filter((step) => !SETTLED_STEP.has(step.status))
            .map((step) => `${step.name}=${step.status}`)
            .join(', '),
    );
    const ordered = [...profile.steps].sort((left, right) => left.order - right.order);
    check(
        'the run executed the starting point\u2019s own steps, in the order it states',
        ordered.every((step, index) => progress.steps[index]?.name === step.kind),
    );
    if (progress.status !== 'completed') {
        throw new Error(`the run rested in ${progress.status}: ${progress.error}`);
    }
    return progress;
}

/** Asserts the deployment now holds a tenant of its own, which is *Ready*. */
async function verifyDeploymentIsSetUp(): Promise<void> {
    section('10. the deployment is set up');
    const status = await api.bootstrapStatus();
    check('the deployment has a tenant of its own', status.hasTenant);
    check('the setup screen is no longer the only page', !status.isInBootstrapMode);
}

/** Hands over: the creating administrator out, the tenant administrator in. */
async function handOver(profile: SeedProfileChoice, details: TenantDetails): Promise<SessionView> {
    section('11. hand off to the tenant administrator');
    await signOut();

    const principal = tenantPrincipal(details);
    const password = administratorPassword(details, ADMIN_PASSWORD);
    const outcome = await api.login({ username: principal, password });
    const resetRequired =
        outcome.outcome === 'active'
            ? outcome.session.passwordResetRequired
            : outcome.passwordResetRequired;
    check(
        'the forced change the answer reports is the starting point\u2019s own choice',
        resetRequired === profile.forcePasswordChange,
        `answer=${String(resetRequired)} starting point=${String(profile.forcePasswordChange)}`,
    );

    if (outcome.outcome === 'active') {
        // The session names the account, not the principal that routes to it:
        // the hostname part of `principal` is the tenant's, and the account is
        // stored under its username.
        check('the tenant administrator signed in', outcome.session.username === TENANT_ADMIN);
        check(
            'the session works in a party',
            outcome.session.party.name.length > 0,
            outcome.session.party.name,
        );
        return outcome.session;
    }

    check(
        'the sign-in offered the parties the account works in',
        outcome.availableParties.length > 0,
        String(outcome.availableParties.length),
    );
    const chosen =
        outcome.availableParties.find((party) => party.id === outcome.defaultPartyId) ??
        outcome.availableParties[0];
    if (chosen === undefined) {
        throw new Error('the deployment offered the tenant administrator no party');
    }
    const opened = await api.selectParty(chosen.id);
    check('the chosen party is the party the session works in', opened.party.id === chosen.id);
    return opened;
}

/** The tenant administrator's first use of their account. */
async function firstSignIn(
    details: TenantDetails,
    policy: PasswordPolicy,
    opened: SessionView,
): Promise<SessionView> {
    section('12. the tenant administrator\u2019s first sign-in');
    let session = (await api.session()) ?? opened;
    check('the session names the tenant the run created', session.tenantName === TENANT_NAME);
    check('the session works in a party', session.party.name.length > 0, session.party.name);
    check(
        'the session belongs to the tenant the run created',
        session.tenantId !== SYSTEM_TENANT_ID,
    );

    if (!session.passwordResetRequired) {
        check('no password change is outstanding', true);
        return session;
    }

    section('13. the starting point asks for a password of the tenant administrator\u2019s own');
    check(
        'the new password meets the policy',
        assessPassword(TENANT_ADMIN_NEW_PASSWORD, policy).valid,
    );
    await api.changePassword(
        administratorPassword(details, ADMIN_PASSWORD),
        TENANT_ADMIN_NEW_PASSWORD,
    );
    session = (await api.session()) ?? session;
    check('the change is no longer outstanding', !session.passwordResetRequired);

    await signOut();
    session = await enterAs(tenantPrincipal(details), TENANT_ADMIN_NEW_PASSWORD);
    check('the new password is the one that signs in', session.username === TENANT_ADMIN);
    return session;
}

async function main(): Promise<number> {
    installBrowserFetch();
    console.log(`deployment: ${BASE_URL}`);
    console.log(`starting point: ${PROFILE_CODE}`);

    await verifyEmptyInstallation();
    const policy = await verifyPasswordPolicy();
    await createAdministrator();
    await enterAsAdministrator();
    const { profile, details } = await chooseStartingPoint(policy);
    await provisionTenant(profile, details);
    await verifyDeploymentIsSetUp();
    const opened = await handOver(profile, details);
    const session = await firstSignIn(details, policy, opened);

    section('Ready');
    console.log(`  signed in as ${session.username} (${session.email})`);
    console.log(`  tenant: ${session.tenantName} (${session.tenantId})`);
    console.log(`  party: ${session.party.name} (${session.party.partyCategory})`);

    await signOut();
    console.log(`\n${failures === 0 ? 'ALL CHECKS PASSED' : `${failures} CHECK(S) FAILED`}`);
    return failures === 0 ? 0 : 1;
}

main().then(
    (code) => process.exit(code),
    (error: unknown) => {
        console.error(
            `\nverification aborted: ${error instanceof Error ? error.message : String(error)}`,
        );
        process.exit(1);
    },
);
