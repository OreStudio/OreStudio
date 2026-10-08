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

import { describe, expect, it, vi } from 'vitest';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';
import { firstRunSteps, type FirstRunChoice } from './firstRunSteps.js';
import { canGoBack, indexOfStep } from './runtime.js';
import { detailsFor, type NewTenant } from './state.js';
import type { TenantEntry } from './FirstSignIn.js';
import type { JourneyServer } from './server.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

const t = createTranslator('en', enFlat, enFlat).t;

const policy: PasswordPolicy = {
    success: true,
    message: '',
    minLength: 12,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    specialChars: '!@#$%^&*()_+-=[]{}|;:,.<>?',
};

const profile: SeedProfileChoice = {
    code: 'empty_operational',
    name: 'Empty operational',
    summary: '',
    audience: 'Production',
    bullets: [],
    tenant: {
        name: 'Northwind Capital',
        code: 'northwind',
        hostname: 'northwind.example.com',
        adminUsername: 'northwind_admin',
        adminEmail: 'admin@northwind.example.com',
    },
    inheritsAdminPassword: false,
    forcePasswordChange: true,
    order: 1,
    steps: [{ kind: 'provision_party', order: 1 }],
    parameters: [],
};

function fakeServer(): JourneyServer {
    return {
        createAdministrator: vi.fn(async () => undefined),
        recheckBootstrap: vi.fn(async () => undefined),
        completeSystemOnboarding: vi.fn(async () => undefined),
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: vi.fn(async () => undefined),
        signOut: vi.fn(async () => undefined),
        passwordPolicy: vi.fn(async () => policy),
        seedProfiles: vi.fn(async () => [profile]),
        tenantCodes: vi.fn(async () => []),
        provision: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
            tenantId: '',
            accountId: '',
        })),
        progress: vi.fn(async () => ({
            success: true,
            message: '',
            status: 'completed',
            error: '',
            step_count: 1,
            steps: [],
        })),
        retry: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            stepIndex: 0,
            stepName: '',
        })),
        changePassword: vi.fn(async () => undefined),
    };
}

/** A state as the page holds it, built without a renderer. */
function tenantState(overrides: Partial<NewTenant> = {}): NewTenant {
    return {
        profile: undefined,
        details: undefined,
        passwordAcceptable: false,
        instanceId: undefined,
        runComplete: false,
        chooseProfile: vi.fn(),
        describe: vi.fn(),
        acceptPassword: vi.fn(),
        recordRun: vi.fn(),
        recordRunComplete: vi.fn(),
        ...overrides,
    };
}

function steps(
    overrides: {
        readonly tenant?: NewTenant;
        readonly acceptable?: boolean;
        readonly creatingPassword?: string;
        /** Whether the run has reached its end. */
        readonly runComplete?: boolean;
        /** Whether the deployment already has its administrator. */
        readonly administratorExists?: boolean;
        /** What the installation is left with. */
        readonly choice?: FirstRunChoice;
        /** Whether the tenant administrator's sign-in has finished. */
        readonly tenantSignInComplete?: boolean;
        readonly entry?: TenantEntry;
        readonly onCreateAdministrator?: () => Promise<void>;
        readonly onAdministratorEntered?: () => Promise<void>;
        readonly onAdministratorSignIn?: () => Promise<void>;
        readonly onCompleteSystemOnboarding?: () => Promise<void>;
        readonly onFinished?: () => void;
        readonly onSignOutAfterBootstrap?: () => Promise<void>;
        readonly goTo?: (id: string) => void;
    } = {},
) {
    return firstRunSteps({
        t,
        server: fakeServer(),
        policy,
        profiles: [profile],
        tenant: {
            ...(overrides.tenant ?? tenantState()),
            runComplete: overrides.runComplete ?? overrides.tenant?.runComplete ?? false,
        },
        administrator: {
            principal: 'super_admin',
            email: 'super_admin@system.ores',
            password: 'Issued-Password-1',
            acceptable: overrides.acceptable ?? true,
        },
        creatingPassword: overrides.creatingPassword ?? 'Issued-Password-1',
        entry: overrides.entry,
        tenantSignInComplete: overrides.tenantSignInComplete ?? false,
        welcome: 'Welcome body',
        administratorForm: 'Administrator body',
        administratorExists: overrides.administratorExists ?? false,
        administratorSignIn: 'Administrator sign-in body',
        administratorArrival: 'Administrator arrival body',
        choice: overrides.choice ?? 'first-tenant',
        goTo: overrides.goTo ?? vi.fn(),
        onCreateAdministrator: overrides.onCreateAdministrator ?? vi.fn(async () => undefined),
        onAdministratorEntered: overrides.onAdministratorEntered ?? vi.fn(async () => undefined),
        onAdministratorSignIn: overrides.onAdministratorSignIn ?? vi.fn(async () => undefined),
        onHandOff: vi.fn(async () => undefined),
        onCompleteSystemOnboarding:
            overrides.onCompleteSystemOnboarding ?? vi.fn(async () => undefined),
        onFinished: overrides.onFinished ?? vi.fn(),
        onSignOutAfterBootstrap:
            overrides.onSignOutAfterBootstrap ?? vi.fn(async () => undefined),
    });
}

describe('the first run rail', () => {
    it('reads as the journey the person is taking, in one flat list', () => {
        expect(steps().map((step) => step.id)).toEqual([
            'welcome',
            'administrator',
            'profile',
            'details',
            'review',
            'provisioning',
            'handOff',
            'signIn',
            'ready',
        ]);
    });

    it('names each step in the person\u2019s terms', () => {
        expect(steps().map((step) => step.title)).toEqual([
            'Welcome to ORE Studio',
            'Create the administrator',
            'Choose a starting point',
            'Describe the tenant',
            'Review',
            'Provisioning',
            'Hand off',
            'First sign-in',
            'Ready',
        ]);
    });

    it('allows no way back past a step that writes', () => {
        const final = steps()
            .filter((step) => step.final === true)
            .map((step) => step.id);

        expect(final).toEqual(['provisioning', 'handOff', 'signIn']);
    });

    it('opens on the welcome, which asks the server nothing', () => {
        const [welcome] = steps();

        expect(welcome?.body).toBe('Welcome body');
        expect(welcome?.next).toEqual({ label: 'Get started', enabled: true });
    });

    it('will not create an administrator whose password breaks the policy', () => {
        const [, administrator] = steps({ acceptable: false });

        expect(administrator?.next?.enabled).toBe(false);
    });

    it('will not describe a tenant before a starting point is chosen', () => {
        const [, , , details] = steps();

        expect(details?.next?.enabled).toBe(false);
    });

    it('lets the tenant be described once the password may be sent', () => {
        const tenant = tenantState({
            profile,
            details: { ...detailsFor(profile), adminPassword: 'Abcdefgh123!' },
            passwordAcceptable: true,
        });
        const [, , profileStep, details] = steps({ tenant });

        expect(profileStep?.next?.enabled).toBe(true);
        expect(details?.next?.enabled).toBe(true);
    });

    it('offers no way past the run until the run has finished', () => {
        const running = steps();
        const finished = steps({ runComplete: true });
        const provisioning = (all: ReturnType<typeof steps>) =>
            all.find((step) => step.id === 'provisioning');

        expect(provisioning(running)?.next?.enabled).toBe(false);
        expect(provisioning(finished)?.next?.enabled).toBe(true);
    });

    it('runs the shared tenant steps in the order the library declares them', () => {
        const shared = newTenantStepsIds();

        expect(
            steps()
                .map((step) => step.id)
                .filter((id) => shared.includes(id)),
        ).toEqual(shared);
    });
});

/** The five steps both tenant journeys run, which this rail carries inline. */
function newTenantStepsIds(): readonly string[] {
    return ['profile', 'details', 'review', 'provisioning', 'handOff'];
}

describe('a step with no state behind it', () => {
    it('renders nothing rather than inventing a form nobody filled in', () => {
        const [, , , details] = steps();

        expect(details?.body).toBeNull();
    });
});

describe('an installation that keeps the system tenant alone', () => {
    const systemOnly = () => steps({ choice: 'system-only' });

    it('drops the tenant steps and the tenant sign-in from the rail', () => {
        expect(systemOnly().map((step) => step.id)).toEqual([
            'welcome',
            'administrator',
            'signIn',
            'ready',
        ]);
    });

    it('changes the rail itself when the choice changes, not just a label', () => {
        const tenant = steps({ choice: 'first-tenant' }).map((step) => step.id);
        const system = systemOnly().map((step) => step.id);

        expect(tenant).toHaveLength(9);
        expect(tenant.filter((id) => !system.includes(id))).toEqual([
            'profile',
            'details',
            'review',
            'provisioning',
            'handOff',
        ]);
    });

    it('leaves the welcome and the administrator where they were', () => {
        const tenant = steps({ choice: 'first-tenant' });
        const system = systemOnly();

        expect(indexOfStep(system, 'welcome')).toBe(indexOfStep(tenant, 'welcome'));
        expect(indexOfStep(system, 'administrator')).toBe(indexOfStep(tenant, 'administrator'));
    });

    it('marks the administrator sign-in as the one-way door', () => {
        const final = systemOnly()
            .filter((step) => step.final === true)
            .map((step) => step.id);

        expect(final).toEqual(['signIn']);
    });

    it('signs in as the administrator the journey created', async () => {
        const entered = vi.fn(async () => undefined);
        const signIn = steps({ choice: 'system-only', onAdministratorSignIn: entered }).find(
            (step) => step.id === 'signIn',
        );

        expect(signIn?.body).toBe('Administrator arrival body');
        expect(signIn?.lead).toBe(
            'The installation administrator signs in, and the installation is ready.',
        );
        expect(signIn?.next?.enabled).toBe(true);
        await signIn?.next?.run?.();
        expect(entered).toHaveBeenCalledTimes(1);
    });

    it('names the administrator it created once the installation is ready', () => {
        const ready = systemOnly().find((step) => step.id === 'ready');

        expect(ready?.lead).toBe('The installation is set up, and super_admin is signed in.');
    });

    it('records the finished wizard before it hands the browser over', async () => {
        const order: string[] = [];
        const onCompleteSystemOnboarding = vi.fn(async () => {
            order.push('complete');
        });
        const onFinished = vi.fn(() => {
            order.push('finished');
        });
        const onSignOutAfterBootstrap = vi.fn(async () => {
            order.push('signed-out');
        });

        const tenantReady = steps({
            choice: 'first-tenant',
            onCompleteSystemOnboarding,
            onFinished,
            onSignOutAfterBootstrap,
        }).find((step) => step.id === 'ready');
        await tenantReady?.next?.run?.();
        /*
         * The flag is what releases the gate, so it is written before the
         * hand-over. The tenant rail hands the browser over signed in, as the
         * administrator it just created.
         */
        expect(order).toEqual(['complete', 'finished']);

        order.length = 0;
        const systemReady = steps({
            choice: 'system-only',
            onCompleteSystemOnboarding,
            onFinished,
            onSignOutAfterBootstrap,
        }).find((step) => step.id === 'ready');
        await systemReady?.next?.run?.();
        /*
         * Bootstrap runs as the tenant's system party, and that is no party to
         * leave somebody sitting in, so the system-only rail ends at the
         * sign-in screen rather than handing the browser over.
         */
        expect(order).toEqual(['complete', 'signed-out']);
        expect(onCompleteSystemOnboarding).toHaveBeenCalledTimes(2);
    });

    it('allows a step back from the administrator, the first step past the choice', () => {
        const system = systemOnly();

        expect(canGoBack(system, indexOfStep(system, 'administrator'))).toBe(true);
        // The welcome is the first step, so there is nowhere behind it.
        expect(canGoBack(system, indexOfStep(system, 'welcome'))).toBe(false);
    });

    it('allows a step back from the sign-in, because no hand-over stands behind it', () => {
        const system = systemOnly();
        const tenant = steps({ choice: 'first-tenant' });

        expect(canGoBack(system, indexOfStep(system, 'signIn'))).toBe(true);
        /*
         * The tenant rail's sign-in has the hand-over behind it, and that step
         * changed server state, so the person may not walk back into it.
         */
        expect(canGoBack(tenant, indexOfStep(tenant, 'signIn'))).toBe(false);
    });
});

describe('the tenant rail the choice leaves alone', () => {
    it('keeps the tenant sign-in waiting for the tenant administrator', () => {
        const entry: TenantEntry = {
            kind: 'active',
            principal: 'northwind_admin@northwind',
            password: 'Typed-Password-1',
            resetRequired: false,
        };
        const signIn = steps({ entry, tenantSignInComplete: false }).find(
            (step) => step.id === 'signIn',
        );

        expect(signIn?.body).not.toBe('Administrator arrival body');
        expect(signIn?.body).not.toBeNull();
        expect(signIn?.next?.enabled).toBe(false);
        expect(signIn?.lead).toBe('The tenant administrator signs in for the first time.');
    });

    it('carries on as before once the tenant administrator has signed in', async () => {
        const entry: TenantEntry = {
            kind: 'active',
            principal: 'northwind_admin@northwind',
            password: 'Typed-Password-1',
            resetRequired: false,
        };
        const goTo = vi.fn();
        const signIn = steps({ entry, tenantSignInComplete: true, goTo }).find(
            (step) => step.id === 'signIn',
        );

        expect(signIn?.next?.enabled).toBe(true);
        await signIn?.next?.run?.();
        expect(goTo).toHaveBeenCalledWith('ready');
    });

    it('names the tenant administrator when the installation is ready', () => {
        const entry: TenantEntry = {
            kind: 'active',
            principal: 'northwind_admin@northwind',
            password: 'Typed-Password-1',
            resetRequired: false,
        };
        const ready = steps({ entry }).find((step) => step.id === 'ready');

        expect(ready?.lead).toBe(
            'The installation is set up, and northwind_admin@northwind is signed in.',
        );
    });
});

describe('an installation that has no administrator yet', () => {
    it('asks for the account that owns it, and creates it', async () => {
        const created = vi.fn(async () => undefined);
        const [, administrator] = steps({ onCreateAdministrator: created });

        expect(administrator?.title).toBe('Create the administrator');
        expect(administrator?.body).toBe('Administrator body');
        expect(administrator?.next?.label).toBe('Create administrator');
        await administrator?.next?.run?.();
        expect(created).toHaveBeenCalledTimes(1);
    });
});

describe('an installation that already has its administrator', () => {
    it('asks the person to sign in as it, because it cannot be created twice', () => {
        const [, administrator] = steps({ administratorExists: true });

        expect(administrator?.title).toBe('Sign in as the administrator');
        expect(administrator?.body).toBe('Administrator sign-in body');
        expect(administrator?.next?.label).toBe('Sign in and continue');
    });

    it('signs in rather than creating a second account', async () => {
        const created = vi.fn(async () => undefined);
        const entered = vi.fn(async () => undefined);
        const [, administrator] = steps({
            administratorExists: true,
            onCreateAdministrator: created,
            onAdministratorEntered: entered,
        });

        await administrator?.next?.run?.();
        expect(entered).toHaveBeenCalledTimes(1);
        expect(created).not.toHaveBeenCalled();
    });

    it('does not measure a password that already exists against the policy', () => {
        // The password was accepted when it was set. A rule tightened since
        // must not lock somebody out of the installation they own.
        const [, administrator] = steps({ administratorExists: true, acceptable: false });

        expect(administrator?.next?.enabled).toBe(true);
    });
});
