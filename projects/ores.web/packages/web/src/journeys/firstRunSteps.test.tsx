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
import { firstRunSteps } from './firstRunSteps.js';
import { detailsFor, type NewTenant } from './state.js';
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
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: vi.fn(async () => undefined),
        signOut: vi.fn(async () => undefined),
        passwordPolicy: vi.fn(async () => policy),
        seedProfiles: vi.fn(async () => [profile]),
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
            current_step_index: 0,
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
        entry: undefined,
        welcome: 'Welcome body',
        administratorForm: 'Administrator body',
        goTo: vi.fn(),
        onAdministratorSignedIn: vi.fn(async () => undefined),
        onHandOff: vi.fn(async () => undefined),
        onFinished: vi.fn(),
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
