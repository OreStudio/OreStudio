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
import { newTenantSteps } from './newTenantSteps.js';
import { handOffToTenant } from './NewTenantJourney.js';
import { detailsFor, type NewTenant } from './state.js';
import type { JourneyServer } from './server.js';
import type { PasswordPolicy, PartySummary, SeedProfileChoice } from '@ores/wire-protocol/browser';

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

const party: PartySummary = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Corporation',
    partyCategory: 'System',
    businessCenterCode: 'GBLO',
};

/** A production starting point: it types its own administrator's password. */
const operational: SeedProfileChoice = {
    code: 'empty_operational',
    name: 'Operational',
    summary: '',
    audience: 'For real use',
    bullets: [],
    tenant: {
        name: 'Northwind Capital',
        code: 'northwind',
        hostname: 'northwind',
        adminUsername: 'northwind_admin',
        adminEmail: 'admin@northwind.com',
    },
    inheritsAdminPassword: false,
    forcePasswordChange: false,
    order: 10,
    steps: [
        { kind: 'publish_bundle', order: 10 },
        { kind: 'provision_party', order: 30 },
    ],
    parameters: [],
};

/** A demonstration starting point: it inherits the creating password. */
const demonstration: SeedProfileChoice = {
    ...operational,
    code: 'acme_demo',
    name: 'ACME demo',
    tenant: {
        name: 'Acme Corporation',
        code: 'acme_corporation',
        hostname: 'acme_corporation',
        adminUsername: 'tenant_admin',
        adminEmail: 'admin@acme_corporation.com',
    },
    inheritsAdminPassword: true,
};

function fakeServer(overrides: Partial<JourneyServer> = {}): JourneyServer {
    return {
        createAdministrator: vi.fn(async () => undefined),
        recheckBootstrap: vi.fn(async () => undefined),
        completeSystemOnboarding: vi.fn(async () => undefined),
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: vi.fn(async () => undefined),
        switchParty: vi.fn(async () => undefined),
        signOut: vi.fn(async () => undefined),
        passwordPolicy: vi.fn(async () => policy),
        seedProfiles: vi.fn(async () => [operational, demonstration]),
        tenantCodes: vi.fn(async () => []),
        provision: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
            tenantId: '',
            accountId: '',
        })),
        provisionParty: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            partyId: '',
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
        leiEntities: vi.fn(async () => []),
        ...overrides,
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
    state: NewTenant,
    options: { readonly server?: JourneyServer; readonly creatingPassword?: string } = {},
): ReturnType<typeof newTenantSteps> {
    return newTenantSteps({
        t,
        server: options.server ?? fakeServer(),
        policy,
        profiles: [operational, demonstration],
        state,
        creatingPassword: options.creatingPassword ?? '',
        onHandOff: async () => undefined,
    });
}

describe('the tenant journey a signed-in administrator runs', () => {
    it('is the five tenant steps on one rail', () => {
        expect(steps(tenantState()).map((step) => step.id)).toEqual([
            'profile',
            'details',
            'review',
            'provisioning',
            'handOff',
        ]);
    });

    it('asks the demonstration tenant for a password of its own, because this journey holds none', () => {
        /*
         * The demonstration profile hands the creating administrator's password
         * to the tenant's administrator. This journey has no creating
         * administrator, so there is nothing to hand over: the offer is dropped
         * and the person types a password. The step therefore cannot move on
         * until they have, and it moves on as soon as they do.
         */
        const details = detailsFor(demonstration, '');
        expect(details.useMyPassword).toBe(false);

        const without = steps(tenantState({ profile: demonstration, details }));
        expect(without[1]!.next?.enabled).toBe(false);

        const withTyped = steps(
            tenantState({
                profile: demonstration,
                details: { ...details, adminPassword: 'Typed-Password-1' },
                passwordAcceptable: true,
            }),
        );
        expect(withTyped[1]!.next?.enabled).toBe(true);
    });

    it('keeps the offer for first run, which does hold the creating password', () => {
        const details = detailsFor(demonstration, 'Creating-Password-1');
        expect(details.useMyPassword).toBe(true);

        const state = tenantState({ profile: demonstration, details });
        expect(steps(state, { creatingPassword: 'Creating-Password-1' })[1]!.next?.enabled).toBe(
            true,
        );
    });

    it('provisions the described tenant and records the run it starts', async () => {
        const server = fakeServer();
        const state = tenantState({
            profile: operational,
            details: { ...detailsFor(operational, ''), adminPassword: 'Typed-Password-1' },
            passwordAcceptable: true,
        });

        await steps(state, { server })[2]!.next!.run!();

        expect(server.provision).toHaveBeenCalledWith({
            profileCode: 'empty_operational',
            tenantCode: 'northwind',
            tenantName: 'Northwind Capital',
            tenantHostname: 'northwind',
            tenantDescription: '',
            adminUsername: 'northwind_admin',
            adminEmail: 'admin@northwind.com',
            adminPassword: 'Typed-Password-1',
            parameters: {},
        });
        expect(state.recordRun).toHaveBeenCalledWith('9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d');
    });

    it('handing off to somebody else ends the session and stops there', async () => {
        const server = fakeServer();

        await handOffToTenant(server, detailsFor(operational, ''), false, 'choose one');

        expect(server.signOut).toHaveBeenCalled();
        expect(server.signIn).not.toHaveBeenCalled();
    });

    it('continuing as the tenant admin signs in as the account it just made', async () => {
        const server = fakeServer();

        await handOffToTenant(server, detailsFor(operational, ''), true, 'choose one');

        expect(server.signOut).toHaveBeenCalled();
        expect(server.signIn).toHaveBeenCalledWith({
            username: 'northwind_admin@northwind',
            password: '',
        });
    });

    it('chooses the only party the new administrator works in', async () => {
        const server = fakeServer({
            signIn: vi.fn(async () => ({
                outcome: 'party-required',
                parties: [party],
                passwordResetRequired: false,
            })),
        });

        await handOffToTenant(server, detailsFor(operational, ''), true, 'choose one');

        expect(server.chooseParty).toHaveBeenCalledWith(party.id, [party]);
    });

    it('refuses to guess when the new administrator works in several parties', async () => {
        const other: PartySummary = { ...party, id: '3f1e2d4c-0000-4000-8000-000000000011' };
        const server = fakeServer({
            signIn: vi.fn(async () => ({
                outcome: 'party-required',
                parties: [party, other],
                passwordResetRequired: false,
            })),
        });

        await expect(
            handOffToTenant(server, detailsFor(operational, ''), true, 'choose one'),
        ).rejects.toThrow('choose one');
        expect(server.chooseParty).not.toHaveBeenCalled();
    });
});
