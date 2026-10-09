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
 * The first run page's own parts.
 *
 * The page reaches the server for its password rules before it renders a rail,
 * and the web package has no browser to put it in, so the welcome it shows is
 * tested where it renders. The rail the starting point produces, and the
 * starting point itself, are asserted where they are built, in
 * `firstRunSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import {
    AdministratorArrival,
    FirstRunJourney,
    WelcomeIntro,
    systemPartyOf,
} from './FirstRunJourney.js';
import type { PartySummary } from '@ores/wire-protocol/browser';
import type { JourneyServer } from './server.js';

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

const NEVER = vi.fn(async () => undefined);

/** A server that answers nothing, because no render reaches the server. */
function fakeServer(): JourneyServer {
    return {
        createAdministrator: NEVER,
        recheckBootstrap: NEVER,
        completeSystemOnboarding: NEVER,
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: NEVER,
        switchParty: NEVER,
        signOut: NEVER,
        passwordPolicy: vi.fn(async () => ({
            success: true,
            message: '',
            minLength: 12,
            requireUppercase: true,
            requireLowercase: true,
            requireDigit: true,
            requireSpecial: true,
            specialChars: '!@#$%^&*()_+-=[]{}|;:,.<>?',
        })),
        registrationPolicy: vi.fn(async () => {
            throw new Error('not asked');
        }),
        signup: vi.fn(async () => {
            throw new Error('not asked');
        }),
        seedProfiles: vi.fn(async () => []),
        tenantCodes: vi.fn(async () => []),
        leiEntities: vi.fn(async () => []),
        provision: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
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
            status: '',
            error: '',
            step_count: 0,
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
        changePassword: NEVER,
    };
}

describe('the welcome', () => {
    it('introduces the setup, and asks nothing', () => {
        const html = render(<WelcomeIntro />);

        expect(html).toContain('Create the administrator');
        expect(html).toContain('Create the first tenant');
        expect(html).toContain('Sign in');
        expect(html).not.toContain('Bare system');
        expect(html).not.toContain('role="radiogroup"');
    });
});

describe('the system-only arrival', () => {
    it('names the administrator that signs in', () => {
        const html = render(<AdministratorArrival principal="super_admin" />);

        expect(html).toContain('super_admin');
        expect(html).not.toContain('tenant');
    });
});

describe('the party bootstrap signs in to', () => {
    const party = (
        id: string,
        partyCategory: string,
    ): PartySummary => ({
        id,
        name: `${partyCategory} party`,
        partyCategory,
        businessCenterCode: 'WRLD',
    });

    it('is the system party, whatever else the account works in', () => {
        const operational = party('11111111-1111-1111-1111-111111111111', 'Operational');
        const system = party('22222222-2222-2222-2222-222222222222', 'System');
        const other = party('33333333-3333-3333-3333-333333333333', 'Operational');

        expect(systemPartyOf([operational, system, other])).toBe(system);
    });

    it('is nothing when the account works in no system party', () => {
        expect(systemPartyOf([party('11111111-1111-1111-1111-111111111111', 'Operational')]))
            .toBeUndefined();
    });
});

describe('the page before the deployment answers', () => {
    it('waits for the password rules rather than rendering a rail without them', () => {
        const html = render(
            <FirstRunJourney
                server={fakeServer()}
                inBootstrapMode={true}
                onStarted={() => undefined}
                onFinished={() => undefined}
            />,
        );

        expect(html).toContain('Loading...');
        expect(html).not.toContain('<nav');
    });
});
