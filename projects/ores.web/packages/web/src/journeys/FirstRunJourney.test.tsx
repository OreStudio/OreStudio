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
 * and the web package has no browser to put it in, so the choice is tested on
 * the card that offers it and on the stages it states. The rail the choice
 * produces is asserted where it is built, in `firstRunSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import {
    AdministratorArrival,
    FirstRunJourney,
    WelcomeCard,
    welcomeStages,
} from './FirstRunJourney.js';
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

describe('the welcome card', () => {
    it('offers both endings, and marks the chosen one', () => {
        const html = render(<WelcomeCard choice="first-tenant" onChoice={() => undefined} />);

        expect(html).toContain('Create the first tenant');
        expect(html).toContain('Keep the system tenant alone');
        expect(html.match(/aria-checked="true"/g)).toHaveLength(1);
        expect(html).toContain('aria-checked="true"');
    });

    it('states the stages of the tenant journey, including the tenant', () => {
        const html = render(<WelcomeCard choice="first-tenant" onChoice={() => undefined} />);

        expect(html).toContain('The first tenant, built from a starting point on the server.');
        expect(html).not.toContain('with the password the journey holds.');
    });

    it('states the system tenant alone, and no tenant stage, when that is chosen', () => {
        const html = render(<WelcomeCard choice="system-only" onChoice={() => undefined} />);

        expect(html).toContain('with the password the journey holds.');
        expect(html).not.toContain('The first tenant, built from a starting point on the server.');
    });

    it('offers the choice itself, so a person cannot miss it', () => {
        const html = render(<WelcomeCard choice="system-only" onChoice={() => undefined} />);

        expect(html).toContain('aria-label="What the installation is left with"');
        expect(html).toContain('role="radiogroup"');
        expect(html).toContain('role="radio"');
    });
});

describe('the stages the choice states', () => {
    it('are the tenant rail stages, and the system-only ones without the tenant', () => {
        expect(welcomeStages('first-tenant').map(([title]) => title)).toEqual([
            'journey.welcome.stage.admin',
            'journey.welcome.stage.tenant',
            'journey.welcome.stage.signIn',
        ]);
        expect(welcomeStages('system-only').map(([title]) => title)).toEqual([
            'journey.welcome.stage.admin',
            'journey.welcome.stage.signIn',
        ]);
    });
});

describe('the system-only arrival', () => {
    it('names the administrator that signs in', () => {
        const html = render(<AdministratorArrival principal="super_admin" />);

        expect(html).toContain('super_admin');
        expect(html).not.toContain('tenant');
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
