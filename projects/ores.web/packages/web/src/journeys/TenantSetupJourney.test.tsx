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
 * The tenant setup screen, pinned to the states it can be in.
 *
 * The screen follows a run rather than a rail, so what matters is what it shows
 * for the run it was given: nothing while the read is in flight, the run itself
 * while it goes, and a plain statement when the tenant has no run at all. The
 * read that produces those states is the container's, and a render-to-string
 * page cannot run it, so the panel is asserted where it is a function of its
 * answer.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { TenantSetupPanel } from './TenantSetupJourney.js';
import type { JourneyServer } from './server.js';

const NEVER = vi.fn(async () => undefined);

/** A server that answers nothing, because no render reaches the server. */
function fakeServer(): JourneyServer {
    return {
        createAdministrator: NEVER,
        recheckBootstrap: NEVER,
        completeSystemOnboarding: NEVER,
        tenantSetupRun: vi.fn(async () => ({ instanceId: '', status: '', error: '' })),
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
            status: 'in_progress',
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

function render(run: { instanceId: string; status: string; error: string } | undefined): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <TenantSetupPanel run={run} server={fakeServer()} onFinished={() => undefined} />
        </TranslationProvider>,
    );
}

/**
 * The door's own opening tag, less its styling, so an assertion reads the
 * button's `disabled` attribute rather than the word in the class names it
 * also carries.
 */
function doorTag(html: string): string {
    const start = html.indexOf('<button');
    if (start === -1) {
        return '';
    }
    return html.slice(start, html.indexOf('>', start)).replace(/class="[^"]*"/, '');
}

describe('the tenant setup screen', () => {
    it('waits rather than stating a run that has not been read', () => {
        const html = render(undefined);

        expect(html).toContain('Loading...');
        expect(html).not.toContain('Finish setting up your tenant');
    });

    it('states that nobody started the tenant setup, and offers no way in', () => {
        const html = render({ instanceId: '', status: '', error: '' });

        expect(html).toContain('Finish setting up your tenant');
        expect(html).toContain(
            'setup has not been started. The deployment administrator has to start it.',
        );
        expect(html).not.toContain('Go to the application');
    });

    it('follows the tenant run, and opens the door only once it completes', () => {
        const html = render({
            instanceId: '55555555-5555-5555-5555-555555555555',
            status: 'in_progress',
            error: '',
        });

        expect(html).toContain('Finish setting up your tenant');
        expect(html).toContain('Go to the application');
        // The run is still going, so the way into the application is closed.
        expect(doorTag(html)).toContain('disabled');
    });

    it('opens the door on the run the tenant itself reported finished', () => {
        const html = render({
            instanceId: '55555555-5555-5555-5555-555555555555',
            status: 'completed',
            error: '',
        });

        expect(html).toContain('Go to the application');
        // The run the panel was handed finished, so the door is open even
        // before the rail has read the run's progress for itself.
        expect(doorTag(html)).not.toContain('disabled');
    });
});
