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

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { HomePage } from './HomePage.js';

/**
 * The landing, per mode.
 *
 * The mode decides the shape of this page and nothing else does, so what is
 * asserted here is that each mode lands on its own content: the system
 * administrator on the tenant management journeys, and a mode whose journeys
 * are not implemented yet on the session it is working in. The card that opens
 * a journey is a link and the card for a journey the tree has not built is not,
 * which is the difference a person can act on.
 */

function landing(mode: SessionMode): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <HomePage
                    username="super_admin"
                    email="super_admin@system.ores"
                    tenantName="System"
                    partyName="System Party"
                    mode={mode}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );
}

describe('the system administration landing', () => {
    it('states the mode and offers the tenant management journeys', () => {
        const html = landing('system-administration');

        expect(html).toContain('System administration');
        expect(html).toContain('Tenants');
        expect(html).toContain('See the tenants');
        expect(html).toContain('New tenant');
        expect(html).toContain('Retire or reset a tenant');
    });

    it('opens the journeys the tree has built, and only those', () => {
        const html = landing('system-administration');

        expect(html).toContain('href="/tenants"');
        expect(html).toContain('href="/tenants/new"');
        expect(html).toContain('Not built yet');
    });

    /*
     * The bootstrap journey belongs to a plain installation. There is no
     * account to sign in with before it has run, so it is the door of that
     * state rather than a place a signed-in person is offered.
     */
    it('does not offer the first run journey', () => {
        expect(landing('system-administration')).not.toContain('First run');
    });
});

describe('a mode with no journeys yet', () => {
    it('lands on the session it is working in', () => {
        const html = landing('application');

        expect(html).toContain('Signed in');
        expect(html).toContain('super_admin@system.ores');
        expect(html).toContain('System Party');
    });

    it('offers no area of another mode', () => {
        expect(landing('application')).not.toContain('Tenants');
    });

    /*
     * Only system administration creates a tenant, and this card is what every
     * other mode lands on, so it must not offer a journey the server refuses.
     */
    it('does not offer a new tenant', () => {
        const html = landing('tenant-administration');

        expect(html).not.toContain('href="/tenants/new"');
        expect(html).toContain('href="/parties/new"');
    });
});
