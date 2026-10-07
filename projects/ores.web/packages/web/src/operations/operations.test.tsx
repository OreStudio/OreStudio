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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { SessionView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { AppRoutes } from '../AppRoutes.js';
import { menuFor } from '../shell/areas.js';
import type { BootstrapState } from '../session/BootstrapProvider.js';
import { OperationsArea } from './OperationsArea.js';
import { VersionsPage } from './VersionsPage.js';

/**
 * The operations area, as a reader sees it.
 *
 * What is checked is what the area promises: the versions screen states the
 * client, the server and the database from the session and asks for nothing
 * itself, it states only the gaps that are still open, and it reaches the
 * journeys that follow it without pointing at screens that do not exist.
 */

const PARTY = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const SESSION_VERSION = 'v0.0.25 [x64-linux] (local a1e507d, 2026-10-04)';

function sessionWith(overrides: Partial<SessionView> = {}): SessionView {
    return {
        username: 'sysadmin',
        email: 'sysadmin@acme.test',
        accountId: '3f1e2d4c-0000-4000-8000-000000000001',
        tenantId: '3f1e2d4c-0000-4000-8000-000000000002',
        tenantName: 'System',
        mode: 'system-administration',
        version: SESSION_VERSION,
        database: {
            fingerprint: '1109eccab21e8fe8',
            environment: 'development',
            commit: 'a1e507d',
            created: '2026-10-04 14:02',
        },
        party: PARTY,
        availableParties: [PARTY],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        ...overrides,
    } as SessionView;
}

const READY: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    message: '',
    version: 'v0.0.25 (test)',
};

function withProviders(element: ReactNode, path = '/'): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [] });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>{element}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function renderRoute(path: string, session: SessionView): string {
    return withProviders(
        <AppRoutes
            gate={READY}
            session={{ status: 'authenticated', session }}
            journey={<p>First run journey</p>}
            newTenantJourney={<p>New tenant journey</p>}
            newPartyJourney={<p>New party journey</p>}
            signUpJourney={<p>Registration door</p>}
            journeyInProgress={false}
            onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
            onChooseParty={async () => undefined}
            onSignOut={() => undefined}
            onRetryBootstrap={() => undefined}
        />,
        path,
    );
}

/** The markup of one card, whose heading reads exactly `heading`. */
function section(html: string, heading: string): string {
    const start = html.indexOf(`>${heading}</h2>`);
    expect(start).toBeGreaterThanOrEqual(0);
    const end = html.indexOf('</section>', start);
    expect(end).toBeGreaterThan(start);
    return html.slice(start, end);
}

/** The value one field states inside a section, or nothing when it is absent. */
function fieldValue(html: string, label: string): string | undefined {
    return new RegExp(`>${label}</dt><dd[^>]*>(.*?)</dd>`).exec(html)?.[1];
}

describe('the operations area', () => {
    it('lists its screens, and links only the one that is built', () => {
        const html = withProviders(<OperationsArea />);

        expect(html).toContain('href="/operations/versions"');
        expect(html).toContain('Running services');
        expect(html).toContain('Compute grid');
        expect(html).toContain('Message bus');
        expect(html).toContain('Telemetry logs');
        // The four screens the later units add carry no link at all, so a
        // reader is never sent to a route that does not exist.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(4);
        for (const route of [
            '/operations/services',
            '/operations/grid',
            '/operations/bus',
            '/operations/logs',
        ]) {
            expect(html).not.toContain(`href="${route}"`);
        }
    });

    it('is offered to system administration alone', () => {
        const routes = (mode: 'system-administration' | 'tenant-administration' | 'application') =>
            menuFor(mode).map((item) => item.to);

        expect(routes('system-administration')).toContain('/operations');
        expect(routes('tenant-administration')).not.toContain('/operations');
        expect(routes('application')).not.toContain('/operations');
    });
});

describe('the versions screen', () => {
    it('states the client, the server and the database from the session', () => {
        const html = withProviders(
            <VersionsPage session={sessionWith()} serverVersion={SESSION_VERSION} />,
        );

        expect(html).toContain('Operations: versions and the database');
        expect(fieldValue(section(html, 'Client'), 'Version')).toBe(__BUILD_RELEASE__);
        expect(fieldValue(section(html, 'Server'), 'Version')).toBe(SESSION_VERSION);
        expect(fieldValue(section(html, 'Database'), 'Fingerprint')).toBe('1109eccab21e8fe8');
        expect(fieldValue(section(html, 'Database'), 'Environment')).toBe('development');
        expect(fieldValue(section(html, 'Database'), 'Created')).toBe('2026-10-04 14:02');
    });

    it('states the database as unknown when the session carried no row', () => {
        const empty = sessionWith({
            database: { fingerprint: '', environment: '', commit: '', created: '' },
        });
        const html = withProviders(
            <VersionsPage session={empty} serverVersion={SESSION_VERSION} />,
        );

        // Each database field states unknown, and no invented value stands in
        // for any of them.
        const database = section(html, 'Database');
        for (const label of ['Fingerprint', 'Environment', 'Commit', 'Created']) {
            expect({ label, value: fieldValue(database, label) }).toEqual({
                label,
                value: 'unknown',
            });
        }
        expect(html).not.toContain('1109eccab21e8fe8');
        // The address is unknown too: a rendered-to-string page has no window
        // to read it from.
        expect(fieldValue(section(html, 'Server'), 'Address')).toBe('unknown');
    });

    it('resolves the server build the footer resolves, when the session carried none', () => {
        const empty = sessionWith({ version: '' });
        const bootstrap = 'v0.0.25 (bootstrap)';
        const html = withProviders(<VersionsPage session={empty} serverVersion={bootstrap} />);

        expect(fieldValue(section(html, 'Server'), 'Version')).toBe(bootstrap);
    });

    it('states only the gaps that are still open', () => {
        const html = withProviders(
            <VersionsPage session={sessionWith()} serverVersion={SESSION_VERSION} />,
        );

        expect(html).toContain('The client and the server strings do not share a shape');
        expect(html).toContain('The per-instance versions are releases only');
        // Each gap names the journey that records it, as its lead promises.
        expect(html).toContain('Recorded by Check the versions and the database');
        expect(html).toContain('Recorded by See the running services');
        // The database row travels on the login answer now, so its gap is
        // closed and must not be stated as open.
        expect(html).not.toContain('The login answer does not carry the database row yet');
    });

    it('reaches the related journeys, and names the ones with no screen yet', () => {
        const html = withProviders(
            <VersionsPage session={sessionWith()} serverVersion={SESSION_VERSION} />,
        );

        expect(html).toContain('href="/audit"');
        expect(html).toContain('See the running services');
        expect(html).toContain('Read the telemetry logs');
        expect(html).toContain('Watch the compute grid');
        expect(html).toContain('Audit sign-ins');
        // Three journeys keep their screen for a later unit: they are stated,
        // marked as unbuilt, and carry no link.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(3);
        expect(html).not.toContain('href="/operations/services"');
    });
});

describe('the route to the versions screen', () => {
    it('opens it in every mode', () => {
        for (const mode of [
            'system-administration',
            'tenant-administration',
            'application',
        ] as const) {
            const html = renderRoute('/operations/versions', sessionWith({ mode }));

            expect({ mode, has: html.includes('Operations: versions and the database') }).toEqual({
                mode,
                has: true,
            });
        }
    });

    it('is offered from home in the modes with no operations menu entry', () => {
        const tenant = renderRoute('/', sessionWith({ mode: 'tenant-administration' }));
        const party = renderRoute('/', sessionWith({ mode: 'application' }));

        expect(tenant).toContain('href="/operations/versions"');
        expect(party).toContain('href="/operations/versions"');
    });

    it('is offered to system administration through the operations hub', () => {
        const html = renderRoute('/', sessionWith({ mode: 'system-administration' }));

        expect(html).toContain('href="/operations"');
    });

    it('renders through the shell, so it carries the footer with both builds', () => {
        const html = renderRoute('/operations/versions', sessionWith());

        // The shell is what carries the footer; the brand and the account menu
        // come from it too.
        expect(html).toContain('ORE Studio');
        expect(html).toContain('Your account');
        expect(html).toContain(`client ${__BUILD_VERSION__}`);
        expect(html).toContain(`server ${SESSION_VERSION}`);
    });
});
