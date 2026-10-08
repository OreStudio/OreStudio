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
import type { ServiceRosterRow, SessionView } from '@ores/wire-protocol/browser';
import type { GridNodeRow, GridView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import { frFlat } from '../i18n/locales/fr.js';
import { createTranslator } from '../i18n/translate.js';
import { AppRoutes } from '../AppRoutes.js';
import { menuFor } from '../shell/areas.js';
import type { BootstrapState } from '../session/BootstrapProvider.js';
import { OperationsArea } from './OperationsArea.js';
import {
    SERVICE_RUNNING_WINDOW_MINUTES,
    compareVersions,
    newestVersionOf,
} from './OperationsParts.js';
import { ServicesPage, SERVICES_QUERY_KEY, formatAge, readTime } from './ServicesPage.js';
import {
    GridPage,
    GRID_QUERY_KEY,
    asGiB,
    asMiB,
    asSeconds,
    formatAge as formatGridAge,
    sampleTime,
} from './GridPage.js';
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

/** The source-language translator, for the units a bare function states. */
const t = createTranslator('en', enFlat, enFlat).t;

function withProviders(
    element: ReactNode,
    path = '/',
    roster: readonly ServiceRosterRow[] = [],
    grid: GridView = gridView(),
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [] });
    client.setQueryData(SERVICES_QUERY_KEY, roster);
    client.setQueryData(GRID_QUERY_KEY, grid);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>{element}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function renderRoute(
    path: string,
    session: SessionView,
    roster: readonly ServiceRosterRow[] = [],
    grid: GridView = gridView(),
): string {
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
        roster,
        grid,
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

/** One roster row as the BFF answers it: an expected instance and its age. */
function rosterRow(overrides: Partial<ServiceRosterRow> = {}): ServiceRosterRow {
    return {
        service_name: 'ores.iam.service',
        display_name: 'IAM Service',
        description: '',
        service_account: null,
        slot: 1,
        state: 'running',
        instance_id: '91b0f33d-4b32-4f65-8809-2d3e4f506172',
        host_id: null,
        version: 'v0.0.25',
        sampled_at: '2026-10-04 14:32:00Z',
        age_seconds: 6,
        ...overrides,
    };
}

/** One node row as the BFF answers it: the measurements, the hostname and its runner. */
function gridNode(overrides: Partial<GridNodeRow> = {}): GridNodeRow {
    return {
        host_id: '9e0f33aa-0000-4000-8000-000000000001',
        host: 'grid-01.example.com',
        instance_id: '1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6',
        state: 'running',
        version: 'v0.0.25',
        tasks_completed: 1284,
        tasks_failed: 0,
        tasks_since_last: 12,
        avg_task_duration_ms: 42_000,
        max_task_duration_ms: 51_000,
        input_bytes_fetched: 1_288_490_188,
        output_bytes_uploaded: 230_686_720,
        seconds_since_hb: 8,
        ...overrides,
    };
}

/** The grid view as the BFF answers it. */
function gridView(overrides: Partial<GridView> = {}): GridView {
    return {
        sampled_at: '2026-10-04 14:31:02Z',
        total_hosts: 6,
        online_hosts: 5,
        idle_hosts: 2,
        total_workunits: 128,
        total_batches: 9,
        active_batches: 3,
        outcomes_success: 412,
        outcomes_client_error: 3,
        outcomes_no_reply: 1,
        nodes: [gridNode()],
        ...overrides,
    };
}

describe('the operations area', () => {
    it('lists its screens, and links only the three that are built', () => {
        const html = withProviders(<OperationsArea />);

        expect(html).toContain('href="/operations/versions"');
        expect(html).toContain('href="/operations/services"');
        expect(html).toContain('href="/operations/grid"');
        expect(html).toContain('Running services');
        expect(html).toContain('Compute grid');
        expect(html).toContain('Message bus');
        expect(html).toContain('Telemetry logs');
        // The two screens the later units add carry no link at all, so a
        // reader is never sent to a route that does not exist.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(2);
        for (const route of ['/operations/bus', '/operations/logs']) {
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
        // One journey keeps its screen for a later unit: the telemetry logs are
        // stated, marked as unbuilt, and carry no link. The services and grid
        // screens exist now, so the versions page links to them rather than
        // naming them as unbuilt.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(1);
        expect(html).toContain('href="/operations/services"');
        expect(html).toContain('href="/operations/grid"');
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

describe('the services screen', () => {
    it('shows one row per expected instance, as running, lost or missing', () => {
        const html = withProviders(<ServicesPage />, '/', [
            rosterRow({ slot: 1 }),
            rosterRow({ slot: 2, instance_id: 'a3f81c02-6d44-4b0e-9c21-7f5e0d8a1b34' }),
            rosterRow({
                service_name: 'ores.reporting.service',
                slot: 1,
                state: 'lost',
                instance_id: 'c9a3f10b-8f76-4da9-8c4d-61728394a5b6',
                version: 'v0.0.24',
                age_seconds: 900,
            }),
            rosterRow({
                service_name: 'ores.analytics.service',
                slot: 1,
                state: 'missing',
                instance_id: null,
                version: null,
                sampled_at: null,
                age_seconds: null,
            }),
        ]);

        // Every expected instance has a row, whether it reports or not: an
        // empty table would read as an installation with no services.
        expect(html).toContain('ores.iam.service');
        expect(html).toContain('ores.reporting.service');
        expect(html).toContain('ores.analytics.service');
        expect(html).toContain(
            `2 of 4 instances reported in the last ${String(SERVICE_RUNNING_WINDOW_MINUTES)} minutes`,
        );
        // The state words are the read's own; an instance that went quiet is
        // lost, never stopped, because a heartbeat cannot tell the two apart.
        expect(html.match(/>running</g) ?? []).toHaveLength(2);
        expect(html.match(/>lost</g) ?? []).toHaveLength(1);
        expect(html.match(/>missing</g) ?? []).toHaveLength(1);
        expect(html).not.toContain('>stopped<');
        // Two of two reported for IAM; nothing reported for either quiet
        // service, so their counts are warned.
        expect(html).toContain('2 of 2');
        expect(html.match(/0 of 1/g) ?? []).toHaveLength(2);
        // The instance id is shown by its first eight characters.
        expect(html).toContain('>91b0f33d<');
        // The age is what the BFF marked, so the screen does no subtraction.
        expect(html).toContain('6 s ago');
        expect(html).toContain('15 m 0 s ago');
    });

    it('leaves the compute runners to the grid screen', () => {
        const html = withProviders(<ServicesPage />, '/', [
            rosterRow(),
            rosterRow({
                service_name: 'ores.compute.wrapper',
                slot: 1,
                instance_id: '1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6',
            }),
        ]);

        expect(html).toContain('ores.iam.service');
        expect(html).not.toContain('ores.compute.wrapper');
    });

    it('names a service whose running instance trails the newest release', () => {
        const html = withProviders(<ServicesPage />, '/', [
            rosterRow(),
            rosterRow({
                service_name: 'ores.analytics.service',
                slot: 1,
                version: 'v0.0.24',
                instance_id: 'a3f81c02-6d44-4b0e-9c21-7f5e0d8a1b34',
            }),
        ]);

        expect(html).toContain('older build');
        expect(html).toContain(
            'Version skew: ores.analytics.service run v0.0.24 while the rest run v0.0.25',
        );
    });

    it('calls v0.0.10 the newest release, and warns about v0.0.9', () => {
        const html = withProviders(<ServicesPage />, '/', [
            rosterRow({ version: 'v0.0.10' }),
            rosterRow({
                service_name: 'ores.analytics.service',
                slot: 1,
                version: 'v0.0.9',
                instance_id: 'a3f81c02-6d44-4b0e-9c21-7f5e0d8a1b34',
            }),
        ]);

        // String order would call v0.0.9 the newest and warn about v0.0.10
        // instead, which is the wrong row and the wrong release.
        expect(html).toContain('Version skew: ores.analytics.service run v0.0.9');
        expect(html).toContain('while the rest run v0.0.10');
    });

    it('states only the gaps that are still open', () => {
        const html = withProviders(<ServicesPage />, '/', [rosterRow()]);

        expect(html).toContain('Why an instance went quiet');
        expect(html).toContain('The heartbeat states the release, not the build');
        expect(html).toContain('No uptime');
        // Each gap names the journey that records it, as its lead promises.
        expect(html).toContain('Recorded by See the running services');
        // The roster read, its order, its permission and the host on the
        // heartbeat are served now, so those gaps must not be stated as open.
        for (const closed of [
            'The expected services are not a read',
            'The reply is unordered',
            'No permission gates the read',
            'The state comes from absence, not from a read',
            'The heartbeat carries a host',
        ]) {
            expect(html).not.toContain(closed);
        }
    });

    it('reaches the related journeys, and names the ones with no screen yet', () => {
        const html = withProviders(<ServicesPage />, '/', [rosterRow()]);

        expect(html).toContain('href="/operations/versions"');
        expect(html).toContain('href="/audit"');
        expect(html).toContain('href="/operations/grid"');
        expect(html).toContain('Watch the compute grid');
        expect(html).toContain('Watch the message bus');
        expect(html).toContain('Read the telemetry logs');
        // Two journeys keep their screen for a later unit: they are stated,
        // marked as unbuilt, and carry no link. The grid screen exists now, so
        // the services page links to it rather than naming it as unbuilt.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(2);
        for (const route of ['/operations/bus', '/operations/logs']) {
            expect(html).not.toContain(`href="${route}"`);
        }
    });

    it('states a read time labelled UTC, and an age in units a person reads', () => {
        expect(readTime(Date.UTC(2026, 9, 4, 14, 32, 5))).toBe('14:32:05 UTC');
        expect(formatAge(6, t)).toBe('6 s');
        expect(formatAge(900, t)).toBe('15 m 0 s');
        expect(formatAge(7_320, t)).toBe('2 h 2 m');
    });

    it('takes the age units from the catalogue rather than from English', () => {
        const french = createTranslator('fr', enFlat, frFlat).t;

        expect(formatAge(900, french)).toBe('15 min 0 s');
    });
});

describe('the newest release among the running instances', () => {
    it('orders releases by their numbers, not by their spelling', () => {
        // String order would put v0.0.9 above v0.0.10 and v0.9.0 above
        // v0.10.0, naming the older release as the newest.
        expect(compareVersions('v0.0.9', 'v0.0.10')).toBeLessThan(0);
        expect(compareVersions('v0.9.0', 'v0.10.0')).toBeLessThan(0);
        expect(
            newestVersionOf([rosterRow({ version: 'v0.0.9' }), rosterRow({ version: 'v0.0.10' })]),
        ).toBe('v0.0.10');
        expect(
            newestVersionOf([rosterRow({ version: 'v0.9.0' }), rosterRow({ version: 'v0.10.0' })]),
        ).toBe('v0.10.0');
    });

    it('treats a component one release omits as zero', () => {
        expect(compareVersions('v1.2', 'v1.2.0')).toBe(0);
        expect(compareVersions('v1.2.0', 'v1.2')).toBe(0);
        expect(compareVersions('v1.2.1', 'v1.2')).toBeGreaterThan(0);
    });

    it('accepts a release with no leading v', () => {
        expect(compareVersions('0.0.10', 'v0.0.9')).toBeGreaterThan(0);
        expect(compareVersions('0.0.10', 'v0.0.10')).toBe(0);
        expect(
            newestVersionOf([rosterRow({ version: '0.0.9' }), rosterRow({ version: 'v0.0.10' })]),
        ).toBe('v0.0.10');
    });
});

describe('the route to the services screen', () => {
    it('opens it, and renders through the shell', () => {
        const html = renderRoute('/operations/services', sessionWith(), [rosterRow()]);

        expect(html).toContain('Operations: services');
        expect(html).toContain('ORE Studio');
        expect(html).toContain('Your account');
        expect(html).toContain(`client ${__BUILD_VERSION__}`);
        expect(html).toContain(`server ${SESSION_VERSION}`);
    });

    it('is offered to system administration through the operations hub', () => {
        const html = renderRoute('/operations', sessionWith({ mode: 'system-administration' }));

        expect(html).toContain('href="/operations/services"');
    });

    it('offers no link to the screens that are not built', () => {
        const html = renderRoute('/operations', sessionWith({ mode: 'system-administration' }));

        for (const route of ['/operations/bus', '/operations/logs']) {
            expect(html).not.toContain(`href="${route}"`);
        }
    });
});

describe('the compute grid screen', () => {
    const QUIET_NODE = gridNode({
        host_id: 'ad55e110-0000-4000-8000-000000000002',
        host: 'grid-05.example.com',
        instance_id: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b',
        state: 'lost',
        version: 'v0.0.24',
        tasks_completed: 101,
        tasks_since_last: 0,
        avg_task_duration_ms: 0,
        max_task_duration_ms: 0,
        input_bytes_fetched: 41_943_040,
        output_bytes_uploaded: 2_097_152,
        seconds_since_hb: 11_520,
    });
    const NAMELESS_NODE = gridNode({
        host_id: 'c30b9d47-0000-4000-8000-000000000003',
        host: null,
        instance_id: null,
        state: 'missing',
        version: null,
        tasks_completed: 57,
        tasks_since_last: 2,
    });

    it('states the counters with the sample time beside them', () => {
        const html = withProviders(<GridPage />);

        expect(html).toContain('Operations: compute grid');
        // The counters mirror one stored sample, so the sample time sits beside
        // them, labelled UTC.
        expect(html).toContain('sampled 14:31:02 UTC');
        expect(html).toContain('>6<');
        expect(html).toContain('Online 5');
        expect(html).toContain('Idle 2');
        expect(html).toContain('128 workunits · 9 batches');
        expect(html).toContain('Active 3');
        expect(html).toContain('412 success');
        expect(html).toContain('3 client error');
        expect(html).toContain('1 no reply');
    });

    it('says the counters are one tenant’s rather than every tenant’s work', () => {
        const html = withProviders(<GridPage />);

        // The summary counters are narrower than the whole-grid node read.
        expect(html).toContain('These counters are computed for one tenant');
        expect(html).toContain('The node table below is the whole installation');
    });

    it('keeps a quiet node’s row, and shows its failures and its slowest task', () => {
        const html = withProviders(
            <GridPage />,
            '/',
            [],
            gridView({
                nodes: [gridNode({ tasks_failed: 4, max_task_duration_ms: 51_000 }), QUIET_NODE],
            }),
        );

        expect(html).toContain('2 rows');
        // The node samples the failures and the slowest task now, so a node
        // failing every task no longer reads as a node doing nothing.
        expect(html).toContain('Failed');
        expect(html).toContain('Slowest');
        expect(html).toContain('>4<');
        expect(html).toContain('51 s');
        expect(html).toContain('42 s');
        expect(html).toContain('1.20 GiB');
        expect(html).toContain('220 MiB');
        // A node that went quiet keeps its row, and its age is the state.
        expect(html).toContain('grid-05.example.com');
        expect(html).toContain('3 h 12 m');
    });

    it('says no host names a node rather than printing the id as a name', () => {
        const html = withProviders(<GridPage />, '/', [], gridView({ nodes: [NAMELESS_NODE] }));

        expect(html).toContain('c30b9d47-0000-4000-8000-000000000003');
        expect(html).toContain('no host names it');
    });

    it('carries each node’s runner on the node row, with the instance tail shown', () => {
        const html = withProviders(
            <GridPage />,
            '/',
            [],
            gridView({
                nodes: [
                    gridNode(),
                    gridNode({
                        host_id: '9e0f33aa-0000-4000-8000-000000000002',
                        host: 'grid-05.example.com',
                        instance_id: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b',
                        state: 'lost',
                        version: 'v0.0.24',
                    }),
                    NAMELESS_NODE,
                ],
            }),
        );

        // One table: the node count and the runner report share its header.
        expect(html).toContain('3 rows');
        expect(html).toContain('1 of 3 runners reported in the last 5 minutes');
        expect(html).toContain('1 lost');
        expect(html).toContain('1 missing');
        // The state words are the read's own, one per node.
        expect(html.match(/>running</g) ?? []).toHaveLength(1);
        expect(html.match(/>lost</g) ?? []).toHaveLength(1);
        expect(html.match(/>missing</g) ?? []).toHaveLength(1);
        // The instance identifier is shown by its tail, because UUIDv7 ids
        // start with the instant the process started, which agents launched
        // together share. The full value stays in the title.
        expect(html).toContain('title="1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6"');
        expect(html).toContain('>b3c4d5e6<');
        expect(html).not.toContain('>1a90fe12<');
        // The version travels with the runner, and the two agent attributes
        // share the row with the node's measurements.
        expect(html).toContain('v0.0.24');
        expect(html).toContain('grid-05.example.com');
    });

    it('warns about a running runner that trails the newest release', () => {
        const html = withProviders(
            <GridPage />,
            '/',
            [],
            gridView({
                nodes: [
                    gridNode(),
                    gridNode({
                        host_id: '9e0f33aa-0000-4000-8000-000000000002',
                        host: 'grid-02.example.com',
                        instance_id: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b',
                        version: 'v0.0.24',
                    }),
                ],
            }),
        );

        // A running runner below the newest release carries the warning; a lost
        // one is not compared, because a release from a process that stopped
        // says nothing about the deployment.
        expect(html).toContain('older build');
        expect(html.match(/older build/g) ?? []).toHaveLength(1);
    });

    it('states no sample rather than presenting zero hosts as the fleet', () => {
        const html = withProviders(
            <GridPage />,
            '/',
            [],
            gridView({ sampled_at: null, total_hosts: 0 }),
        );

        expect(html).toContain('No sample yet');
        // The node table stands alone.
        expect(html).toContain('grid-01.example.com');
        expect(html).not.toContain('Online 5');
    });

    it('states only the gaps the journey still records', () => {
        const html = withProviders(<GridPage />);

        expect(html).toContain('No history');
        expect(html).toContain('A node with no host row');
        expect(html).toContain('counters are one tenant');
        // Each gap names the journey that records it, as its lead promises.
        expect(html).toContain('Recorded by Watch the compute grid');
        // The whole-grid read, the host on the heartbeat, the failure fields
        // and the permission are all served now, so none of them may be stated
        // as open.
        for (const closed of [
            "The grid is the installation's and the read serves the whole grid",
            'A wrapper sits on its node',
            'The failures reach the screen',
            'The read checks a permission',
            'The read narrows a shared grid to one tenant',
            'The failures are stored and then dropped',
            'A wrapper cannot be placed on its node',
            'No permission gates the read',
            'listed beside the nodes rather than on them',
        ]) {
            expect(html).not.toContain(closed);
        }
    });

    it('reaches the related journeys, and names the ones with no screen yet', () => {
        const html = withProviders(<GridPage />);

        expect(html).toContain('href="/operations/services"');
        expect(html).toContain('href="/operations/versions"');
        expect(html).toContain('Watch the message bus');
        expect(html).toContain('Read the telemetry logs');
        // Two journeys keep their screen for a later unit: they are stated,
        // marked as unbuilt, and carry no link.
        expect(html.match(/Not built yet/g) ?? []).toHaveLength(2);
        for (const route of ['/operations/bus', '/operations/logs']) {
            expect(html).not.toContain(`href="${route}"`);
        }
    });

    it('states a sample time labelled UTC, and units a person reads', () => {
        expect(sampleTime('2026-10-04 14:31:02Z')).toBe('14:31:02 UTC');
        expect(sampleTime(null)).toBeUndefined();
        expect(sampleTime('')).toBeUndefined();
        expect(sampleTime('not a time')).toBeUndefined();
        expect(asGiB(1_288_490_188, t)).toBe('1.20 GiB');
        expect(asMiB(230_686_720, t)).toBe('220 MiB');
        expect(asSeconds(42_000, t)).toBe('42 s');
        expect(formatGridAge(8, t)).toBe('8 s');
        expect(formatGridAge(900, t)).toBe('15 m 0 s');
        expect(formatGridAge(11_520, t)).toBe('3 h 12 m');
    });

    it('takes the units from the catalogue rather than from English', () => {
        const french = createTranslator('fr', enFlat, frFlat).t;

        expect(asGiB(1_288_490_188, french)).toBe('1.20 Gio');
        expect(asMiB(230_686_720, french)).toBe('220 Mio');
        expect(formatGridAge(900, french)).toBe('15 min 0 s');
    });
});

describe('the route to the compute grid', () => {
    it('opens it, and renders through the shell', () => {
        const html = renderRoute('/operations/grid', sessionWith());

        expect(html).toContain('Operations: compute grid');
        expect(html).toContain('ORE Studio');
        expect(html).toContain('Your account');
        expect(html).toContain(`client ${__BUILD_VERSION__}`);
        expect(html).toContain(`server ${SESSION_VERSION}`);
    });

    it('is offered to system administration through the operations hub', () => {
        const html = renderRoute('/operations', sessionWith({ mode: 'system-administration' }));

        expect(html).toContain('href="/operations/grid"');
    });
});
