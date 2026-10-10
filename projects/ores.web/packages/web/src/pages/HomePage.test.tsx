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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type {
    Account,
    BusView,
    DeploymentOverview,
    GridView,
    ServiceRosterRow,
    SessionMode,
    TenantSummary,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import {
    DEFAULT_HEALTH_RANGE,
    SERVICES_QUERY_KEY,
    logCountKey,
} from '../operations/InstallationHealth.js';
import { BUS_QUERY_KEY } from '../operations/BusPage.js';
import { GRID_QUERY_KEY } from '../operations/GridPage.js';
import { HomePage } from './HomePage.js';

/**
 * Home, per mode.
 *
 * The answers are seeded into the query cache rather than stubbed, because
 * what is checked is the screen: the state it states in words, the actions it
 * offers, and that no mode names how the screens were designed.
 */

function tenant(code: string, name: string, status: string): TenantSummary {
    return {
        id: `${code}-0000-4000-8000-000000000000`,
        code,
        name,
        type: 'production',
        description: '',
        hostname: `${code}.example`,
        status,
        registrationDefault: false,
        setup: null,
    };
}

const acme = tenant('acme', 'Acme Corporation', 'active');
const globex = {
    ...tenant('globex', 'Globex Markets', 'bootstrapping'),
    setup: {
        instanceId: 'run-globex',
        status: 'failed',
        stepsDone: 4,
        stepCount: 7,
        error: 'Publishing failed.',
    },
};
const initech = tenant('initech', 'Initech Capital', 'suspended');

const busy: DeploymentOverview = {
    inService: 3,
    onEvaluation: 1,
    settingUp: 1,
    attention: [
        { tenant: globex, reason: 'setup-failed' },
        { tenant: initech, reason: 'suspended' },
    ],
    tenants: [acme, globex],
    totalCount: 9,
    activity: [
        {
            instanceId: 'run-acme',
            tenantName: 'Acme Corporation',
            status: 'completed',
            stepsDone: 7,
            stepCount: 7,
            error: '',
            at: '2026-10-04T09:05:00Z',
        },
    ],
    activityUnavailable: false,
};

const quiet: DeploymentOverview = {
    ...busy,
    attention: [],
    activity: [],
    activityUnavailable: true,
};

/** The signed-in person's own account, as the wiring reads it. */
const signedInAccount: Account = {
    version: 1,
    id: '11111111-1111-4111-8111-111111111111',
    tenantId: '22222222-2222-4222-8222-222222222222',
    username: 'marco',
    fullName: 'Marco Craveiro',
    email: 'marco@example.com',
    accountType: 'user',
    jobTitle: 'Head of Desk',
    reportsToAccountId: null,
    defaultPartyId: null,
    imageId: null,
    modifiedBy: 'marco',
    changeReasonCode: 'common.non_material_update',
    changeCommentary: '',
    performedBy: 'marco',
    recordedAt: '2026-10-05 09:30:00Z',
};

function home(
    mode: SessionMode,
    overview?: DeploymentOverview,
    self?: Account | null,
    seed?: (client: QueryClient) => void,
    path = '/',
): string {
    const client = new QueryClient();
    if (overview !== undefined) {
        client.setQueryData(['overview'], overview);
        client.setQueryData(['tenant-types'], []);
        client.setQueryData(['tenant-statuses'], []);
    }
    seed?.(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <HomePage
                        username="marco"
                        email="marco@example.com"
                        tenantName="Acme Corporation"
                        partyName="Acme London"
                        mode={mode}
                        {...(self !== undefined && { self })}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('every home', () => {
    it('never names a journey', () => {
        for (const html of [
            home('system-administration', busy),
            home('tenant-administration'),
            home('application'),
        ]) {
            expect(html.toLowerCase()).not.toContain('journey');
        }
    });
});

/** A dashboard on which every panel is fine: the quiet overview and every other read healthy. */
function seedHealthy(client: QueryClient): void {
    client.setQueryData(SERVICES_QUERY_KEY, [
        serviceRow('ores.iam.service', 'running'),
        serviceRow('ores.web.service', 'running'),
    ]);
    client.setQueryData(logCountKey('error', DEFAULT_HEALTH_RANGE), 0);
    client.setQueryData(logCountKey('warn', DEFAULT_HEALTH_RANGE), 0);
    client.setQueryData(GRID_QUERY_KEY, gridView({ total_hosts: 5, online_hosts: 5 }));
    client.setQueryData([BUS_QUERY_KEY, '15m'], busView({ slow_consumers: 0 }));
}

function serviceRow(name: string, state: 'running' | 'lost' | 'missing'): ServiceRosterRow {
    return {
        service_name: name,
        display_name: name,
        description: '',
        service_account: null,
        slot: 1,
        state,
        instance_id: state === 'missing' ? null : 'instance',
        host_id: null,
        version: state === 'missing' ? null : '0.0.27',
        sampled_at: null,
        age_seconds: null,
    };
}

function gridView(overrides: Partial<GridView> = {}): GridView {
    return {
        sampled_at: '2026-10-10 12:00:00Z',
        total_hosts: 0,
        online_hosts: 0,
        idle_hosts: 0,
        total_workunits: 0,
        total_batches: 0,
        active_batches: 0,
        outcomes_success: 0,
        outcomes_client_error: 0,
        outcomes_no_reply: 0,
        nodes: [],
        ...overrides,
    };
}

function busView(newest: { readonly slow_consumers: number } | null): BusView {
    return {
        sampled_at: newest === null ? null : '2026-10-10 12:00:00Z',
        samples:
            newest === null
                ? []
                : [
                      {
                          sampled_at: '2026-10-10 12:00:00Z',
                          in_msgs: 1,
                          out_msgs: 1,
                          in_bytes: 1,
                          out_bytes: 1,
                          connections: 24,
                          mem_bytes: 1,
                          slow_consumers: newest.slow_consumers,
                      },
                  ],
        streams: [],
    };
}

const ALL_CLEAR = 'Everything is running';

function occurrences(html: string, text: string): number {
    return html.split(text).length - 1;
}

describe("the system administrator's home", () => {
    it('welcomes the person and no longer says how many tenants there are', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        expect(html).toContain('Welcome, marco');
        expect(html).not.toContain('This deployment runs');
    });

    it('has the tabs Dashboard, Active modules and Upcoming modules, on the Dashboard first', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        const order = ['Dashboard', 'Active modules', 'Upcoming modules'].map((title) =>
            html.indexOf(`>${title}<`),
        );
        expect(order.every((position) => position > 0)).toBe(true);
        expect([...order].sort((a, b) => a - b)).toEqual(order);
        expect(html).toMatch(/aria-selected="true"[^>]*>Dashboard</);
        expect(html).not.toContain('href="/people"');
    });

    it('has a Tenants, a Services, a Grid and a Message queue panel on the Dashboard', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        for (const title of ['Tenants', 'Services', 'Grid', 'Message queue']) {
            expect(html).toContain(`>${title}</h2>`);
        }
    });

    it('says Everything is running in every panel, and once more for the whole installation', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        expect(occurrences(html, ALL_CLEAR)).toBe(5);
    });

    it('spends no line on a panel that is fine: its mark says so, and only the top status says it in words', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        expect(occurrences(html, `>${ALL_CLEAR}<`)).toBe(1);
        expect(occurrences(html, `aria-label="${ALL_CLEAR}"`)).toBe(4);
    });

    it('counts the areas that need the person in the one status at the top', () => {
        const html = home('system-administration', busy, undefined, (client) => {
            seedHealthy(client);
            client.setQueryData(SERVICES_QUERY_KEY, [
                serviceRow('ores.iam.service', 'running'),
                serviceRow('ores.dq.service', 'lost'),
            ]);
            client.setQueryData(GRID_QUERY_KEY, gridView({ total_hosts: 5, online_hosts: 4 }));
            client.setQueryData([BUS_QUERY_KEY, '15m'], busView({ slow_consumers: 2 }));
        });

        expect(html).toContain('4 areas need attention');
    });

    it('says nothing at the top until every panel has read, and the panel that has read says so alone', () => {
        const html = home('system-administration', quiet);

        expect(html).not.toContain('area needs attention');
        expect(html).not.toContain('areas need attention');
        expect(occurrences(html, ALL_CLEAR)).toBe(1);
    });

    it('closes the Services panel with the newest release, and the others with when they were read', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        expect(html).toContain('>v0.0.27<');
        expect(occurrences(html, '>Sampled<')).toBe(2);
        expect(html).toContain('12:00:00 UTC');
    });

    it('says how many need attention, in the same words, in the panel that does', () => {
        const html = home('system-administration', busy, undefined, (client) => {
            seedHealthy(client);
            client.setQueryData(SERVICES_QUERY_KEY, [
                serviceRow('ores.iam.service', 'running'),
                serviceRow('ores.dq.service', 'lost'),
            ]);
            client.setQueryData(GRID_QUERY_KEY, gridView({ total_hosts: 5, online_hosts: 4 }));
            client.setQueryData([BUS_QUERY_KEY, '15m'], busView({ slow_consumers: 2 }));
        });

        expect(html).toContain('2 tenants need attention');
        expect(html).toContain('1 service needs attention');
        expect(html).toContain('1 node needs attention');
        expect(html).toContain('2 slow consumers need attention');
        expect(html).not.toContain(ALL_CLEAR);
    });

    it('does not say Everything is running over data a failed refresh left behind', () => {
        const html = home('system-administration', quiet, undefined, (client) => {
            seedHealthy(client);
            const query = client.getQueryCache().build(client, { queryKey: ['overview'] });
            query.setState({
                ...query.state,
                status: 'error',
                error: new Error('the overview could not be read'),
                fetchStatus: 'idle',
            });
        });

        expect(occurrences(html, ALL_CLEAR)).toBe(3);
        expect(html).toContain('Could not be read');
        expect(html).not.toContain('area needs attention');
    });

    it('says so plainly when a panel has nothing to report yet', () => {
        const html = home('system-administration', quiet, undefined, (client) => {
            client.setQueryData(GRID_QUERY_KEY, gridView());
            client.setQueryData([BUS_QUERY_KEY, '15m'], busView(null));
        });

        expect(html).toContain('No node has reported yet');
        expect(html).toContain('No sample yet');
    });

    it('leads each panel to its screen', () => {
        const html = home('system-administration', quiet, undefined, seedHealthy);

        for (const href of [
            '/tenants',
            '/operations/services',
            '/operations/grid',
            '/operations/bus',
        ]) {
            expect(html).toContain(`href="${href}"`);
        }
    });

    it('states the services running, the errors and the warnings in the Services panel', () => {
        const html = home('system-administration', quiet, undefined, (client) => {
            seedHealthy(client);
            client.setQueryData(logCountKey('error', DEFAULT_HEALTH_RANGE), 7);
            client.setQueryData(logCountKey('warn', DEFAULT_HEALTH_RANGE), 31);
        });

        expect(html).toMatch(/>2 of 2<\/span><span[^>]*>Services running</);
        expect(html).toMatch(/>7<\/span><span[^>]*>Errors, Last hour</);
        expect(html).toMatch(/>31<\/span><span[^>]*>Warnings, Last hour</);
    });

    it('does not repeat the tenants heading and its sentence above the table', () => {
        const html = home('system-administration', busy, undefined, seedHealthy);

        expect(html).not.toContain('The organisations this deployment serves');
        expect(occurrences(html, '>Tenants</h2>')).toBe(1);
    });

    it('states the counts and leads a failed setup to its run and a suspended tenant to its screen', () => {
        const html = home('system-administration', busy, undefined, seedHealthy);

        expect(html).toContain('>3<');
        expect(html).toContain('In service');
        expect(html).toContain('Setup stopped after 4 of 7 steps.');
        expect(html).toContain('href="/tenants/runs/run-globex"');
        expect(html).toContain('Suspended: nobody in it can sign in.');
        expect(html).toContain('href="/tenants/initech"');
    });

    it('names each setup by its tenant, and says so when the setups cannot be read', () => {
        expect(home('system-administration', busy, undefined, seedHealthy)).toContain(
            'Acme Corporation finished setting up',
        );
        expect(home('system-administration', quiet, undefined, seedHealthy)).toContain(
            'The setup history could not be read.',
        );
    });

    it('lists the first tenants, with a way to all of them', () => {
        const html = home('system-administration', busy, undefined, seedHealthy);

        expect(html).toContain('href="/tenants/acme"');
        expect(html).toContain('Showing 2 of 9 tenants');
        expect(html).toContain('>See all<');
    });

    it('shows the panels before anything has been read, saying they are loading', () => {
        const html = home('system-administration');

        expect(html).toContain('>Message queue</h2>');
        expect(html).toContain(enFlat['common.loading']);
        expect(html).not.toContain(ALL_CLEAR);
    });

    it('offers four cards on the Active modules tab and reaches the operations screens through one', () => {
        const html = home('system-administration', quiet, undefined, undefined, '/?tab=active');

        for (const href of ['/tenants', '/people', '/operations', '/security']) {
            expect(html).toContain(`href="${href}"`);
        }
        expect(html.match(/href="/g)).toHaveLength(4);
        expect(html).not.toContain('href="/operations/');
        expect(html).not.toContain('System health');
        expect(html).not.toContain('href="/tenants/new"');
        expect(html).not.toContain('Add tenant');
    });

    it('marks the journeys with no screen on the Upcoming modules tab, without a link', () => {
        const html = home('system-administration', quiet, undefined, undefined, '/?tab=upcoming');

        for (const title of [
            'Market data feeds',
            'Yield curve process types',
            'What the grid runs',
        ]) {
            expect(html).toContain(title);
        }
        expect(html).toContain('Coming later');
        expect(html).not.toContain('href=');
    });

    it('shows words and never a translation key on any tab', () => {
        for (const path of ['/', '/?tab=active', '/?tab=upcoming']) {
            const html = home('system-administration', busy, undefined, seedHealthy, path);

            expect(html).not.toMatch(/\b(home|shell|operations)\.[a-zA-Z]+\.?[a-zA-Z]*/);
        }
    });

    it('is not shown to a tenant administrator or a member', () => {
        const label = enFlat['operations.overview.running'] ?? '';

        expect(label).not.toBe('');
        expect(home('tenant-administration')).not.toContain(label);
        expect(home('application')).not.toContain(label);
    });
});

describe('the greeting', () => {
    it("shows the name the signed-in person's own account holds", () => {
        expect(home('system-administration', busy, signedInAccount)).toContain(
            'Welcome, Marco Craveiro',
        );
    });

    it('falls back to the username when the account holds no name', () => {
        expect(home('application', undefined, { ...signedInAccount, fullName: '' })).toContain(
            'Welcome, marco',
        );
    });

    it('falls back to the username until the shell has read the account', () => {
        expect(home('system-administration', busy, null)).toContain('Welcome, marco');
    });
});

describe("the tenant administrator's home", () => {
    it("offers the tenant's own screens and no tenant management", () => {
        const html = home('tenant-administration');

        expect(html).toContain('Acme Corporation');
        for (const href of ['/parties', '/parties/new', '/rescue', '/audit', '/security']) {
            expect(html).toContain(`href="${href}"`);
        }
        expect(html).not.toContain('href="/tenants');
    });
});

describe("a party user's home", () => {
    it('says where they work, offers their own screens, and marks what is coming', () => {
        const html = home('application');

        expect(html).toContain('Working for Acme London');
        expect(html).toContain('href="/security"');
        expect(html).toContain('Trading');
        expect(html).toContain('Coming later');
        expect(html).not.toContain('href="/tenants');
    });

    it('offers the audit card, as the tenant home does', () => {
        const html = home('application');

        expect(html).toContain('href="/audit"');
        expect(html).toContain('Sign-ins');
    });
});
