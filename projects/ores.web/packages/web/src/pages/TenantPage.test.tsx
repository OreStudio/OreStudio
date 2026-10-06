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
import { MemoryRouter, Route, Routes } from 'react-router';
import type { TenantDetailResponse } from '@ores/wire-protocol/browser';
import { ApiFailure } from '../api/transport.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { TenantPage } from './TenantPage.js';

const tenantParties = { key: 'tenant-parties', scope: 'acme_corporation' };
const tenantPeople = { key: 'tenant-people', scope: 'acme_corporation' };

/**
 * One tenant's screen, as a reader sees it.
 *
 * The read is seeded in the query cache, so the screen renders its loaded
 * state without a server. What is checked is the screen: the details, the
 * run, what each says when its part is missing, the way inside the tenant, and
 * the answer to a code no tenant holds.
 */

const RUN = '55555555-5555-5555-5555-555555555555';

const loaded: TenantDetailResponse = {
    tenant: {
        id: '44444444-4444-4444-4444-444444444444',
        code: 'acme_corporation',
        name: 'Acme Corporation',
        type: 'evaluation',
        description: 'Acme evaluation tenant',
        hostname: 'acme.example.com',
        status: 'active',
        registrationDefault: false,
        setup: null,
        version: 3,
        modifiedBy: 'admin',
        performedBy: 'ores.iam.service',
        changeReasonCode: 'system.initial_load',
        changeCommentary: '',
        recordedAt: '2026-10-02 21:39:00Z',
    },
    setupUnavailable: false,
};

function render(seed: (client: QueryClient) => void, code = 'acme_corporation', tab = ''): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    client.setQueryData(['tenant-statuses'], []);
    client.setQueryData(['tenant-types'], []);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter
                    initialEntries={[`/tenants/${code}${tab === '' ? '' : `?tab=${tab}`}`]}
                >
                    <Routes>
                        <Route path="/tenants/:code" element={<TenantPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function renderLoaded(detail: TenantDetailResponse): string {
    return render((client) => client.setQueryData(['tenant', 'acme_corporation'], detail));
}

describe('TenantPage', () => {
    it('shows the details and the last-changed line under the standard header', () => {
        const html = renderLoaded(loaded);

        expect(html).toContain('Acme Corporation');
        expect(html).toContain('acme_corporation · version 3');
        expect(html).toContain('acme.example.com');
        expect(html).toContain('Acme evaluation tenant');
        expect(html).toContain('Last changed');
        expect(html).toContain('admin for ores.iam.service');
        expect(html).toContain('system.initial_load');
        expect(html).toContain('href="/tenants"');
    });

    /*
     * The tenant's data is a tab like its details. Nothing offers to enter or
     * leave the tenant: the read inside it is the server's, for the tab.
     */
    it('offers the details, parties and people as tabs, read only, and no way in or out', () => {
        const html = renderLoaded(loaded);

        for (const tab of ['Details', 'Parties', 'People']) {
            expect(html).toContain(`>${tab}</button>`);
        }
        expect(html).toContain('aria-selected="true"');
        expect(html).toContain('Read only');
        expect(html).toContain('made by its own administrators');
        expect(html).not.toContain('>Edit<');
        expect(html.toLowerCase()).not.toContain('act in');
        expect(html.toLowerCase()).not.toContain('leave');
    });

    /*
     * The system tenant is the system administrator's own, so nothing on its
     * screen says another tenant's administrators make the changes.
     */
    it('opens the system tenant with its tabs, without the note about its administrators', () => {
        const html = render(
            (client) =>
                client.setQueryData(['tenant', 'system'], {
                    ...loaded,
                    tenant: {
                        ...loaded.tenant,
                        id: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
                        code: 'system',
                        name: 'System',
                        type: 'system',
                    },
                }),
            'system',
        );

        expect(html).toContain('>People</button>');
        expect(html).not.toContain('made by its own administrators');
        expect(html).not.toContain('Delete');
    });

    it('offers to delete the tenant from its own screen', () => {
        const html = renderLoaded(loaded);

        expect(html.match(/>Delete</g)).toHaveLength(1);
    });

    it("lists the tenant's parties on the parties tab, each with the party it belongs to", () => {
        const html = render(
            (client) => {
                client.setQueryData(['tenant', 'acme_corporation'], loaded);
                client.setQueryData(pageKey(tenantParties, FIRST_PAGE), {
                    rows: [
                        {
                            id: '77777777-7777-7777-7777-777777777777',
                            code: 'ACMCOR',
                            name: 'Acme Corporation Plc',
                            category: 'Operational',
                            type: 'Corporate',
                            status: 'active',
                            parentId: null,
                            parentName: null,
                            businessCentreCode: 'GBLO',
                            flagImageId: '99999999-9999-9999-9999-999999999991',
                        },
                        {
                            id: '88888888-8888-8888-8888-888888888888',
                            code: 'ACCOHK',
                            name: 'Acme Corporation HK Ltd',
                            category: 'Operational',
                            type: 'Corporate',
                            status: 'active',
                            parentId: '77777777-7777-7777-7777-777777777777',
                            parentName: 'Acme Corporation Plc',
                            businessCentreCode: 'HKHK',
                            flagImageId: null,
                        },
                    ],
                    total: 2,
                });
            },
            'acme_corporation',
            'parties',
        );

        expect(html).toContain('Acme Corporation HK Ltd');
        expect(html).toContain('Top of the group');
        expect(html).toMatch(/>Acme Corporation Plc<\/td>/);
        expect(html).not.toContain('Last changed');
        // The flag is read inside the tenant; a centre with no flag keeps its code.
        expect(html).toContain(
            'src="/api/tenants/acme_corporation/images/99999999-9999-9999-9999-999999999991"',
        );
        expect(html.match(/<img/g)).toHaveLength(1);
        expect(html).toContain('HKHK');
    });

    it('lists the people on the people tab', () => {
        const html = render(
            (client) => {
                client.setQueryData(['tenant', 'acme_corporation'], loaded);
                client.setQueryData(pageKey(tenantPeople, FIRST_PAGE), {
                    rows: [
                        {
                            version: 1,
                            id: '99999999-9999-9999-9999-999999999999',
                            tenantId: '44444444-4444-4444-4444-444444444444',
                            username: 'priya',
                            fullName: 'Priya Natarajan',
                            email: 'priya@acme.example',
                            accountType: 'user',
                            jobTitle: '',
                            reportsToAccountId: null,
                            defaultPartyId: null,
                            imageId: '99999999-9999-9999-9999-999999999992',
                        },
                        {
                            version: 1,
                            id: '99999999-9999-9999-9999-999999999998',
                            tenantId: '44444444-4444-4444-4444-444444444444',
                            username: 'tom.reed',
                            fullName: '',
                            email: 'tom@acme.example',
                            accountType: 'user',
                            jobTitle: '',
                            reportsToAccountId: null,
                            defaultPartyId: null,
                            imageId: null,
                        },
                    ],
                    total: 2,
                });
            },
            'acme_corporation',
            'people',
        );

        expect(html).toContain('Priya Natarajan');
        expect(html).toContain('priya@acme.example');
        // A person with a picture shows it; one without shows their initials.
        expect(html).toContain(
            'src="/api/tenants/acme_corporation/images/99999999-9999-9999-9999-999999999992"',
        );
        expect(html.match(/<img/g)).toHaveLength(1);
        expect(html).toContain('>TR</span>');
        expect(html).toContain('tabindex="0"');
    });

    /*
     * An unfinished run is the one a person came back for, so the screen
     * names where it stopped and leads to the run's page.
     */
    it('leads to an unfinished run and shows the error it stopped on', () => {
        const html = renderLoaded({
            ...loaded,
            tenant: {
                ...loaded.tenant,
                setup: {
                    instanceId: RUN,
                    status: 'failed',
                    currentStepIndex: 3,
                    stepCount: 7,
                    error: 'Seeding failed.',
                },
            },
        });

        expect(html).toContain('Failed at step 4');
        expect(html).toContain('Seeding failed.');
        expect(html).toContain(`href="/tenants/runs/${RUN}"`);
        expect(html).toContain('Resume setup');
    });

    it('says the run completed, or that none is on record', () => {
        const completed = renderLoaded({
            ...loaded,
            tenant: {
                ...loaded.tenant,
                setup: {
                    instanceId: RUN,
                    status: 'completed',
                    currentStepIndex: 6,
                    stepCount: 7,
                    error: '',
                },
            },
        });

        expect(completed).toContain('The setup run completed.');
        expect(completed).not.toContain('Resume setup');
        expect(renderLoaded(loaded)).toContain('No setup run is on record for this tenant.');
    });

    it('says the run could not be read, and still shows the tenant', () => {
        const html = renderLoaded({ ...loaded, setupUnavailable: true });

        expect(html).toContain('Acme Corporation');
        expect(html).toContain('The provisioning runs could not be read.');
    });

    it('goes back to the tenant list when no tenant has the code', () => {
        const html = render((client) => {
            client
                .getQueryCache()
                .build(client, { queryKey: ['tenant', 'nobody'] })
                .setState({
                    status: 'error',
                    error: new ApiFailure(404, {
                        code: 'not-found',
                        message: 'No tenant has this code.',
                    }),
                });
        }, 'nobody');

        // The redirect runs after the first render, so a static render draws neither an error nor a page.
        expect(html).toBe('');
    });
});
