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
import type { TenantSummary } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { TenantsPage, tenantListPage, tenantQuery } from './TenantsPage.js';

/**
 * The roster, as a reader sees it.
 *
 * The read is seeded rather than stubbed, because what is being checked is the
 * screen: the columns it names, the state it states in words, and what it says
 * to a deployment that holds no tenant yet. The query cache is where the answer
 * goes, so the screen renders its loaded state without a server.
 */

const acme: TenantSummary = {
    id: '44444444-4444-4444-4444-444444444444',
    code: 'acme_corporation',
    name: 'Acme Corporation',
    type: 'operational',
    description: '',
    hostname: 'acme_corporation',
    status: 'active',
    registrationDefault: false,
    setup: null,
};

const RUN = '55555555-5555-5555-5555-555555555555';

/** The active status as the BFF answers it: its own words, the badge's colours. */
const activeStatus = {
    code: 'active',
    name: 'Active',
    description: 'Tenant is active and fully operational',
    badge: {
        code: 'active',
        label: 'Active',
        description: 'Record is active and operational.',
        backgroundColour: '#22c55e',
        textColour: '#ffffff',
        severity: 'success',
    },
};

/** The types as the BFF answers them: their own words, each with its badge. */
const tenantTypes = [
    {
        code: 'system',
        name: 'System',
        description: '',
        badge: null,
    },
    {
        code: 'operational',
        name: 'Operational',
        description: 'A tenant in use.',
        badge: {
            code: 'tenant_type_operational',
            label: 'Operational',
            description: '',
            backgroundColour: '#2563eb',
            textColour: '#ffffff',
            severity: 'info',
        },
    },
    {
        code: 'automation',
        name: 'Automation',
        description: 'Automated test infrastructure.',
        badge: {
            code: 'tenant_type_automation',
            label: 'Automation',
            description: '',
            backgroundColour: '#9ca3af',
            textColour: '#ffffff',
            severity: 'secondary',
        },
    },
];

function render(
    tenants: readonly TenantSummary[],
    totalCount: number,
    statuses: readonly unknown[] = [activeStatus],
    setupUnavailable = false,
    hiddenTestCount = 0,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(pageKey({ key: 'tenants' }, FIRST_PAGE), {
        rows: tenants,
        total: totalCount,
        notes: [
            ...(setupUnavailable
                ? [{ tone: 'warn', text: 'The provisioning runs could not be read.' }]
                : []),
            ...(hiddenTestCount > 0
                ? [{ tone: 'info', text: `${hiddenTestCount} test tenants hidden` }]
                : []),
        ],
    });
    client.setQueryData(['tenant-statuses'], statuses);
    client.setQueryData(['tenant-types'], tenantTypes);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <TenantsPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the tenant roster', () => {
    it('names each tenant and the state it is in', () => {
        const html = render([acme], 1);

        expect(html).toContain('acme_corporation');
        expect(html).toContain('Acme Corporation');
        expect(html).toContain('operational');
        expect(html).toContain('Active');
        expect(html).toContain('1–1 of 1');
    });

    it('names the columns the journey document states', () => {
        const html = render([acme], 1);

        for (const column of ['Code', 'Name', 'Hostname', 'Type', 'Status', 'Setup']) {
            expect(html).toContain(column);
        }
    });

    /*
     * The words are the status row's own and the colours are its badge's. The
     * screen decides neither, so a status reads the same wherever it appears and
     * the badge catalogue cannot replace the deployment's vocabulary.
     */
    it('paints the status with its own name and its badge colours', () => {
        const html = render([acme], 1);

        expect(html).toContain('background-color:#22c55e');
        expect(html).toContain('color:#ffffff');
        expect(html).toContain('title="Tenant is active and fully operational"');
        expect(html).toContain('>Active<');
    });

    /*
     * A deployment whose status row reads `Suspended` shows the word
     * `Suspended`, whatever the badge that paints it is called.
     */
    it('keeps the word the status row carries, not the badge name', () => {
        const suspended = {
            code: 'suspended',
            name: 'Suspended',
            description: 'Tenant is temporarily suspended - users cannot log in',
            badge: {
                code: 'frozen',
                label: 'Frozen',
                description: 'Record is frozen; no changes permitted.',
                backgroundColour: '#eab308',
                textColour: '#ffffff',
                severity: 'warning',
            },
        };
        const html = render([{ ...acme, status: 'suspended' }], 1, [suspended]);

        expect(html).toContain('>Suspended<');
        expect(html).not.toContain('>Frozen<');
    });

    /*
     * A status the deployment does not hold is shown as the server wrote it. A
     * tenant in a state nobody has described is the row somebody needs to see,
     * so swallowing it would hide the interesting case.
     */
    it('shows a status the deployment does not hold as it was written', () => {
        const html = render([{ ...acme, status: 'quarantined' }], 1, []);

        expect(html).toContain('quarantined');
    });

    /*
     * A status whose badge has left the catalogue keeps its words and loses its
     * colours, which is worse to look at and better than not being readable.
     */
    it('shows the words when the badge has gone', () => {
        const unpainted = { ...activeStatus, code: 'retired', name: 'Retired', badge: null };
        const html = render([{ ...acme, status: 'retired' }], 1, [unpainted]);

        expect(html).toContain('Retired');
    });

    /*
     * The table is the page. A card around it, carrying the title and the count
     * the header already states, is a box in a box, and the second box only
     * makes the table narrower than the screen it was given.
     */
    it('draws the table as the page, not inside a card', () => {
        expect(render([acme], 1)).not.toContain('class="card');
    });

    it('offers to add a tenant from the header, at any row count', () => {
        for (const html of [render([acme], 1), render([], 0)]) {
            expect(html).toContain('Add tenant');
        }
    });

    /*
     * The way back into a journey somebody left is the tenant it acts on. A
     * run still working says how far it has got, counting steps from one as a
     * person does, and links to its rail.
     */
    it('links a tenant whose run is still working to that run', () => {
        const html = render(
            [
                {
                    ...acme,
                    status: 'provisioning',
                    setup: {
                        instanceId: RUN,
                        status: 'in_progress',
                        currentStepIndex: 2,
                        stepCount: 7,
                        error: '',
                    },
                },
            ],
            1,
        );

        expect(html).toContain(`href="/tenants/runs/${RUN}"`);
        expect(html).toContain('Step 3 of 7');
    });

    /*
     * A failed run is the case that most needs a way back, so it is shown as
     * failed and carries its error, rather than disappearing from the roster.
     */
    it('shows a failed run as failed, with its error', () => {
        const html = render(
            [
                {
                    ...acme,
                    setup: {
                        instanceId: RUN,
                        status: 'failed',
                        currentStepIndex: 3,
                        stepCount: 7,
                        error: 'Seeding failed.',
                    },
                },
            ],
            1,
        );

        expect(html).toContain('Failed at step 4');
        expect(html).toContain('title="Seeding failed."');
        expect(html).toContain(`href="/tenants/runs/${RUN}"`);
    });

    it('says nothing about a run that completed', () => {
        const html = render(
            [
                {
                    ...acme,
                    setup: {
                        instanceId: RUN,
                        status: 'completed',
                        currentStepIndex: 6,
                        stepCount: 7,
                        error: '',
                    },
                },
            ],
            1,
        );

        expect(html).not.toContain('/tenants/runs/');
    });

    it('says the runs are missing rather than that there are none', () => {
        const html = render([acme], 1, [activeStatus], true);

        expect(html).toContain('The provisioning runs could not be read.');
    });

    it('offers a search over the tenants on the server', () => {
        const html = render([acme], 1);

        expect(html).toContain('type="search"');
    });

    /*
     * The count is the server's: every tenant that matches, not the rows on
     * this page. The first page of many offers the next page and not the one
     * before it.
     */
    it('counts every match and pages from the first page', () => {
        const html = render([acme], 60);

        expect(html).toContain('1–1 of 60');
        const previous = html.match(/<button[^>]*>Previous<\/button>/)?.[0] ?? '';
        const next = html.match(/<button[^>]*>Next<\/button>/)?.[0] ?? '';
        expect(previous).toContain('disabled=""');
        expect(next).not.toContain('disabled=""');
    });

    it('offers no next page when every match is shown', () => {
        const html = render([acme], 1);

        const next = html.match(/<button[^>]*>Next<\/button>/)?.[0] ?? '';
        expect(next).toContain('disabled=""');
    });

    /*
     * The type is painted like the status: its own words, the badge's colours.
     */
    it('paints the type with its own name and its badge colours', () => {
        const html = render([acme], 1);

        expect(html).toContain('background-color:#2563eb');
        expect(html).toContain('>Operational<');
    });

    it('offers a filter for each type the roster can show, and each status', () => {
        const html = render([acme], 1);

        expect(html).toContain('All types');
        expect(html).toContain('<option value="operational">Operational</option>');
        expect(html).toContain('<option value="automation">Automation</option>');
        // The system tenant is listed, so its type is offered too.
        expect(html).toContain('<option value="system">System</option>');
        expect(html).toContain('All statuses');
        expect(html).toContain('<option value="active">Active</option>');
    });

    /*
     * Test infrastructure is hidden by default, and the screen says how much
     * it hid, so a person who wants it knows it is there.
     */
    it('says how many test tenants it hides, above the table, beside the toggle that shows them', () => {
        const html = render([acme], 1, [activeStatus], false, 12);

        expect(html).toContain('Show test tenants');
        expect(html).toContain('12 test tenants hidden');
    });

    it('offers the toggle to a deployment that holds only test tenants', () => {
        const html = render([], 0, [activeStatus], false, 3);

        expect(html).toContain('Show test tenants');
        expect(html).toContain('3 test tenants hidden');
    });

    /*
     * A row opens the tenant's page, which is where setup is resumed and the
     * tenant is deleted, so the row carries no menu of its own.
     */
    it('opens the tenant from its row, and offers no row menu', () => {
        const html = render([acme], 1);

        expect(html).toContain('tabindex="0"');
        expect(html).not.toContain('Actions for');
        expect(html).not.toContain('Remove tenant');
    });

    it('says so when the deployment holds no tenant, and offers Add', () => {
        const html = render([], 0);

        expect(html).toContain('There are no tenants yet.');
        expect(html.match(/Add tenant/g)).toHaveLength(2);
    });
});

describe('the tenant roster read', () => {
    it('sends the search, the filters and the bounds of the page', () => {
        expect(
            tenantQuery({
                ...FIRST_PAGE,
                offset: 30,
                search: 'acme',
                filters: { type: 'operational', status: 'active', test: '1' },
            }),
        ).toEqual({
            search: 'acme',
            type: 'operational',
            status: 'active',
            includeTest: true,
            offset: 30,
            limit: 15,
        });
        expect(tenantQuery(FIRST_PAGE)).toMatchObject({ type: '', status: '', includeTest: false });
    });

    it('turns missing runs and hidden test tenants into notes, and says nothing otherwise', () => {
        const t = (key: string) => key;
        const plural = (key: string, count: number) => `${key}:${count}`;
        const quiet = {
            tenants: [acme],
            totalCount: 1,
            setupUnavailable: false,
            hiddenTestCount: 0,
        };

        expect(tenantListPage(quiet, t, plural)).toEqual({ rows: [acme], total: 1, notes: [] });
        expect(
            tenantListPage({ ...quiet, setupUnavailable: true, hiddenTestCount: 4 }, t, plural)
                .notes,
        ).toEqual([
            { tone: 'warn', text: 'tenants.setupUnavailable' },
            { tone: 'info', text: 'tenants.hiddenTest:4' },
        ]);
    });
});
