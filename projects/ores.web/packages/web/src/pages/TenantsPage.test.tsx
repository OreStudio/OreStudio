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
import { TenantsPage } from './TenantsPage.js';

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
};

/** The badge the catalogue maps `active` to, as the BFF answers it. */
const activeBadge = {
    code: 'active',
    label: 'Active',
    description: 'Record is active and operational.',
    backgroundColour: '#22c55e',
    textColour: '#ffffff',
    severity: 'success',
};

function render(
    tenants: readonly TenantSummary[],
    totalCount: number,
    badges: Readonly<Record<string, unknown>> = { active: activeBadge },
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['tenants'], { tenants, totalCount });
    client.setQueryData(['badges', 'tenant_status'], { domain: 'tenant_status', badges });
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
        expect(html).toContain('1 tenant');
    });

    it('names the columns the journey document states', () => {
        const html = render([acme], 1);

        for (const column of ['Code', 'Name', 'Hostname', 'Type', 'Status']) {
            expect(html).toContain(column);
        }
    });

    /*
     * The status is painted by the platform's badge catalogue, not by a table on
     * this screen: the label, the two colours and the words behind it all come
     * from the answer, so two screens showing the same status agree.
     */
    it('paints the status with the badge the catalogue maps it to', () => {
        const html = render([acme], 1);

        expect(html).toContain('background-color:#22c55e');
        expect(html).toContain('color:#ffffff');
        expect(html).toContain('title="Record is active and operational."');
        expect(html).toContain('>Active<');
    });

    /*
     * A value with no badge is shown as the server wrote it. A tenant in a state
     * nobody has mapped is the row somebody needs to see, so swallowing it would
     * hide the interesting case.
     */
    it('shows a status the catalogue does not hold as it was written', () => {
        const html = render([{ ...acme, status: 'quarantined' }], 1, {});

        expect(html).toContain('quarantined');
    });

    /*
     * The table is the page. A card around it, carrying the title and the count
     * the header already states, is a box in a box, and the second box only
     * makes the table narrower than the screen it was given.
     */
    it('draws the table as the page, not inside a card', () => {
        expect(render([acme], 1)).not.toContain('class="card');
    });

    it('offers the journey that creates a tenant from the header, at any row count', () => {
        for (const html of [render([acme], 1), render([], 0)]) {
            expect(html).toContain('href="/tenants/new"');
            expect(html).toContain('New tenant');
        }
    });

    /*
     * A deployment whose administrator exists and whose first tenant does not
     * is a normal state, not a failure. The state is stated once and the way
     * out is the header's action, not a second button beneath it.
     */
    it('says so when the deployment holds no tenant, without repeating the action', () => {
        const html = render([], 0);

        expect(html).toContain('no tenant of its own');
        expect(html.match(/href="\/tenants\/new"/g)).toHaveLength(1);
    });
});
