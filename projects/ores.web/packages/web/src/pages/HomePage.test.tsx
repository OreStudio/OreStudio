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
    DeploymentOverview,
    SessionMode,
    TenantSummary,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
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

function home(mode: SessionMode, overview?: DeploymentOverview, self?: Account | null): string {
    const client = new QueryClient();
    if (overview !== undefined) {
        client.setQueryData(['overview'], overview);
        client.setQueryData(['tenant-types'], []);
        client.setQueryData(['tenant-statuses'], []);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
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

describe("the system administrator's home", () => {
    it('welcomes the person and offers to add and manage tenants', () => {
        const html = home('system-administration', busy);

        expect(html).toContain('Welcome, marco');
        expect(html).toContain('This deployment runs 9 tenants.');
        expect(html).toContain('href="/tenants/new"');
        expect(html).toContain('>Add tenant<');
        expect(html).toContain('>Manage tenants<');
    });

    it('states the counts and how many tenants need attention', () => {
        const html = home('system-administration', busy);

        expect(html).toContain('System health');
        expect(html).toContain('>3<');
        expect(html).toContain('In service');
        expect(html).toContain('2 tenants need attention');
        expect(html).not.toContain('Everything is running');
    });

    it('says everything is running when nothing needs attention', () => {
        const html = home('system-administration', quiet);

        expect(html).toContain('Everything is running');
        expect(html).not.toContain('Needs attention');
    });

    it('leads a failed setup to its run and a suspended tenant to its screen', () => {
        const html = home('system-administration', busy);

        expect(html).toContain('Setup stopped after 4 of 7 steps.');
        expect(html).toContain('href="/tenants/runs/run-globex"');
        expect(html).toContain('Suspended: nobody in it can sign in.');
        expect(html).toContain('href="/tenants/initech"');
    });

    it('names each setup by its tenant, and says so when the setups cannot be read', () => {
        expect(home('system-administration', busy)).toContain(
            'Acme Corporation finished setting up',
        );
        expect(home('system-administration', quiet)).toContain(
            'The setup history could not be read.',
        );
    });

    it('lists the first tenants, with a way to all of them', () => {
        const html = home('system-administration', busy);

        expect(html).toContain('href="/tenants/acme"');
        expect(html).toContain('Showing 2 of 9 tenants');
        expect(html).toContain('>See all<');
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
});
