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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { SessionView } from '@ores/wire-protocol/browser';
import { AppRoutes } from '../AppRoutes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import type { BootstrapState } from '../session/BootstrapProvider.js';
import { MENU } from './areas.js';
import { AREA_CARDS, AREA_PAGE, CARDS, cardOf, screenAreaOf, type CardId } from './cards.js';
import { PATHS } from './paths.js';

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const READY: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    onboardingComplete: true,
    onboardingTenantComplete: true,
    accountId: '3f1e2d4c-0000-4000-8000-000000000001',
    sessionPresent: true,
    message: '',
    version: 'v0.0.25 (test)',
};

function session(mode: SessionView['mode']): SessionView {
    return {
        username: 'admin',
        email: 'admin@acme.test',
        accountId: '3f1e2d4c-0000-4000-8000-000000000001',
        tenantId: '3f1e2d4c-0000-4000-8000-000000000002',
        tenantName: 'Acme Corporation',
        mode,
        tenantBootstrapping: false,
        version: 'v0.0.25 (test)',
        party,
        availableParties: [party],
        accessLifetimeSeconds: 3600,
        passwordResetRequired: false,
        actingIn: null,
        database: {
            fingerprint: 'f',
            environment: 'development',
            commit: 'c',
            created: '2026-10-04 14:02',
        },
    } as SessionView;
}

function trailAt(path: string, mode: SessionView['mode'] = 'system-administration'): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [{ permissionCodes: ['*', 'installation::deployment:read'] }],
    });
    const html = renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <AppRoutes
                        gate={READY}
                        session={{ status: 'authenticated', session: session(mode) }}
                        journey={<p>First run journey</p>}
                        newTenantJourney={<p>New tenant journey</p>}
                        newPartyJourney={<p>New party journey</p>}
                        signUpJourney={<p>Registration door</p>}
                        journeyInProgress={false}
                        onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
                        onChooseParty={async () => undefined}
                        onSwitchParty={async () => undefined}
                        onSignOut={() => undefined}
                        onRetryBootstrap={() => undefined}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
    const nav = html.match(/<nav aria-label="Breadcrumb"[^>]*>(.*?)<\/nav>/s);
    if (nav === null) {
        return '(no breadcrumb)';
    }
    return (nav[1] ?? '')
        .replace(/<span aria-hidden="">›<\/span>/g, '›')
        .replace(/<[^>]+>/g, '')
        .replace(/›/g, ' › ')
        .replace(/\s+/g, ' ')
        .trim();
}

const en = (key: string): string => enFlat[key] ?? `(missing ${key})`;
const home = en('shell.menu.home');
const trail = (...labels: string[]): string => [home, ...labels].join(' › ');

/** The English name of an area, which is the middle of every trail inside it. */
function nameOfArea(path: string): string {
    const entry = MENU.find((item) => item.to === path);
    return entry === undefined ? '(no such area)' : en(entry.nameKey);
}

/**
 * Every screen the application draws, walked once: it is under the address of
 * its area, it has the one trail Home, its area, itself, and it is reached by
 * one address. The Menu structure page is the contract.
 */
describe('walking every screen', () => {
    const ids = Object.keys(CARDS) as CardId[];

    it('finds each card screen under the address of its area, by one address', () => {
        const addresses = ids.map((id) => cardOf(id).to);
        expect(new Set(addresses).size).toBe(addresses.length);

        const outside = ids.flatMap((id) => {
            const area = screenAreaOf(id);
            if (area === undefined) {
                return [];
            }
            const page = AREA_PAGE[area];
            const to = cardOf(id).to;
            return to === page || to.startsWith(`${page}/`)
                ? []
                : [`${id}: ${to} is not under ${page}`];
        });

        expect(outside).toEqual([]);
    });

    for (const id of ids) {
        const card = cardOf(id);
        const area = screenAreaOf(id);
        if (area === undefined) {
            continue;
        }
        const screen = en(card.screenTitleKey ?? card.titleKey);
        const expected =
            card.to === AREA_PAGE[area]
                ? trail(nameOfArea(AREA_PAGE[area]))
                : trail(nameOfArea(AREA_PAGE[area]), screen);

        it(`gives ${card.to} the trail ${expected}`, () => {
            expect(trailAt(card.to)).toBe(expected);
        });
    }

    for (const entry of MENU.filter((item) => item.to !== '/' && item.to !== '/tenants')) {
        it(`gives the page of ${entry.to} the trail Home, ${en(entry.nameKey)}`, () => {
            expect(trailAt(entry.to)).toBe(trail(en(entry.nameKey)));
        });
    }

    it('gives the pages of a person themselves the trail Home, then the page', () => {
        expect(trailAt('/access')).toBe(trail(en('access.mine.title')));
        expect(trailAt('/security')).toBe(trail(en('security.title')));
        expect(trailAt('/profile')).toBe(trail(en('profile.title.mine')));
        expect(trailAt('/where-i-work')).toBe(trail(en('membership.where.title')));
    });

    it('gives the reports a party runs the trail Home, then Reports', () => {
        expect(trailAt(PATHS.reporting)).toBe(trail(en('reporting.title')));
    });

    it('gives the screens that have no card the trail of their area', () => {
        expect(trailAt('/tenants/new')).toBe(trail(en('shell.menu.tenants'), en('tenants.add')));
        expect(trailAt('/development')).toBe(trail(en('shell.menu.development')));
    });

    it('lists every area card on an area page of the menu', () => {
        const menuPaths = MENU.map((item) => item.to);
        for (const area of Object.keys(AREA_CARDS) as (keyof typeof AREA_CARDS)[]) {
            expect(menuPaths, `${area} has a menu entry`).toContain(AREA_PAGE[area]);
        }
    });
});

/**
 * Every route the application declares is walked above or named here. A route
 * added without a trail test fails this case, so a new screen cannot skip the
 * walk.
 */
describe('the routes of the application', () => {
    const source = readFileSync(
        fileURLToPath(new URL('../AppRoutes.tsx', import.meta.url)),
        'utf8',
    );
    const declared = [
        ...source.matchAll(/<Route\s+(?:key=\{[^}]*\}\s+)?path=(\{`[^`]*`\}|\{[^}]*\}|"[^"]*")/g),
    ].map((match) => (match[1] ?? '').replace(/^\{|\}$/g, '').replace(/^"|"$/g, ''));

    const resolve = (route: string): string => {
        const template = route.replace(/^`|`$/g, '');
        return template
            .replace(/\$\{PATHS\.(\w+)\}/g, (_, name: string) => PATHS[name as keyof typeof PATHS])
            .replace(/^PATHS\.(\w+)$/, (_, name: string) => PATHS[name as keyof typeof PATHS]);
    };

    const walked = new Set<string>([
        ...Object.values(CARDS).map((card) => card.to),
        ...MENU.map((item) => item.to),
        '/access',
        '/security',
        '/profile',
        '/where-i-work',
        '/tenants/new',
        '/development',
        PATHS.reporting,
        // Opened from an oresmd URL, so no card leads to it.
        '/market-data/series',
    ]);

    /** Routes that are a record's page, a redirect or outside the signed-in shell. */
    const exempt = (path: string): boolean =>
        path.includes(':') ||
        path === '*' ||
        path === '/login' ||
        path === '/signup' ||
        path === '/' ||
        path === '/classifications' ||
        // The loop that redirects each earlier address to its new one.
        path === 'from';

    it('walks every route that is not a record page, a redirect or a door', () => {
        const unwalked = declared.map(resolve).filter((path) => !exempt(path) && !walked.has(path));

        expect(unwalked).toEqual([]);
    });

    it('declares routes at all, and resolves every address it can', () => {
        expect(declared.length).toBeGreaterThan(30);
        expect(declared.map(resolve).filter((path) => path.includes('${'))).toEqual([]);
    });
});
