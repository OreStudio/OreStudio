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
import type { AppVersionPageView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { APP_VERSION_QUERY_KEY, GridAppVersionPage, parseNote } from './GridAppVersionPage.js';

/**
 * The app version page, as a reader sees it.
 *
 * What is checked is what the page promises: the build, the note drawn as the
 * stored text marks it, the packages with a warning when there are none, the other
 * versions of the app, and the edit control for the super administrator alone.
 */

const VERSION = '33333333-3333-4333-8333-333333333333';
const OLDER = '44444444-4444-4444-8444-444444444444';
const APP = '22222222-2222-4222-8222-222222222222';

function view(overrides: Partial<AppVersionPageView> = {}): AppVersionPageView {
    return {
        version: {
            id: VERSION,
            app_id: APP,
            app_name: 'ORE',
            wrapper_version: '1.0.0',
            engine_version: '1.8.17.0',
            min_ram_mb: 512,
            release_note:
                '## Changes\n- Fixes the day count.\n- Adds the swap leg report.\n\nKnown issue: none.',
            version: 3,
            modified_by: 'super_admin',
            recorded_at:
                '2026-10-23 08:43:09.000000Z' as AppVersionPageView['version']['recorded_at'],
        },
        packages: [
            {
                platform_id: 'p1',
                code: 'x64-linux',
                display_name: 'Linux x64',
                is_active: true,
                package_uri: '/storage/ORE-1.8.17.0-x64-linux.tar.gz',
                sha256: 'abc123',
            },
            {
                platform_id: 'p2',
                code: 'x64-windows',
                display_name: 'Windows x64',
                is_active: false,
                package_uri: '/storage/ORE-1.8.17.0-x64-windows.tar.gz',
                sha256: 'def456',
            },
        ],
        versions: [
            {
                id: VERSION,
                engine_version: '1.8.17.0',
                wrapper_version: '1.0.0',
                recorded_at:
                    '2026-10-23 08:43:09.000000Z' as AppVersionPageView['version']['recorded_at'],
            },
            {
                id: OLDER,
                engine_version: '1.8.16.0',
                wrapper_version: '1.0.0',
                recorded_at:
                    '2026-10-22 08:00:00.000000Z' as AppVersionPageView['version']['recorded_at'],
            },
        ],
        may_write: false,
        ...overrides,
    };
}

function render(page: AppVersionPageView): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData([APP_VERSION_QUERY_KEY, VERSION], page);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[`/operations/grid/app-versions/${VERSION}`]}>
                    <Routes>
                        <Route
                            path="/operations/grid/app-versions/:id"
                            element={<GridAppVersionPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the app version page', () => {
    it('states the build under the model headers', () => {
        const html = render(view());

        for (const header of ['ORE', 'Engine Version', 'Wrapper Version', 'Min RAM MB']) {
            expect(html).toContain(header);
        }
        expect(html).toContain('1.8.17.0');
        expect(html).toContain('512 MB');
    });

    it('draws the release note with its headings and items', () => {
        const html = render(view());

        expect(html).toContain('>Changes<');
        expect(html).toContain('<li>Fixes the day count.</li>');
        expect(html).toContain('<li>Adds the swap leg report.</li>');
        expect(html).toContain('Known issue: none.');
    });

    it('says when a release has no note and who writes it', () => {
        const base = view();
        const html = render(view({ version: { ...base.version, release_note: '' } }));

        expect(html).toContain('This release has no note. A super administrator writes it.');
    });

    it('lists the packages with their platform, state and digest', () => {
        const html = render(view());

        expect(html).toContain('x64-linux');
        expect(html).toContain('>active<');
        expect(html).toContain('>inactive<');
        expect(html).toContain('abc123');
        expect(html).toContain('2 packages');
    });

    it('says a version with no package never runs its jobs', () => {
        const html = render(view({ packages: [] }));

        expect(html).toContain('This version has no package, so its jobs never run');
    });

    it('lists the other versions of the app and links them', () => {
        const html = render(view());

        expect(html).toContain(`href="/operations/grid/app-versions/${OLDER}"`);
        expect(html).toContain('(this version)');
        expect(html).toContain('Versions of ORE');
    });

    it('offers the edit of the note to the super administrator alone', () => {
        const writer = render(view({ may_write: true }));
        const reader = render(view({ may_write: false }));

        expect(writer).toContain('>Edit<');
        expect(reader).not.toContain('>Edit<');
        expect(reader).toContain('Release Note');
    });
});

describe('the release note marks', () => {
    it('reads a heading, an item and a paragraph, and drops blank lines', () => {
        expect(parseNote('## Changes\n- One\n\n- Two\nA line')).toEqual([
            { kind: 'heading', text: 'Changes' },
            { kind: 'item', text: 'One' },
            { kind: 'item', text: 'Two' },
            { kind: 'paragraph', text: 'A line' },
        ]);
    });

    it('reads an empty note as no block', () => {
        expect(parseNote('')).toEqual([]);
        expect(parseNote('\n \n')).toEqual([]);
    });
});
