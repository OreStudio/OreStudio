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
import type { SessionMode } from '@ores/wire-protocol/browser';
import { describe, expect, it } from 'vitest';
import type { Shape } from '../api/identityShapes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { IdentityShapesPage, familiesOf } from './IdentityShapesPage.js';

/**
 * The identity shapes screen.
 *
 * What is checked is what the screen promises: the rows are grouped by family,
 * each says whether it is the shared catalogue's or the tenant's own, and a
 * change is offered only to a person who holds the permission and only on a row
 * that person's tenant owns.
 */

function shape(overrides: Partial<Shape> = {}): Shape {
    return {
        asset_class: 'ir',
        series_subclass: 'index_fixing',
        index_family: 'ibor',
        field: 'ccy',
        requirement: 'required',
        description: 'A fixing is a rate of one currency.',
        version: 1,
        system: true,
        modified_by: 'system',
        recorded_at: '2026-11-17 09:00:00Z',
        change_reason_code: 'system.initial_load',
        change_commentary: '',
        ...overrides,
    };
}

const ROWS: readonly Shape[] = [
    shape({ field: 'unit_ccy', requirement: 'forbidden' }),
    shape({ asset_class: 'fx', series_subclass: 'spot', index_family: '', field: 'ccy' }),
    shape(),
    shape({ field: 'tenor', system: false, requirement: 'forbidden' }),
];

function render(permissions: readonly string[], mode: SessionMode): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: 'r',
                name: 'Market data administrator',
                description: '',
                permissionCodes: [...permissions],
                givenBy: 'system',
                givenAt: '2026-11-17 09:00:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    });
    client.setQueryData(['series-identity-shapes'], ROWS);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <IdentityShapesPage mode={mode} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const WRITE = 'marketdata::series_identity_shapes:write';
const DELETE = 'marketdata::series_identity_shapes:delete';

function count(html: string, text: string): number {
    return html.split(text).length - 1;
}

describe('identity shapes screen', () => {
    it('groups the rows by family, and the fields of a family by name', () => {
        expect(
            familiesOf(ROWS).map((family) => [family.name, family.rows.map((row) => row.field)]),
        ).toEqual([
            ['fx / spot', ['ccy']],
            ['ir / index_fixing / ibor', ['ccy', 'tenor', 'unit_ccy']],
        ]);
    });

    it("says whether each row is the shared catalogue's or the tenant's own", () => {
        const html = render([], 'tenant-administration');
        expect(count(html, '>Shared<')).toBe(3);
        expect(count(html, '>Own<')).toBe(1);
        expect(html).toContain('>Required<');
        expect(html).toContain('>Forbidden<');
    });

    it('offers no change to a person without the permissions', () => {
        const html = render([], 'tenant-administration');
        expect(html).not.toContain('Add a row');
        expect(html).not.toContain('>Change<');
        expect(html).not.toContain('>Remove<');
        expect(count(html, '>History<')).toBe(4);
    });

    it("offers a tenant a change of its own rows only, not of the shared catalogue's", () => {
        const html = render([WRITE, DELETE], 'tenant-administration');
        expect(html).toContain('Add a row');
        expect(count(html, '>Change<')).toBe(1);
        expect(count(html, '>Remove<')).toBe(1);
    });

    it('offers the system tenant a change of every row', () => {
        const html = render([WRITE, DELETE], 'system-administration');
        expect(count(html, '>Change<')).toBe(4);
        expect(count(html, '>Remove<')).toBe(4);
    });
});
