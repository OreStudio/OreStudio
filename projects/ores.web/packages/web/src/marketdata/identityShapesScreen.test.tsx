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
import { Route } from 'react-router';
import type { RecordRow } from '../api/client.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { accessOf, renderRecords, resourceView } from '../refdata/testSupport.js';
import { IdentityShapePage, IdentityShapesPage, codeOf } from './IdentityShapesPage.js';

const SHAPES = 'series-identity-shapes';
const ENTRIES = 'series-identity-shape-entries';

const REGISTRY = [
    resourceView(SHAPES, ['code'], {
        search: true,
        sortable: ['code', 'asset_class', 'series_subclass', 'index_family'],
    }),
    resourceView(ENTRIES, ['id']),
];

const IBOR: RecordRow = {
    version: 2,
    code: 'ir.index_fixing.ibor',
    asset_class: 'ir',
    series_subclass: 'index_fixing',
    index_family: 'ibor',
    description: 'A fixing of an interbank offered rate, named by its index.',
};

const SPOT: RecordRow = {
    version: 1,
    code: 'fx.spot',
    asset_class: 'fx',
    series_subclass: 'spot',
    index_family: '',
    description: 'The spot price of a currency pair.',
};

function entry(field: string, requirement: string): RecordRow {
    return {
        version: 1,
        id: `00000000-0000-4000-8000-00000000000${field.length}`,
        shape_code: IBOR['code'],
        field,
        requirement,
        description: `Why ${field}.`,
    };
}

function render(path: string, permissions: string[] = ['*']): string {
    return renderRecords(
        path,
        REGISTRY,
        <>
            <Route path="/market-data/identity-shapes" element={<IdentityShapesPage />} />
            <Route path="/market-data/identity-shapes/:code" element={<IdentityShapePage />} />
        </>,
        (client) => {
            client.setQueryData(['my-access'], accessOf(permissions));
            client.setQueryData(pageKey({ key: SHAPES }, FIRST_PAGE), {
                rows: [SPOT, IBOR],
                total: 7,
            });
            client.setQueryData(['records', SHAPES, 'key', IBOR['code']], IBOR);
            client.setQueryData(
                ['records', ENTRIES, IBOR['code']],
                [entry('unit_ccy', 'forbidden'), entry('ccy', 'required')],
            );
        },
    );
}

describe('the identity shape list', () => {
    it('draws one page of shapes with the server total, each with what it is for', () => {
        const html = render('/market-data/identity-shapes');
        expect(html).toContain('1–2 of 7');
        expect(html).toContain('fx.spot');
        expect(html).toContain('The spot price of a currency pair.');
    });

    it('offers Add only to a person who may write shapes', () => {
        expect(render('/market-data/identity-shapes')).toContain('Add a shape');
        expect(
            render('/market-data/identity-shapes', ['marketdata::series_identity_shapes:read']),
        ).not.toContain('Add a shape');
    });
});

describe('the identity shape page', () => {
    it('states what the shape is for, and offers its entries and history', () => {
        const html = render('/market-data/identity-shapes/ir.index_fixing.ibor');
        expect(html).toContain('A fixing of an interbank offered rate, named by its index.');
        expect(html).toContain('Entries');
        expect(html).toContain('History');
    });

    it('lists the fields the shape requires and forbids, in field order', () => {
        const html = render('/market-data/identity-shapes/ir.index_fixing.ibor?tab=entries');
        expect(html.indexOf('ccy')).toBeLessThan(html.indexOf('unit_ccy'));
        expect(html).toContain('Required');
        expect(html).toContain('Forbidden');
        expect(html).toContain('Why unit_ccy.');
    });
});

describe('the code of a shape', () => {
    it('joins the parts of its family, and leaves out an empty index family', () => {
        expect(codeOf(IBOR)).toBe('ir.index_fixing.ibor');
        expect(codeOf(SPOT)).toBe('fx.spot');
    });
});
