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
import type { AuthenticatedCaller } from './account-operations.js';
import { readImageMap } from './image-map.js';

const ok = { outcome: 'ok', code: '', message: '' };

function caller(replies: Readonly<Record<string, unknown>>): AuthenticatedCaller {
    return {
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            return schema.parse(replies[subject] ?? { result: ok });
        },
    } as unknown as AuthenticatedCaller;
}

describe('readImageMap', () => {
    it('maps each code to its image, a calendar and a centre through their country, and finds the placeholder', async () => {
        const map = await readImageMap(
            caller({
                'refdata.v1.currencies.list': {
                    result: ok,
                    currencies: [
                        { iso_code: 'EUR', image_id: 'eur-flag', version: 1 },
                        { iso_code: 'XTS', image_id: null, version: 1 },
                    ],
                },
                'refdata.v1.countries.list': {
                    result: ok,
                    countries: [{ alpha2_code: 'GB', image_id: 'gb-flag', version: 1 }],
                },
                'refdata.v1.calendars.list': {
                    result: ok,
                    calendars: [
                        { code: 'UK', country_code: 'GB', image_id: null, version: 1 },
                        { code: 'OWN', country_code: 'GB', image_id: 'own-flag', version: 1 },
                    ],
                },
                'refdata.v1.business_centres.list': {
                    result: ok,
                    centres: [{ code: 'GBLO', country_alpha2_code: 'GB', version: 1 }],
                },
                'assets.v1.images.list': {
                    result: ok,
                    images: [
                        { id: 'xxs', code: 'xxs', description: 'Not it' },
                        { id: 'placeholder', code: 'xx', description: 'Flag of xx' },
                    ],
                    total: 2,
                },
            }),
        );
        expect(map).toEqual({
            currencies: { EUR: 'eur-flag' },
            countries: { GB: 'gb-flag' },
            calendars: { UK: 'gb-flag', OWN: 'own-flag' },
            businessCentres: { GBLO: 'gb-flag' },
            noFlag: 'placeholder',
        });
    });
});
