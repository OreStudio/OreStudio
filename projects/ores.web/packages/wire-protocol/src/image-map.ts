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

import type { AuthenticatedCaller } from './account-operations.js';
import { listImageSummaries } from './entities/image.js';
import { listRecords, recordResource, type RecordRow } from './records.js';

/**
 * Which image each record that shows a flag uses, for the whole tenant.
 *
 * This is the one place a screen learns a flag. It holds identifiers only;
 * the bytes are fetched once per image from the image's own address and kept
 * by the browser. A calendar uses its own image when it has one, else its
 * country's; a business centre uses its country's. `noFlag` is the
 * placeholder drawn for a code that has no image.
 */
export interface ImageMap {
    readonly currencies: Readonly<Record<string, string>>;
    readonly countries: Readonly<Record<string, string>>;
    readonly calendars: Readonly<Record<string, string>>;
    readonly businessCentres: Readonly<Record<string, string>>;
    readonly noFlag: string | null;
}

/** The image code of the placeholder flag. */
const NO_FLAG_CODE = 'xx';

async function rowsOf(caller: AuthenticatedCaller, key: string): Promise<readonly RecordRow[]> {
    const resource = recordResource(key);
    return resource === undefined ? [] : await listRecords(caller, resource);
}

function imageOf(row: RecordRow): string | undefined {
    const image = row['image_id'];
    return typeof image === 'string' && image !== '' ? image : undefined;
}

function mapOf(
    rows: readonly RecordRow[],
    key: string,
    image: (row: RecordRow) => string | undefined,
): Record<string, string> {
    const map: Record<string, string> = {};
    for (const row of rows) {
        const code = row[key];
        const id = image(row);
        if (typeof code === 'string' && id !== undefined) {
            map[code] = id;
        }
    }
    return map;
}

/** Reads every mapping the screens draw flags from. */
export async function readImageMap(caller: AuthenticatedCaller): Promise<ImageMap> {
    const [currencies, countries, calendars, centres, placeholder] = await Promise.all([
        rowsOf(caller, 'currencies'),
        rowsOf(caller, 'countries'),
        rowsOf(caller, 'calendars'),
        rowsOf(caller, 'business-centres'),
        listImageSummaries(caller, { offset: 0, limit: 50, search: NO_FLAG_CODE }),
    ]);
    const countryMap = mapOf(countries, 'alpha2_code', imageOf);
    const viaCountry = (field: string) => (row: RecordRow) => {
        const country = row[field];
        return typeof country === 'string' ? countryMap[country] : undefined;
    };
    return {
        currencies: mapOf(currencies, 'iso_code', imageOf),
        countries: countryMap,
        calendars: mapOf(
            calendars,
            'code',
            (row) => imageOf(row) ?? viaCountry('country_code')(row),
        ),
        businessCentres: mapOf(centres, 'code', viaCountry('country_alpha2_code')),
        noFlag: placeholder.images.find((image) => image.code === NO_FLAG_CODE)?.imageId ?? null,
    };
}
