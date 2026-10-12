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

import type { OresmdSchema } from '@ores/contracts';

const spec = (
    name: string,
    role: 'identity' | 'coordinate',
    may_be_none = false,
    kind = 'text',
    codes: string[] = [],
) => ({ name, role, may_be_none, kind, codes });

/** A small schema with the shapes the editor draws: a path, optional and coordinate fields, a closed set, a fixing family. */
export const SCHEMA: OresmdSchema = {
    assets: ['inflation', 'ir'],
    instruments: [
        {
            code: 'seasonality',
            ore_name: 'SEASONALITY',
            title: '{subject} inflation seasonality',
            asset: 'inflation',
            subject: 'index',
            fields: [
                spec('seasonality_type', 'identity'),
                spec('index', 'identity'),
                spec('month', 'coordinate'),
            ],
        },
        {
            code: 'capfloor',
            ore_name: 'CAPFLOOR',
            title: '{subject} cap/floor',
            asset: 'ir',
            subject: 'index_name',
            fields: [
                spec('index_name', 'identity'),
                spec('cap_floor', 'identity', true, 'code', ['Cap', 'Floor']),
                spec('term', 'coordinate', false, 'term'),
            ],
        },
    ],
    quotes: ['rate', 'price'],
    quote_titles: ['rate', 'price'],
    indexes: [
        {
            code: 'ibor',
            asset: 'ir',
            subject: 'ccy',
            fields: [
                { name: 'name', optional: false },
                { name: 'tenor', optional: true },
            ],
        },
    ],
};
