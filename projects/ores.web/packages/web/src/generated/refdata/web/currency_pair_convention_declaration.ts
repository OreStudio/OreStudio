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
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_web_declaration.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * Where the currency_pair_convention screens live and what they can do.
 *
 * The route, the API path and the key parameter are values the model carries, so
 * a screen never repeats them and two screens cannot disagree. The labels are
 * translation keys and live in the catalogue with the rest of the words.
 */
import { currencyPairConventionMeta } from '../ui/currency_pair_convention_ui.js';
import type { EntityDescriptor } from '../../../entity/descriptor.js';

export const currencyPairConventionDescriptor: EntityDescriptor = {
    component: 'refdata',
    entity: 'currency_pair_convention',
    meta: currencyPairConventionMeta,
    routeSegment: 'currency-pair-convention',
    apiBase: '/api/conventions',
    keyFields: [
        'pair_code',
    ],
    capabilities: {
        create: true,
        edit: true,
        remove: true,
        history: true,
    },
    searchFields: [
        'pair_code',
        'pip_factor',
        'tick_size',
        'decimal_places',
        'business_day_convention',
        'spot_relative',
        'end_of_month',
    ],
    writeFields: [
        'pair_code',
        'pip_factor',
        'tick_size',
        'decimal_places',
        'business_day_convention',
        'spot_relative',
        'end_of_month',
    ],
};
