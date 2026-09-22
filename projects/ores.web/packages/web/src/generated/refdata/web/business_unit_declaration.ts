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
 * Where the business_unit screens live and what they can do.
 *
 * The route, the API path and the key parameter are values the model carries, so
 * a screen never repeats them and two screens cannot disagree. The labels are
 * translation keys and live in the catalogue with the rest of the words.
 */
import { businessUnitMeta } from '../ui/business_unit_ui.js';
import type { EntityDescriptor } from '../../../entity/descriptor.js';

export const businessUnitDescriptor: EntityDescriptor = {
    component: 'refdata',
    entity: 'business_unit',
    meta: businessUnitMeta,
    routeSegment: 'business-unit',
    apiBase: '/api/business_units',
    keyFields: [
        'unit_code',
    ],
    capabilities: {
        create: true,
        edit: true,
        remove: true,
        history: true,
    },
    searchFields: [
        'unit_code',
        'unit_name',
        'business_centre_code',
        'status',
    ],
    writeFields: [
        'id',
        'unit_name',
        'parent_business_unit_id',
        'unit_code',
        'business_centre_code',
        'unit_type_id',
        'status',
    ],
};
