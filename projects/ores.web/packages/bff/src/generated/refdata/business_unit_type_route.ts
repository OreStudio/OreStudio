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
 * Template: ts_bff_route.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * How the BFF reaches the business_unit_type service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the members a write record carries and the subjects. The generic
 * factory builds the canonical envelopes from those, so the entity adds no
 * handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/business_unit_type_protocol';
import { MINTED_WRITE_DEFAULT, type EntityRouteDescriptor } from '../../entity-routes.js';

export const businessUnitTypeRoute: EntityRouteDescriptor = {
  collection: 'business_unit_types',
  key: 'id',
  keyField: 'code',
  rowField: 'business_unit_type',
  writeFields: ['id', 'coding_scheme_code', 'code', 'name', 'level', 'description'],
  writeDefaults: { id: MINTED_WRITE_DEFAULT, coding_scheme_code: '', code: '', name: '', level: 0, description: '' },
  intentFields: {
    reason: 'change_reason_code',
    commentary: 'change_commentary',
  },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_business_unit_types_request,
    save: subjects.put_business_unit_type_request,
  },
  rowsField: 'types',
};
