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
 * How the BFF reaches the instrument_code service.
 *
 * The descriptor holds values and no functions: the collection, the key, the
 * members a write record carries, the optional members the requests declare
 * and the subjects. The generic factory builds the canonical envelopes from
 * those, so the entity adds no handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/instrument_code_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const instrumentCodeRoute: EntityRouteDescriptor = {
  component: 'refdata',
  entity: 'instrument_code',
  collection: 'codes',
  keyFields: ['code'],
  writeFields: ['code', 'name', 'description', 'asset_class', 'ore_trade_type', 'display_order', 'curve_role'],
  writeDefaults: { code: '', name: '', description: '', asset_class: null, ore_trade_type: null, display_order: 0, curve_role: '' },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_instrument_codes_request,
    get: subjects.get_instrument_code_request,
    save: subjects.put_instrument_code_request,
    remove: subjects.delete_instrument_code_request,
    history: subjects.list_instrument_code_versions_request,
  },
  rowsField: 'instruments',
  getRowField: 'instrument_code',
  historyRowsField: 'versions',
};
